package me.chuwy.otusfp

import cats.{Functor, Monad}
import cats.data.{OptionT, Kleisli, Reader}
import cats.implicits._

import cats.effect._

import org.http4s.{Request, Header, Http, AuthedRoutes, HttpApp, Response, HttpRoutes, Status}
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.{AuthMiddleware, HttpMiddleware, Router, Middleware}
import org.typelevel.ci.CIString

object Restful {

  case class Config(fileName: String, maxConcurrency: Int)

  val getDbName: Reader[Config, String] = Reader { (t: Config) => t.fileName ++ ".db" }
  val multiplyConcurrency: Reader[Config, Int] = Reader { (t: Config) => t.maxConcurrency * 8 }

  val result: Reader[Config, String] = for {
    dbName <- getDbName
    realConcurrency <- multiplyConcurrency
  } yield s"$dbName:$realConcurrency"

  type RIO[Env, A] = Kleisli[IO, Env, A]

  val t: Kleisli[IO, Config, Unit] =
    Kleisli { (c: Config) => IO(println(s"Reading ${c.fileName}")) }
  val t2: Kleisli[IO, Config, Int] =
    Kleisli { (c: Config) => IO.pure(c.maxConcurrency) }


  val first: Kleisli[IO, Config, String] =
    Kleisli { (c: Config) => IO.pure(c.fileName) }
  val second: Kleisli[IO, String, Unit] =
    Kleisli { (s: String) => IO.println(s) }
  val third: Kleisli[IO, Unit, Unit] =
    Kleisli { (_: Unit) => IO.println("Launched missles") }

  val aa = first.andThen(second).andThen(third)
  val r = t.run(Config("my-app", 8))
  val a = result.run(Config("my-app", 4))

  type Database[F[_]] = Ref[F, List[String]]

  def serviceOne(db: Database[IO]): HttpRoutes[IO] =
    HttpRoutes.of {
      case req @ GET -> Root / "hello" / name =>
        req.headers.get(CIString("Otus-Authrozied")) match {
          case Some(nel) if nel.head.value == "true" =>
            Ok(s"hello, $name")
          case None =>
            Forbidden("You shall not pass")
        }

      case PUT -> Root / "register" / name =>
        db.update(list => name :: list).flatMap { _ =>
          Ok("User has been registered")
        }
    }

  def authRoutes(db: Database[IO]): AuthedRoutes[User, IO] =
    AuthedRoutes.of {
      case GET -> Root / "hello" / path as user =>
        if (user.name != "anon") Ok(s"hello, ${user.name} from $path")
        else Forbidden("You're anonymous")
      case PUT -> Root / "register" / name as _ =>
        db.update(list => name :: list).flatMap { _ =>
          Ok("User has been registered")
        }
    }

  def hello(name: String): IO[Response[IO]] = {
    Ok(s"hello, $name")
  }

  import org.http4s.Header._

  def myMiddle[F[_]: Functor](route: HttpRoutes[F]): HttpRoutes[F] = {
    val header: Header.ToRaw = "Otus" -> "http4s"
    Kleisli { (req: Request[F]) =>
      val result = route(req)
      result.map {
        case Status.Successful(res) =>
          res.putHeaders(header)
        case res =>
          res
      }
    }
  }

  def authMiddlewareHeaders[F[_]: Monad](db: Database[F])(route: HttpRoutes[F]): HttpRoutes[F] = {
    val header: Header.ToRaw = "Otus-Authorized" -> "true"
    Kleisli { (req: Request[F]) =>
      req.uri.path.segments.toList match {
        case _ :: username :: _ =>
          OptionT.liftF(db.get).flatMap { users =>
            if (users.contains(username.toString)) route(req.putHeaders(header))
            else route(req)
          }
        case _ =>
          route(req)
      }
    }
  }

  case class User(name: String)

  def authUser(db: Database[IO]): Kleisli[OptionT[IO, *], Request[IO], User] =
    Kleisli { (req: Request[IO]) =>
      req.headers.get(CIString("username")) match {
        case Some(nel) =>
          OptionT.liftF(db.get).map { users =>
            val username = nel.head.value
            if (users.contains(username)) User(username)
            else User("unregistered")
          }
        case None =>
          OptionT.pure(User("anon"))
      }
    }



  def buildAuthMiddleware(db: Database[IO]): AuthMiddleware[IO, User] =
    AuthMiddleware(authUser(db))

  def buildRoutes(db: Database[IO]): HttpRoutes[IO] =
    buildAuthMiddleware(db)(authRoutes(db))

  def router(db: Database[IO]) = Router("/" -> buildAuthMiddleware(db)(authRoutes(db)))

  val builder = for {
    db <- Resource.eval[IO, Database[IO]](Ref.of(List.empty))
    builder <- BlazeServerBuilder[IO]
      .bindHttp(port = 8080, host = "localhost")
      .withHttpApp(router(db).orNotFound)
      .resource
  } yield builder

}
