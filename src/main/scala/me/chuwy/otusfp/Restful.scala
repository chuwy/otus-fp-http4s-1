package me.chuwy.otusfp

import cats.data.Kleisli
import cats.effect._

import org.http4s.{Request, Http, HttpApp, Response, HttpRoutes}
import org.http4s.blaze.server.BlazeServerBuilder
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.Router


object Restful {
  val serviceOne: HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "hello" / name =>
        IO.println("Logging a request") *> Ok(s"hello, $name")
    }

  val serviceTwo: HttpRoutes[IO] =
    HttpRoutes.of {
      case GET -> Root / "hello" / name =>
        IO.println("Logging a request") *> Ok(s"hello, $name")
    }

  def hello(name: String): IO[Response[IO]] = {
    Ok(s"hello, $name")
  }

  val router = Router("/" -> serviceOne, "/api" -> serviceTwo)

  val builder = BlazeServerBuilder[IO]
    .bindHttp(port = 8080, host = "localhost")
    .withHttpApp(router.orNotFound)
    .resource

}
