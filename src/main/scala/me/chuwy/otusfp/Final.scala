package me.chuwy.otusfp

import cats.effect.{IO, Resource, Ref}

import io.circe.Json

import org.http4s.circe._

import org.http4s.Request
import org.http4s.EntityDecoder
import org.http4s.implicits._
import org.http4s.blaze.client.BlazeClientBuilder

import Jawson._

object Final {
  val builder = BlazeClientBuilder[IO].resource

  implicit def userEntityDecoder: EntityDecoder[IO, User] =
    jsonOf[IO, User]


  val request = Request[IO]().withUri(uri"http://localhost:8080/hello/world")

  val result = for {
    client <- builder
    response <- client.run(request)
  } yield response

  val response = result.use { res =>
    res.as[User].flatMap(IO.println)
  }

  val pureRequest = Ref.of[IO, List[String]](List.empty).flatMap { (db: Restful.Database[IO]) =>
    val routes = Restful.buildRoutes(db)
    routes.run(request).value
  }
}
