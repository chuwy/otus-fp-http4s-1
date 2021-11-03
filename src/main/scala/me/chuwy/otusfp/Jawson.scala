package me.chuwy.otusfp

import cats.implicits._

import io.circe.literal._
import io.circe.syntax._
import io.circe.parser.parse

import io.circe.{Encoder, Json, Decoder, DecodingFailure}
import io.circe.generic.semiauto._

object Jawson {
  // Encoder = A => Json
  // Decoder = Json => Either[Error, A]

  val example = json"""{"name": "Bob", "email": null}"""

  case class User(name: String, email: Option[String])

  case class Permission(user: User, id: Int)

  implicit val decoderUser: Decoder[User] =
    deriveDecoder[User]
  implicit val decoderPermission: Decoder[Permission] =
    deriveDecoder[Permission]

  val parsedResult = parse("").flatMap(_.as[User])



  //  implicit val decoderUser: Decoder[User] =
//    Decoder.instance { cur =>
//      for {
//        hashMap <- cur.as[Map[String, Json]]
//        name <- hashMap.get("name").toRight(DecodingFailure("No 'name' key", cur.history)).flatMap(_.as[String])
//        email <- hashMap.get("email").toRight(DecodingFailure("No 'email' key", cur.history)).flatMap(_.as[Option[String]])
//      } yield User(name, email)
//    }

  val decodingResult = example.as[User]


}
