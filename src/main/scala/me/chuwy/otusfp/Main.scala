package me.chuwy.otusfp

import cats.effect.{IOApp, IO, Resource}

import scala.concurrent.duration._

import cats.implicits._

object Main extends IOApp.Simple {
  def run: IO[Unit] = {
    for {
      result <- Final.pureRequest
    } yield ()
  }
}
