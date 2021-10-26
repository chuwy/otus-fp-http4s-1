package me.chuwy.otusfp

import cats.effect.{IOApp, IO, Resource}

import cats.implicits._

object Main extends IOApp.Simple {
  import Transformers._

  def run: IO[Unit] = {
    Restful.builder.void.use { _ =>
      IO.never
    }
  }
}
