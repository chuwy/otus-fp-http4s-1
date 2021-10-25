package me.chuwy.otusfp

import cats.Applicative
import cats.implicits._

import cats.effect.IO
import cats.data.{StateT, OptionT, ReaderT, State, Reader}

object Transformers {
  def getUsername: IO[Option[String]] = IO.pure(Some("Bob"))
  def getId(name: String): IO[Option[Int]] = IO.pure(Some(42))
  def getPermissions(id: Int): IO[Option[String]] = IO.pure(None)

  def program: IO[Option[String]] =
    ???

  case class RealWorld(events: List[String]) {
    def addEvent(event: String): RealWorld =
      RealWorld(event :: events)
  }
  type WorldChange[A] = State[RealWorld, A]

}
