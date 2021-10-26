package me.chuwy.otusfp

import cats.{Applicative, Monad}
import cats.implicits._

import cats.effect.IO
import cats.data.{OptionT, ReaderT, State, StateT, EitherT}


object Transformers {
  def getUsername: IO[Option[String]] = IO.pure(Some("Bob"))
  def getId(name: String): IO[Option[Int]] = IO.pure(Some(42))
  def getPermissions(id: Int): IO[Option[String]] = IO.pure(None)


  type Reader[E, A] = E => A


  case class RealWorld(events: List[String]) {
    def addEvent(event: String): RealWorld =
      RealWorld(event :: events)
  }
  type WorldChange[A] = State[RealWorld, A]
  type Life[A] = Either[Catastrophe, A]
  type WorldWithCatastrophe[A] = StateT[Life, RealWorld, A]

  def liftState[F[_]: Monad, S, A](state: State[S, A]): StateT[F, S, A] =
    StateT.fromState(state.map(a => Monad[F].pure(a)))

  case class Catastrophe(msg: String)

  def something: Life[Unit] = Left(Catastrophe("pandemic"))

  def born: WorldChange[Unit] =
    State.modify(_.addEvent("birth"))
  def goSchool: WorldChange[Unit] =
    State.modify(_.addEvent("school"))
  def learnCode: WorldChange[Unit] =
    State.modify(_.addEvent("learn FP"))

  val life: WorldWithCatastrophe[Unit] = for {
    _ <- liftState[Life, RealWorld, Unit](born)
    _ <- liftState[Life, RealWorld, Unit](goSchool)
    _ <- StateT.liftF(something)
    _ <- liftState[Life, RealWorld, Unit](learnCode)
  } yield ()

  val action: IO[Int] = ???

  val a: EitherT[IO, Catastrophe, Int] = EitherT.liftF(action)

  type Env = String
  val readerTexample: ReaderT[IO, Env, Int] = ???
  val readerTexample1: ReaderT[IO, Env, Int] = ???
  val readerTexample2: ReaderT[IO, Env, Int] = ???
  val readerTexample3: ReaderT[IO, Env, Int] = ???

  val result = for {
    a <- readerTexample1
    b <- readerTexample2
    c <- readerTexample3
  } yield a + b + c

}
