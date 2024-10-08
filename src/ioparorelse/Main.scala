package ioparorelse

import cats.effect.*
import ioparorelse.IoParOrElse.*
import scala.concurrent.duration.*

object Main extends IOApp.Simple:

  def boom[A]: IO[A] = IO.raiseError[A](Throwable("💥 BOOM!"))

  def run: IO[Unit] =
    IO.sleep(2.seconds)
      .as("1️⃣")
      .flatTap(_ => boom)
      .parOrElse(secondary = IO.sleep(1.seconds).as("2️⃣"))
      .timed
      .flatMap((t, s) => IO.println(s" $s [${t.toMillis}ms]"))
