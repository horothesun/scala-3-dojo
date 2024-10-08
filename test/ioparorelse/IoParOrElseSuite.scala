package ioparorelse

import IoParOrElseSuite.*
import cats.effect.IO
import cats.effect.kernel.Outcome.*
import cats.effect.testkit.TestControl
import ioparorelse.IoParOrElse.*
import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Gen
import org.scalacheck.effect.PropF
import scala.concurrent.duration.*

class IoParOrElseSuite extends CatsEffectSuite with ScalaCheckEffectSuite:

  test("[✅ in Xs] parOrElse [✅ or ❌ in Ys] will ✅ w/ primary value in Xs"):
    val primaryResult = "1️⃣"
    PropF.forAllF(succeedingIoAndDurationGen(primaryResult), ioGen(result = "2️⃣")) { case ((p, tp), s) =>
      val program = p.parOrElse(s).timed
      TestControl.executeEmbed(program).assertEquals((tp, primaryResult))
    }

  test("[❌ in Xs] parOrElse [✅ in Ys] will ✅ w/ secondary value in max(X,Y)s"):
    val secondaryResult = "2️⃣"
    PropF.forAllF(failingIoAndDurationGen[String], succeedingIoAndDurationGen(secondaryResult)) {
      case ((p, tp), (s, ts)) =>
        val program = p.parOrElse(s).timed
        val expectedDuration = Math.max(tp.toSeconds, ts.toSeconds).seconds
        TestControl.executeEmbed(program).assertEquals((expectedDuration, secondaryResult))
    }

  test("[❌ in 5s] parOrElse [✅ in 5s] will ✅ w/ secondary value in 5s"):
    val duration = 5.seconds
    val primary = IO.sleep(duration).as("1️⃣").flatTap(_ => boom)
    val secondary = IO.sleep(duration).as("2️⃣")
    val program = primary.parOrElse(secondary)
    TestControl.execute(program) flatMap { c =>
      for {
        _ <- c.results.assertEquals(None)
        _ <- c.tick
        t <- c.nextInterval
        _ <- IO(assertEquals(t, duration))
        _ <- c.advanceAndTick(t)
        _ <- c.results.assertEquals(Some(Succeeded("2️⃣")))
      } yield ()
    }

  test("[❌ in 3s] parOrElse([❌ in 1s], customError) will ❌ w/ customError in 3s"):
    val primary = IO.sleep(3.seconds).as("1️⃣").flatTap(_ => boom)
    val secondary = IO.sleep(1.seconds).as("2️⃣").flatTap(_ => boom)
    val customError = Throwable("Custom both-KO error!")
    val program = primary.parOrElse(secondary, customError)
    TestControl.execute(program).flatMap { c =>
      for {
        _ <- c.results.assertEquals(None)
        _ <- c.tick
        t1 <- c.nextInterval
        _ <- IO(assertEquals(t1, 1.seconds))
        _ <- c.advanceAndTick(t1)
        _ <- c.results.assertEquals(None)
        t2 <- c.nextInterval
        _ <- IO(assertEquals(t2, 2.seconds))
        _ <- IO(assertEquals(t1 + t2, 3.seconds))
        _ <- c.advanceAndTick(t2)
        _ <- c.results.assertEquals(Some(Errored(customError)))
      } yield ()
    }

  test("[❌ in 1s] parOrElse([❌ in 3s], customError) will ❌ w/ customError in 3s"):
    val primary = IO.sleep(1.seconds).as("1️⃣").flatTap(_ => boom)
    val secondary = IO.sleep(3.seconds).as("2️⃣").flatTap(_ => boom)
    val customError = Throwable("Custom both-KO error!")
    val program = primary.parOrElse(secondary, customError)
    TestControl.execute(program).flatMap { c =>
      for {
        _ <- c.results.assertEquals(None)
        _ <- c.tick
        t1 <- c.nextInterval
        _ <- IO(assertEquals(t1, 1.seconds))
        _ <- c.advanceAndTick(t1)
        _ <- c.results.assertEquals(None)
        t2 <- c.nextInterval
        _ <- IO(assertEquals(t2, 2.seconds))
        _ <- IO(assertEquals(t1 + t2, 3.seconds))
        _ <- c.advanceAndTick(t2)
        _ <- c.results.assertEquals(Some(Errored(customError)))
      } yield ()
    }

  test("[❌ in 5s] parOrElse([❌ in 5s], customError) will ❌ w/ customError in 5s"):
    val duration = 5.seconds
    val primary = IO.sleep(duration).as("1️⃣").flatTap(_ => boom)
    val secondary = IO.sleep(duration).as("2️⃣").flatTap(_ => boom)
    val customError = Throwable("Custom both-KO error!")
    val program = primary.parOrElse(secondary, customError)
    TestControl.execute(program).flatMap { c =>
      for {
        _ <- c.results.assertEquals(None)
        _ <- c.tick
        t <- c.nextInterval
        _ <- IO(assertEquals(t, duration))
        _ <- c.advanceAndTick(t)
        _ <- c.results.assertEquals(Some(Errored(customError)))
      } yield ()
    }

object IoParOrElseSuite:

  case object TestException extends RuntimeException

  def boom[A]: IO[A] = IO.raiseError[A](Throwable("💥 BOOM!"))

  def ioGen[R](result: R): Gen[IO[R]] = ioAndDurationGen(result).map(_._1)
  def succeedingIoGen[R](result: R): Gen[IO[R]] = succeedingIoAndDurationGen(result).map(_._1)
  def failingIoGen[R]: Gen[IO[R]] = failingIoAndDurationGen[R].map(_._1)

  def ioAndDurationGen[R](result: R): Gen[(IO[R], FiniteDuration)] =
    Gen.oneOf(succeedingIoAndDurationGen(result), failingIoAndDurationGen)
  def succeedingIoAndDurationGen[R](result: R): Gen[(IO[R], FiniteDuration)] =
    posDurationGen.map(t => (IO.sleep(t).as(result), t))
  def failingIoAndDurationGen[R]: Gen[(IO[R], FiniteDuration)] = posDurationGen.map(t => (boom.delayBy(t), t))
  def posDurationGen: Gen[FiniteDuration] = Gen.posNum[Int].map(_.seconds)
