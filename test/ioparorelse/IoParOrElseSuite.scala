package ioparorelse

import ioparorelse.IoParOrElse.*
import IoParOrElseSuite.*
import cats.effect.IO
import cats.effect.kernel.Outcome.*
import cats.effect.testkit.TestControl
import munit.{CatsEffectSuite, ScalaCheckSuite}
import org.scalacheck.Gen
import org.scalacheck.Prop.*
import scala.concurrent.duration.*

class IoParOrElseSuite extends CatsEffectSuite with ScalaCheckSuite:

  property("[✅] parOrElse [✅ or ❌] will ✅ w/ primary value") {
    forAll(succeedingIoGen(result = "1️⃣"), ioGen(result = "2️⃣")) { case (primary, secondary) =>
      val program = primary.parOrElse(secondary)
      assertEquals(TestControl.executeEmbed(program).unsafeRunSync(), "1️⃣")
    }
  }

  property("[✅ in Xs] parOrElse [✅ or ❌ in Ys] will ✅ w/ primary value in Xs") {
    val primaryResult = "1️⃣"
    forAll(succeedingIoAndDurationGen(primaryResult), ioGen(result = "2️⃣")) { case ((p, tp), s) =>
      val program = p.parOrElse(s).timed
      assertEquals(TestControl.executeEmbed(program).unsafeRunSync(), (tp, primaryResult))
    }
  }

  property("[❌ in Xs] parOrElse [✅ in Ys] will ✅ w/ secondary value in max(X, Y)s") {
    val secondaryResult = "2️⃣"
    forAll(failingIoAndDurationGen[String], succeedingIoAndDurationGen(secondaryResult)) { case ((p, tp), (s, ts)) =>
      val program = p.parOrElse(s).timed
      val expectedDuration = Math.max(tp.toSeconds, ts.toSeconds).seconds
      assertEquals(
        TestControl.executeEmbed(program).unsafeRunSync(),
        (expectedDuration, secondaryResult)
      )
    }
  }

  test("[✅ in 5s] parOrElse [✅ in 5s] will ✅ w/ primary value in 5s") {
    val primary = IO.sleep(5.seconds).as("1️⃣")
    val program = primary.parOrElse(secondary = IO.sleep(5.seconds).as("2️⃣"))
    TestControl.execute(program).flatMap { c =>
      for {
        _ <- c.results.assertEquals(None)
        _ <- c.tick
        t <- c.nextInterval
        _ <- IO(assertEquals(t, 5.seconds))
        _ <- c.advanceAndTick(t)
        _ <- c.results.assertEquals(Some(Succeeded("1️⃣")))
      } yield ()
    }
  }

  test("[✅ in 5s] parOrElse [❌ in 5s] will ✅ w/ primary value in 5s") {
    val primary = IO.sleep(5.seconds).as("1️⃣")
    val program = primary.parOrElse(secondary = IO.sleep(5.seconds).as("2️⃣").flatTap(_ => boom))
    TestControl.execute(program) flatMap { c =>
      for {
        _ <- c.results.assertEquals(None)
        _ <- c.tick
        t <- c.nextInterval
        _ <- IO(assertEquals(t, 5.seconds))
        _ <- c.advanceAndTick(t)
        _ <- c.results.assertEquals(Some(Succeeded("1️⃣")))
      } yield ()
    }
  }

  test("[❌ in 5s] parOrElse [✅ in 5s] will ✅ w/ secondary value in 5s") {
    val primary = IO.sleep(5.seconds).as("1️⃣").flatTap(_ => boom)
    val program = primary.parOrElse(secondary = IO.sleep(5.seconds).as("2️⃣"))
    TestControl.execute(program) flatMap { c =>
      for {
        _ <- c.results.assertEquals(None)
        _ <- c.tick
        t <- c.nextInterval
        _ <- IO(assertEquals(t, 5.seconds))
        _ <- c.advanceAndTick(t)
        _ <- c.results.assertEquals(Some(Succeeded("2️⃣")))
      } yield ()
    }
  }

  test("[❌ in 5s] parOrElse([❌ in 5s], customError) will ❌ w/ customError in 5s") {
    val primary = IO.sleep(5.seconds).as("1️⃣").flatTap(_ => boom)
    val customError = Throwable("Custom both-KO error!")
    val program = primary.parOrElse(
      secondary = IO.sleep(5.seconds).as("2️⃣").flatTap(_ => boom),
      customError
    )
    TestControl.execute(program) flatMap { c =>
      for {
        _ <- c.results.assertEquals(None)
        _ <- c.tick
        t <- c.nextInterval
        _ <- IO(assertEquals(t, 5.seconds))
        _ <- c.advanceAndTick(t)
        _ <- c.results.assertEquals(Some(Errored(customError)))
      } yield ()
    }
  }

  test("[✅ in 1s] parOrElse [✅ in 3s] will ✅ w/ primary value in 1s") {
    val primary = IO.sleep(1.seconds).as("1️⃣")
    val program = primary.parOrElse(secondary = IO.sleep(3.seconds).as("2️⃣"))
    TestControl.execute(program) flatMap { c =>
      for {
        _ <- c.results.assertEquals(None)
        _ <- c.tick
        t <- c.nextInterval
        _ <- IO(assertEquals(t, 1.seconds))
        _ <- c.advanceAndTick(t)
        _ <- c.results.assertEquals(Some(Succeeded("1️⃣")))
      } yield ()
    }
  }

  test("[✅ in 1s] parOrElse [❌ in 3s] will ✅ w/ primary value in 1s") {
    val primary = IO.sleep(1.seconds).as("1️⃣")
    val program = primary.parOrElse(secondary = IO.sleep(3.seconds).as("2️⃣").flatTap(_ => boom))
    TestControl.execute(program) flatMap { c =>
      for {
        _ <- c.results.assertEquals(None)
        _ <- c.tick
        t <- c.nextInterval
        _ <- IO(assertEquals(t, 1.seconds))
        _ <- c.advanceAndTick(t)
        _ <- c.results.assertEquals(Some(Succeeded("1️⃣")))
      } yield ()
    }
  }

  test("[✅ in 3s] parOrElse [✅ in 1s] will ✅ w/ primary value in 3s") {
    val primary = IO.sleep(3.seconds).as("1️⃣")
    val program = primary.parOrElse(secondary = IO.sleep(1.seconds).as("2️⃣"))
    TestControl.execute(program) flatMap { c =>
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
        _ <- c.results.assertEquals(Some(Succeeded("1️⃣")))
      } yield ()
    }
  }

  test("[✅ in 3s] parOrElse [❌ in 1s] will ✅ w/ primary value in 3s") {
    val primary = IO.sleep(3.seconds).as("1️⃣")
    val program = primary.parOrElse(secondary = IO.sleep(1.seconds).as("2️⃣").flatTap(_ => boom))
    TestControl.execute(program) flatMap { c =>
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
        _ <- c.results.assertEquals(Some(Succeeded("1️⃣")))
      } yield ()
    }
  }

  test("[❌ in 1s] parOrElse [✅ in 3s] will ✅ w/ secondary value in 3s") {
    val primary = IO.sleep(1.seconds).as("1️⃣").flatTap(_ => boom)
    val program = primary.parOrElse(secondary = IO.sleep(3.seconds).as("2️⃣"))
    TestControl.execute(program) flatMap { c =>
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
        _ <- c.results.assertEquals(Some(Succeeded("2️⃣")))
      } yield ()
    }
  }

  test("[❌ in 3s] parOrElse [✅ in 1s] will ✅ w/ secondary value in 3s") {
    val primary = IO.sleep(3.seconds).as("1️⃣").flatTap(_ => boom)
    val program = primary.parOrElse(secondary = IO.sleep(1.seconds).as("2️⃣"))
    TestControl.execute(program) flatMap { c =>
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
        _ <- c.results.assertEquals(Some(Succeeded("2️⃣")))
      } yield ()
    }
  }

  test("[❌ in 3s] parOrElse([❌ in 1s], customError) will ❌ w/ customError in 3s") {
    val primary = IO.sleep(3.seconds).as("1️⃣").flatTap(_ => boom)
    val customError = Throwable("Custom both-KO error!")
    val program = primary.parOrElse(
      secondary = IO.sleep(1.seconds).as("2️⃣").flatTap(_ => boom),
      customError
    )
    TestControl.execute(program) flatMap { c =>
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
  }

  test("[❌ in 1s] parOrElse([❌ in 3s], customError) will ❌ w/ customError in 3s") {
    val primary = IO.sleep(1.seconds).as("1️⃣").flatTap(_ => boom)
    val customError = Throwable("Custom both-KO error!")
    val program = primary.parOrElse(
      secondary = IO.sleep(3.seconds).as("2️⃣").flatTap(_ => boom),
      customError
    )
    TestControl.execute(program) flatMap { c =>
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
  }

object IoParOrElseSuite:

  case object TestException extends RuntimeException

  def boom[A]: IO[A] = IO.raiseError[A](Throwable("💥 BOOM!"))

  def ioGen[R](result: R): Gen[IO[R]] = ioAndDurationGen(result).map(_._1)
  def succeedingIoGen[R](result: R): Gen[IO[R]] = succeedingIoAndDurationGen(result).map(_._1)
  def failingIoGen[R]: Gen[IO[R]] = failingIoAndDurationGen[R].map(_._1)

  def ioAndDurationGen[R](result: R): Gen[(IO[R], FiniteDuration)] =
    Gen.oneOf(succeedingIoAndDurationGen(result), failingIoAndDurationGen)
  def succeedingIoAndDurationGen[R](result: R): Gen[(IO[R], FiniteDuration)] = Gen.posNum[Int].map { i =>
    val t = i.seconds
    (IO.sleep(t).as(result), t)
  }
  def failingIoAndDurationGen[R]: Gen[(IO[R], FiniteDuration)] = Gen.posNum[Int].map { i =>
    val t = i.seconds
    (boom.delayBy(t), t)
  }
