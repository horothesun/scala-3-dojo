package ioparorelse

import cats.effect.IO
import cats.effect.Outcome.*

object IoParOrElse:

  extension [A](primary: IO[A])
    def parOrElse(secondary: IO[A], bothKo: Throwable = Throwable("Both IOs failed or got cancelled.")): IO[A] =
      IO.racePair(primary, secondary).flatMap {
        case Left((po @ Succeeded(p), sf)) => p.flatTap(_ => sf.cancel)
        case Left((_, sf)) =>
          sf.join.flatMap {
            case Succeeded(s) => s
            case _            => IO.raiseError[A](bothKo)
          }
        case Right((pf, so)) =>
          pf.join.flatMap {
            case Succeeded(p) => p
            case _ =>
              so match
                case Succeeded(s) => s
                case _            => IO.raiseError[A](bothKo)
          }
      }
