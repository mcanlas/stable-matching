package com.htmlism.stablematching.core

import cats.Monad
import cats.syntax.all.*

object TestUtils:
  def applyTimes[F[_]: Monad, A](x: A)(n: Int)(f: A => F[A]): F[A] =
    (1 to n)
      .foldLeft(x.pure[F]) { (acc, _) =>
        Monad[F]
          .flatMap(acc)(y => f(y))
      }
