package folding

import cats._

import scala.language.higherKinds

sealed trait FoldMW[M[_], A, B, W] { self =>

  def step: (W, A) => M[W]
  def begin: M[W]
  def done: W => M[B]

  def map[C](f: B => C)(implicit M: Monad[M]): FoldMW[M, A, C, W] = {
    //def done(w: W): M[C] =  M.map[B,C](self.done(w))(b => f(b))

    FoldMW(self.step, self.begin, self.done andThen M.lift(f))
  }

  def ap[C, Z](f: FoldMW[M, A, B => C, Z])(implicit M: Monad[M]): FoldMW[M, A, C, (Z, W)] = {
    def step(x: (Z, W), a: A): M[(Z, W)] = M.flatMap(f.step(x._1, a)){ xL1 =>
      M.flatMap(self.step(x._2, a)){ xR1 =>
        M.pure((xL1, xR1))
      }
    }

    def begin: M[(Z, W)] = M.flatMap(f.begin){ xL =>
      M.flatMap(self.begin){ xR =>
        M.pure((xL, xR))
      }
    }

    def done(x: (Z, W)): M[C] = M.flatMap(f.done(x._1)){ fL =>
      M.flatMap(self.done(x._2)){ b =>
        M.pure(fL(b))
      }
    }

    FoldMW(step, begin, done)
  }

  def foldM[F[_]](fa: F[A])(implicit F: Foldable[F], M: Monad[M]): M[B] = {
    def step(a: A, k: W => M[B], w: W): M[B] = M.flatMap(self.step(w, a))(k)

    M.flatMap(self.begin){w =>
      F.foldLeft[A, M[B]](fa, done(w))((mb, a) => step(a, done, w))
    }

  }


}



object FoldMW {
  def apply[M[_], A, B, W](s:(W, A) => M[W] , b:M[W] , d:W => M[B] )(implicit M: Monad[M]):FoldMW[M, A, B, W] = new FoldMW[M, A, B, W] {
    override def step: (W, A) => M[W] = s
    override def done: (W) => M[B] = d
    override def begin: M[W] = b
  }
  //case class Id[X](run: X) extends AnyVal

  type FoldMId[A,B, W] = FoldMW[Id, A, B, W]

}

object FoldM {
  type FoldM[M[_], A, B] = FoldMW[M, A, B, _]

  def apply[M[_], A, B](z: M[B])(f: (B, A) => M[B])(implicit M: Monad[M]) = FoldMW[M, A, B, B](f, z, { case b => M.pure(b)})

  implicit def foldMFunctor[M[_], A, B](implicit M: Monad[M]): Functor[FoldM[M, A, ?]] = new Functor[FoldM[M, A, ?]] {
    override def map[C, D](fa: FoldM[M, A, C])(f: (C) => D): FoldM[M, A, D] = fa map f
  }

  implicit def foldMApplicative[M[_],X](implicit M: Monad[M]): Applicative[FoldM[M, X, ?]] = new Applicative[FoldM[M, X, ?]] {
    override def pure[B](x: B): FoldM[M, X, B] = FoldMW[M, X, B, Unit]({ case ((), _) => M.pure(()) }, M.pure(()), _ => M.pure(x))

    override def ap[A, B](fa: FoldM[M, X, A])(f: FoldM[M, X, (A) => B]): FoldM[M, X, B] = fa.ap(f)
  }
}
