import cats.Apply
import folding.Fold
import folding.Fold._
import cats.implicits._

import scala.language.postfixOps

def sumF[A](implicit x: scala.math.Numeric[A]): Fold[A, A] =
  Fold(x.zero)(x.plus)

def productF[A](implicit x: scala.math.Numeric[A]): Fold[A, A] =
  Fold(x.one)(x.times)

def lengthF[A]: Fold[A, Int] =
  Fold(0)({case (b, _) => b + 1})

foldLeft[Int, Int](sum)(1 to 100 toList)

def average[A]: Fold[Int, Int] = Apply[Fold[Int, ?]].map2(sum[Int], lengthF[Int])(_ / _)

//fold(average)(1 to 1000000 toStream)

val foo = 1 to 10000000 toList

foldLeft(lengthF[Int])(foo)


//def sumSq[A](implicit x: scala.math.Numeric[A]): Fold[A, A] =
//  Fold()