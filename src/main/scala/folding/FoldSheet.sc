import cats.Apply
import cats.implicits._
import folding.Fold
import folding.Fold._
import scala.language.{existentials, postfixOps}
val smallSum = fold[List, Int, Int](sum)(1 to 100 toList)
def average: Fold[Long, Double] = Apply[Fold[Long, ?]].map2(sum[Long], genericLength[Long, Long])((a, b) => a.toDouble / b.toDouble)
val avg = fold(average)(1L to 1000000 toList)

val s = sum[Long]
val gl = genericLength[Long, Long]
val average2 = (s |@| gl).map(_.toDouble / _.toDouble)
//fold(average2)(1L to 10000000 toList)


val average3 = (sum[Long] |@| genericLength[Long, Long]) map (_.toDouble / _.toDouble)

def sumSq[A](implicit x: Numeric[A]): Fold[A, A] = Fold[A, A](x.zero)((b, a) => x.plus(b, x.times(a, a)) )

val std =
  (sumSq[BigInt] |@| sum[BigInt] |@| genericLength[BigInt, BigInt]) map {(ss: BigInt, s: BigInt, len: BigInt) =>
    val sl = s / len
    val ssl = ss / len
    val rad = ssl - (sl * sl)
    math.sqrt(rad.doubleValue())
  }

val ans = fold(std)(BigInt(1) to BigInt(1000000) toList)
//fold(genericLength[BigInt, Long])(BigInt(1) to BigInt(10) toList)