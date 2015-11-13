import cats.Apply
import cats.implicits._
import folding.Fold
import folding.Fold._
import scala.language.{existentials, postfixOps}
object BigDecimalMath {
  import scala.language.implicitConversions
  implicit def toBigDecimal(decimal: String): BigDecimal = BigDecimal(decimal)
  def sqrt(x: BigDecimal): BigDecimal = {
    val maxIterations = x.mc.getPrecision + 1

    val guessSteam: Stream[BigDecimal] = newtonRaphsonApproximations(x).take(maxIterations)
    val exactMatch: Option[Stream[BigDecimal]] = guessSteam.sliding(2).find(a => a(0) == a(1))
    val root: Stream[BigDecimal] = exactMatch.getOrElse(Stream(guessSteam.last))
    root(0)
  }

  /**
   * A sequence of BigDecimals the tend towards the square root of toSqrt.
   * Using the Newton Raphson Approximations http://en.wikipedia.org/wiki/Newton's_method
   * @param toSqrt the value to find the root of
   * @param guess the first guess to iterate over (typically toSqrt/2)
   * @return
   */
  private[this] def newtonRaphsonApproximations(toSqrt: BigDecimal, guess: BigDecimal): Stream[BigDecimal] =
    Stream.cons(guess, newtonRaphsonApproximations(toSqrt, ((toSqrt / guess) + guess) / 2))

  private[this] def newtonRaphsonApproximations(toSqrt: BigDecimal): Stream[BigDecimal] =
    newtonRaphsonApproximations(toSqrt, toSqrt / 2)

}


val smallSum = fold[List, Int, Int](sum)(1 to 100 toList)
def average: Fold[Long, Double] = Apply[Fold[Long, ?]].map2(sum[Long], genericLength[Long, Long])((a, b) => a.toDouble / b.toDouble)
val avg = fold(average)(1L to 1000000 toList)

val s = sum[Long]
val gl = genericLength[Long, Long]
val average2 = (s |@| gl).map(_.toDouble / _.toDouble)
//fold(average2)(1L to 10000000 toList)

foldLeft(average)(Seq(1,2,3,4,5))

val average3 = (sum[Long] |@| genericLength[Long, Long]) map (_.toDouble / _.toDouble)

def sumSq[A](implicit x: Numeric[A]): Fold[A, A] = Fold[A, A](x.zero)((b, a) => x.plus(b, x.times(a, a)) )


val std =
  (sumSq[BigInt] |@| sum[BigInt] |@| genericLength[BigInt, BigInt]) map {(ss: BigInt, s: BigInt, len: BigInt) =>
    val sl = s / len
    val ssl = ss / len
    val rad = ssl - (sl * sl)
    BigDecimalMath.sqrt(BigDecimal(rad))
//    math.sqrt(rad.doubleValue())
  }
val ans = fold(std)(BigInt(1) to BigInt(1000000) toList)
val ans2 = std.fold((BigInt(1) to BigInt(1000000) toList))
//fold(genericLength[BigInt, Long])(BigInt(1) to BigInt(10) toList)
def mapF[A, B](f: A => B): Fold[A, List[B]] =
  Fold(Nil: List[B])((b, a) => f(a) :: b)