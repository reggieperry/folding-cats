import cats.Apply
import cats.implicits._
import folding.Fold
import folding.Fold._
import scala.language.{existentials, postfixOps}
val smallSum = fold[List, Int, Int](sum)(1 to 100 toList)
def average: Fold[Long, Double] = Apply[Fold[Long, ?]].map2(sum[Long], genericLength[Long, Long])((a, b) => a.toDouble / b.toDouble)
val avg = fold(average)(1L to 1000000 toList)
//val average2 = sum[Long].map2(genericLength[Long, Long])((a, b) => a.toDouble / b.toDouble)
//val avg2 = fold(average2)(1L to 1000000 toList)

// Error:(7, 12) value map2 is not a member of folding.FoldW[Long,Long,_$1]

//(sum[Long] |@| genericLength[Long, Long])((a: Long, b: Long) => a.toDouble / b.toDouble)
//val average2 = (sum[Long] |@| genericLength[Long, Long]).map((a: Long, b: Long) => a.toDouble / b.toDouble)
val s = sum[Long]
val gl = genericLength[Long, Long]
val average2 = (s |@| gl).map(_.toDouble / _.toDouble)
fold(average2)(1L to 10000000 toList)


val average3 = (sum[Long] |@| genericLength[Long, Long]) map (_.toDouble / _.toDouble)