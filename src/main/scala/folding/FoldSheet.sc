import cats.Apply
import folding.Fold._
import cats.implicits._

import scala.language.postfixOps

fold[List, Int, Int](sum)(1 to 100 toList)

def average: Fold[Long, Double] = Apply[Fold[Long, ?]].map2(sum[Long], genericLength[Long, Long])((a, b) => a.toDouble / b.toDouble)

fold(average)(1L to 1000000 toList)



