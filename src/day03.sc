import scala.io.Source

val lines = Source.fromFile("day03-input.txt").getLines.toSeq
val inputPattern = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r
val fabricUsages = scala.collection.mutable.Map[(Int, Int), Int]()
val claims = scala.collection.mutable.Map[Int, Seq[(Int, Int)]]()


val occurrences = lines.foreach { i: String =>
  val inputPattern(claim, x, y, width, height) = i
  claims.put(claim.toInt, Seq())

  for (col <- x.toInt until x.toInt + width.toInt) {
    for (row <- y.toInt until y.toInt + height.toInt) {
      if (fabricUsages.contains((col, row))) {
        fabricUsages((col, row)) += 1
      } else {
        fabricUsages.put((col, row), 1)
      }

      claims(claim.toInt) = claims(claim.toInt) :+ (col, row)
    }
  }
}

val result1 = fabricUsages.count(_._2 > 1)

val result2 = claims.find(c => c._2.forall(x => fabricUsages(x) == 1))