import scala.io.Source

val lines = Source.fromFile("day02-input.txt").getLines.toSeq
val occurrences = lines.map { i: String =>
  val grouped = i.split("").groupBy(x => x)
  (grouped.exists(_._2.length == 2), grouped.exists(_._2.length == 3))
}

val result = occurrences.count(_._1 == true) * occurrences.count(_._2 == true)

for (i <- 0 until lines.head.length) {
  val grouped = lines.map { s =>
    s.substring(0, i) + s.substring(i + 1, s.length)
  }.groupBy(x => x)

  val matches = grouped.filter(_._2.length > 1)

  if (matches.nonEmpty) {
    println(matches)
  }
}