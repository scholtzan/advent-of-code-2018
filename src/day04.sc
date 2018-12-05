import scala.io.Source

val lines = Source.fromFile("day04-input.txt").getLines.toSeq
val cache = scala.collection.mutable.Map[Int, Seq[Int]]()

val guardChangePattern = "\\[.+\\] Guard #([0-9]+) begins shift".r
val sleepPattern = "\\[[0-9]+-[0-9]+-[0-9]+ [0-9]+:([0-9]+)\\] falls asleep".r
val wakePattern = "\\[[0-9]+-[0-9]+-[0-9]+ [0-9]+:([0-9]+)\\] wakes up".r

val sortedInput = lines.sorted.foldLeft((0, 0)) { (acc, i) =>
  i match {
    case guardChangePattern(guardID) =>
      (guardID.toInt, 0)
    case wakePattern(time) =>
      if (cache.contains(acc._1)) {
        cache(acc._1) = cache(acc._1) ++ (acc._2 to time.toInt)
      } else {
        cache.put(acc._1, acc._2 to time.toInt)
      }
      (acc._1, time.toInt)
    case sleepPattern(time) =>
      (acc._1, time.toInt)
  }
}

val guard = cache.maxBy(_._2.length)
val result = guard._1 * guard._2.groupBy(x => x).maxBy(_._2.length)._1

val guardMinute = cache.mapValues(m => m.groupBy(x => x).maxBy(_._2.length))
val maxGuard = guardMinute.maxBy(_._2._2.length)
val result2 = maxGuard._1 * maxGuard._2._1