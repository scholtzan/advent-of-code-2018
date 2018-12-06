val input = Seq(
  (195, 221),
  (132, 132),
  (333, 192),
  (75, 354),
  (162, 227),
  (150, 108),
  (46, 40),
  (209, 92),
  (153, 341),
  (83, 128),
  (256, 295),
  (311, 114),
  (310, 237),
  (99, 240),
  (180, 337),
  (332, 176),
  (212, 183),
  (84, 61),
  (275, 341),
  (155, 89),
  (169, 208),
  (105, 78),
  (151, 318),
  (92, 74),
  (146, 303),
  (184, 224),
  (285, 348),
  (138, 163),
  (216, 61),
  (277, 270),
  (130, 155),
  (297, 102),
  (197, 217),
  (72, 276),
  (299, 89),
  (357, 234),
  (136, 342),
  (346, 221),
  (110, 188),
  (82, 183),
  (271, 210),
  (46, 198),
  (240, 286),
  (128, 95),
  (111, 309),
  (108, 54),
  (258, 305),
  (241, 157),
  (117, 162),
  (96, 301)
)

val maxRows = input.maxBy(_._1)._1 + 1
val maxCols = input.maxBy(_._2)._2 + 1

// part 1

val grid = collection.mutable.ArrayBuffer.fill(maxRows, maxCols)((0, maxCols + maxRows))

input.zipWithIndex.foreach { case ((row, col), index) =>
  for (r <- 0 until maxRows) {
    for (c <- 0 until maxCols) {
      val dist = math.abs(row - r) + math.abs(col - c)

      if (grid(r)(c)._2 == dist) {
        grid(r)(c) = (-1, dist)
      } else if (grid(r)(c)._2 > dist) {
        grid(r)(c) = (index, dist)
      }
    }
  }
}

val infiniteCoordinates = (grid.head ++ grid.last ++ grid.flatMap(x => Seq(x.head, x.last))).map(_._1).distinct
val validCoordinates = grid.flatten.filter(x => x._1 != -1 && !infiniteCoordinates.contains(x._1))
val result = validCoordinates.groupBy(x => x._1).maxBy(_._2.length)._2.length

// part 2

val regions = (0 to maxRows).flatMap { r =>
  (0 to maxCols).flatMap { c =>
    if (input.map(x => math.abs(x._1 - r) + math.abs(x._2 - c)).sum < 10000) {
      Some(true)
    } else {
      None
    }
  }
}
val result2 = regions.length