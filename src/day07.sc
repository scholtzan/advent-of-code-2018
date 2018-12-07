import scala.io.Source

val lines = Source.fromFile("day07-input.txt").getLines
val inputPattern = "Step (.) must be finished before step (.) can begin.".r
val nodes = scala.collection.mutable.HashMap[String, Seq[String]]()

lines.foreach { i =>
  val inputPattern(finish, next) = i

  if (nodes.contains(next)) {
    nodes(next) = nodes(next) :+ finish
  } else {
    nodes.put(next, Seq(finish))
  }

  if (!nodes.contains(finish)) {
    nodes.put(finish, Seq())
  }
}

// part 1

def getPath(path: String): String = {
  if (nodes.isEmpty) {
    return path
  }

  val start = nodes.filter(_._2.isEmpty).keys.toSeq.sorted.head

  nodes.keys.foreach { k =>
    nodes(k) = nodes(k).filter(x => start != x)
  }

  nodes.remove(start)

  getPath(path + start)
}

val result = getPath("")


// part 2

def letterToTime(c: String): Int = {
  c.charAt(0) - 'A' + 60
}

def getTime(totalTime: Int, worker: Seq[(Int, String)]): Int = {
  if (nodes.isEmpty && worker.forall(_._1 == 0)) {
    return totalTime
  }

  var freeNodes = nodes.filter(_._2.isEmpty).keys.toSeq.sorted

  val newWorker = worker.map { w =>
    if (w._1 == 0) {
      if (freeNodes.nonEmpty) {
        val next = freeNodes.head
        nodes.remove(next)

        freeNodes = freeNodes.tail

        if (letterToTime(next) == 0) {
          nodes.keys.foreach { k =>
            nodes(k) = nodes(k).filter(x => next != x)
          }
        }

        (letterToTime(next), next)
      } else {
        (0, "")
      }
    } else {
      if (w._1 - 1 == 0) {
        nodes.remove(w._2)
        nodes.keys.foreach { k =>
          nodes(k) = nodes(k).filter(x => w._2 != x)
        }
      }

      (w._1 - 1, w._2)
    }
  }

  getTime(totalTime + 1, newWorker)
}

val result2 = getTime(0, Seq((0, ""), (0, ""), (0, ""), (0, ""), (0, "")))