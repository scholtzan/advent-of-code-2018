import scala.collection.mutable
import scala.io.Source

val input = Source.fromFile("day05-input.txt").mkString.trim

// part 1
def collapse(poly: String): Int = {
  val polymerStack = mutable.Stack[String]()

  poly.split("").foreach { i =>
    if (polymerStack.nonEmpty && polymerStack.head != i && i.toLowerCase == polymerStack.head.toLowerCase) {
      polymerStack.pop()
    } else {
      polymerStack.push(i)
    }
  }

  polymerStack.length
}

val result1 = collapse(input)

// part 2

val removedPolymer = input.split("").distinct.minBy { p =>
  collapse(input.replace(p.toLowerCase, "").replace(p.toUpperCase, ""))
}

val result2 = collapse(input.replace(removedPolymer.toLowerCase, "").replace(removedPolymer.toUpperCase, ""))