import scala.io.Source

var file = Source.fromFile("day08-input.txt").mkString
var input = file.trim.split(" ").map(_.toInt).toBuffer

// part 1

def licenseCheck(): Int = {
  val children = input.head
  val metaDataLength = input(1)
  input.remove(0)
  input.remove(0)

  val childrenMetaData = (0 until children).map { c =>
    licenseCheck()
  }

  val metadata = (0 until metaDataLength).map { i =>
    val tmp = input.head
    input.remove(0)
    tmp
  }.sum + childrenMetaData.sum

  metadata
}

val result = licenseCheck()


// part 2

def licenseCheck2(): Int = {
  val children = input.head
  val metaDataLength = input(1)
  input.remove(0)
  input.remove(0)

  val childrenMetaData: Seq[Int] = (0 until children).map { c =>
    licenseCheck2()
  }

  val metadata: Seq[Int] = (0 until metaDataLength).map { i =>
    val tmp = input.head
    input.remove(0)

    if (childrenMetaData.isEmpty) {
      tmp
    } else if (tmp <= childrenMetaData.length) {
      childrenMetaData(tmp - 1)
    } else {
      0
    }
  }

  metadata.sum
}

val result2 = licenseCheck2()