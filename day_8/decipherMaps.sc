import java.io.File
import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable

object DecipherMaps {
    abstract case class Direction(char: Char, index:  Int)

    object Left extends Direction('L', 0)
    object Right extends Direction('R', 1)

    val mapReg = """(\w+)\s+=\s+[(](\w+),\s+(\w+)[)]""".r

    def main(args: Array[String]): Unit = {
        val start = System.currentTimeMillis()
        val inputFile = new File("./input/maps_input")
        println(countSteps2(parseMaps(inputFile)))
        println("Time to run: " + (System.currentTimeMillis() - start) + " ms.")
    }

    /** Part 1 implementation */
    private def countSteps1(instructionsTuple: (mutable.Queue[Direction], mutable.HashMap[String, Array[String]]), start: String = "AAA"): Long = {
        val dirQueue = instructionsTuple._1
        val map = instructionsTuple._2
        var current = start
        var count: Long = 0

        while (!current.endsWith("Z")) {
            val direction = dirQueue.dequeue()
            current = map(current)(direction.index)
            dirQueue += direction
            count += 1
        }
        count
    }

    /** Part 2 implementation */
    private def countSteps2(instructionsTuple: (mutable.Queue[Direction], mutable.HashMap[String, Array[String]])): Long = {
        val dirQueue = instructionsTuple._1
        val map = instructionsTuple._2
        val startArr = map.keys.filter(_.endsWith("A")).toArray

        val endArr = startArr.map(countSteps1((dirQueue.clone(), map.clone()), _))
        println(endArr.mkString("Array(", ", ", ")"))

        endArr.foldLeft(1L)((a, b) => lcm(a, b))
    }

    /** Greatest common divisor using Euclidean algorithm https://sites.math.rutgers.edu/~greenfie/gs2004/euclid.html */
    @tailrec
    private def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

    /** Lowest common multiple https://www.cuemath.com/numbers/lcm-least-common-multiple/ */
    private def lcm(a: Long, b: Long): Long = (a * b) / gcd(a, b)

    private def parseMaps(file: File): (mutable.Queue[Direction], mutable.HashMap[String, Array[String]]) = {
        val source = Source.fromFile(file)
        val lines = source.getLines()

        val dirQueue = parseDirections(lines.next)
        val map = new mutable.HashMap[String, Array[String]]()

        for (line <- lines) {
            if (!line.isBlank) line match {
                case mapReg(node, fromLeft, fromRight) => map += (node -> Array(fromLeft, fromRight))
            }
        }
        (dirQueue, map)
    }

    private def parseDirections(string: String): mutable.Queue[Direction] = {
        val dirQueue = new mutable.Queue[Direction]()

        for (c <- string) {
            val element = c match {
                case 'L' => Left
                case 'R' => Right
            }
            dirQueue += element
        }
        dirQueue
    }
}
