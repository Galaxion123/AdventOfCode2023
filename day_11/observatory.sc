import java.io.File
import scala.collection.mutable
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Observatory {
    case class Galaxy(x: Int, y: Int)

    case class Pair(galaxy1: Galaxy, galaxy2: Galaxy)

    def main(args: Array[String]): Unit = {
        val inputFile = new File("./input/galaxies_input")

        val (spaceMap, mapOfExpandedRows, mapOfExpandedColumns) = parseAndExpand(inputFile, 1000000L)
        //println(spaceMap.map(_.mkString("")).mkString("\n"))
        println(galaxyPairs(spaceMap).length)

        println(part1(galaxyPairs(spaceMap), mapOfExpandedRows, mapOfExpandedColumns))
    }

    /** Map parser */
    def parseAndExpand(file: File, expansionRate: Long): (Array[Array[Char]], mutable.HashMap[Int, Long], mutable.HashMap[Int, Long]) = {
        val source = Source.fromFile(file)
        val lines = source.getLines().toArray.map(_.toCharArray)

        val mapOfExpandedRows = new mutable.HashMap[Int, Long]()

        for (y <- lines.indices) {
            if (lines(y).contains('#')) {
                mapOfExpandedRows += (y -> 1)
            } else {
                mapOfExpandedRows += (y -> expansionRate)
            }
        }

        val mapOfExpandedColumns = new mutable.HashMap[Int, Long]()
        val transposedLines = lines.transpose

        for (x <- transposedLines.indices) {
            if (transposedLines(x).contains('#')) {
                mapOfExpandedColumns += (x -> 1)
            } else {
                mapOfExpandedColumns += (x -> expansionRate)
            }
        }

        (lines, mapOfExpandedRows, mapOfExpandedColumns)
    }

    /** Determine pairs */
    private def galaxyPairs(spaceMap: Array[Array[Char]]): Array[Pair] = {
        val galaxies = new ArrayBuffer[Galaxy]()

        for {
            y <- spaceMap.indices
            x <- spaceMap(y).indices
        } {
            if (spaceMap(y)(x) == '#') {
                galaxies += Galaxy(x, y)
            }
        }

        (for {
            i <- galaxies.indices
            j <- i + 1 until galaxies.length
        } yield Pair(galaxy1 = galaxies(i), galaxy2 = galaxies(j))).toArray
    }

    private def part1(pairs: Array[Pair], mapOfExpandedRows: mutable.HashMap[Int, Long], mapOfExpandedColumns: mutable.HashMap[Int, Long]): Long = {
        // distance(pairs.head, mapOfExpandedRows, mapOfExpandedColumns)
        pairs.map(distance(_, mapOfExpandedRows, mapOfExpandedColumns)).sum
    }

    /** Calculate disctance between galaxies in a pair */
    private def distance(pair: Pair, mapOfExpandedRows: mutable.HashMap[Int, Long], mapOfExpandedColumns: mutable.HashMap[Int, Long]): Long = {
        val (startGalaxyX, endGalaxyX) = if (pair.galaxy1.x < pair.galaxy2.x) (pair.galaxy1, pair.galaxy2) else (pair.galaxy2, pair.galaxy1)
        val (startGalaxyY, endGalaxyY) = if (pair.galaxy1.y < pair.galaxy2.y) (pair.galaxy1, pair.galaxy2) else (pair.galaxy2, pair.galaxy1)

        // println("Pait: " + pair + " : " + startGalaxy + " -> " + endGalaxy)

        var res = 0L

        for (x <- startGalaxyX.x + 1 to endGalaxyX.x) {
            res += mapOfExpandedColumns(x)
        }

        for (y <- startGalaxyY.y + 1 to endGalaxyY.y) {
            res += mapOfExpandedRows(y)
        }

        res
    }
}
