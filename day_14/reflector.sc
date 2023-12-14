import java.io.File
import scala.collection.mutable
import scala.io.Source
import java.security.MessageDigest
import scala.collection.mutable.ArrayBuffer

object Reflector {
    def main(args: Array[String]): Unit = {
        val inputFile = new File("./input/test_input")
        val platform = parse(inputFile)
        // println("Original: " + platform.map(_.mkString("")).mkString("\n") + "\n-------------\n\n")
        // part1(platform)
        part2(platform)
        // println("\n" + platform.transpose.map(_.mkString("")).mkString("\n"))
    }

    /** Part 1 roll to north */
    private def part1(platform: Array[Array[Char]]): Unit = {
        rockNRoll(platform)
        println(countLoad(platform.transpose))
    }

    /** Part 2 1000000000 cycles */
    private def part2(platform: Array[Array[Char]]): Unit = {
        val hashMap = new mutable.HashMap[String, Array[Array[Char]]]()
        val statesMap = new mutable.HashMap[String, Array[Array[Char]]]()
        var cyclePlatform = platform

        (1 to 3).foreach(cycle => {
            // println(s"${hash(cyclePlatform)} - ${countLoad(cyclePlatform)}")
            cyclePlatform = statesMap.getOrElseUpdate(hash(cyclePlatform), {
                // println(s"cycle: $cycle -" + statesMap.size)
                cyclePlatform = cyclePlatform.transpose
                cyclePlatform = rockNRoll(cyclePlatform)
                // println("North: \n" + cyclePlatform.map(_.mkString("")).mkString("\n"))
                // Tilt West
                cyclePlatform = cyclePlatform.transpose
                cyclePlatform = rockNRoll(cyclePlatform)
                // println("West: \n" + cyclePlatform.map(_.mkString("")).mkString("\n"))
                // Tilt South
                cyclePlatform = cyclePlatform.reverse.transpose
                cyclePlatform = rockNRoll(cyclePlatform)
                // println("South: \n" + cyclePlatform.map(_.mkString("")).mkString("\n"))
                // Tilt East
                cyclePlatform = cyclePlatform.transpose.reverse.map(_.reverse)
                cyclePlatform = rockNRoll(cyclePlatform)
                // println("East: \n" + cyclePlatform.map(_.mkString("")).mkString("\n"))
                // Tilt North
                cyclePlatform = cyclePlatform.map(_.reverse)
                cyclePlatform
            })
            // println(countLoad(cyclePlatform))
            println(s"Original pos after $cycle: \n" + cyclePlatform.map(_.mkString("")).mkString("\n"))
        })

        println(countLoad(cyclePlatform))
    }

    private def md5(s: String): String = {
        MessageDigest.getInstance("MD5").digest(s.getBytes()).map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
    }

    private def hash(map: Array[Array[Char]]): String = {
        md5((for {
            y <- map.indices
            x <- map(y).indices
            if map(y)(x) == 'O'
        } yield s"$y-$x").mkString("|"))
    }

    private def countLoad(rolledRocksMap: Array[Array[Char]]): Int = {
        rolledRocksMap.zip(LazyList.from(rolledRocksMap.length, -1)).map{
            case (lineArr, lineNumber) =>
                // println(s"Line: ${lineArr.mkString("")} -> Count of rocks = ${lineArr.count(_ == 'O')} on line number $lineNumber")
                lineArr.count(_ == 'O') * lineNumber
        }.sum
    }

    /** Roll the rocks on the entire platform */
    private def rockNRoll(rocksMap: Array[Array[Char]]): Array[Array[Char]] = {
        val alteredMap = rocksMap
        alteredMap.foreach(traverseLine)
        alteredMap
    }

    /** Roll the rocks on one line */
    private def traverseLine(line: Array[Char]): Unit = {
        // println(line.mkString(""))
        var prevRock = -1
        for (i <- line.indices) {
            if (line(i) == 'O') {
                swap(i, prevRock + 1, line)
                prevRock = prevRock + 1
            }
            if (line(i) == '#') {
                prevRock = i
            }
        }
        // println(line.mkString(""))
    }

    /** Custom swap function */
    private def swap(index1: Int, index2: Int, line: Array[Char]): Unit = {
        val mem = line(index1)
        line(index1) = line(index2)
        line(index2) = mem
    }

    /** Platform parser */
    private def parse(file: File): Array[Array[Char]] = {
        val source = Source.fromFile(file)
        source.getLines().toArray.map(_.toCharArray)
    }
}
