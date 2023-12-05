import java.io.File
import scala.io.Source
import scala.collection.mutable.{ListBuffer, ArrayBuffer, HashMap}
import scala.util.control._

object DecipherSchematic {
    private val mybreaks = new Breaks
    import mybreaks.{break, breakable}

    private val digitsRegEx = """\d+""".r

    def main(args: Array[String]): Unit = {
        val inputFile = new File("./input/parts_input")
        println(decipher(inputFile))
    }

    private def decipher(file: File): (Int, Int) = {
        val source = Source.fromFile(file)
        val lines = source.getLines().toArray
        var symbolsMap = new HashMap[(Int, Int), Char]()

        for {
            (line, y) <- lines.zipWithIndex
            (c, x) <- line.zipWithIndex
            if !c.isDigit && c != '.'
        } {
            symbolsMap += ((y, x) -> c)
        }

        var sum = 0
        var gearsMap = new HashMap[(Int, Int), ListBuffer[Int]]()

        for ((line, y) <- lines.zipWithIndex) {
            for (regMatch <- digitsRegEx.findAllMatchIn(line)) {
                breakable {
                    for (((s_y, s_x), c) <- symbolsMap) {
                        if ((regMatch.start - 1 <= s_x && s_x <= regMatch.end) && (y - 1 <= s_y && s_y <= y + 1)) {
                            var num = regMatch.toString().toInt
                            sum += num
                            if (c == '*') {
                                if (gearsMap.contains(s_y, s_x)) {
                                    gearsMap(s_y, s_x) += num
                                } else {
                                    gearsMap += ((s_y, s_x) -> ListBuffer(num))
                                }
                            }
                            break()
                        }
                    }
                }
            }
        }
        (sum, gearsMap.values.withFilter(_.size == 2).map(_.product).sum)
    }
}
