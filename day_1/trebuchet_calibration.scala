import java.io.File
import scala.io.Source
import scala.collection.mutable.{ListBuffer, ArrayBuffer, HashMap}

object TrebuchetCalibration {
    private val searchMap: Map[Char, Array[String]] = Map(
        'o' -> Array("one"),
        't' -> Array("two", "three"),
        'f' -> Array("four", "five"),
        's' -> Array("six", "seven"),
        'e' -> Array("eight"),
        'n' -> Array("nine")
    )

    private val digitsMap: Map[String, Int] = Map(
        "one" -> 1,
        "two" -> 2,
        "three" -> 3,
        "four" -> 4,
        "five" -> 5,
        "six" -> 6,
        "seven" -> 7,
        "eight" -> 8,
        "nine" -> 9
    )

    private def containsStartWith(searchArr: Array[String], needle: String): Boolean = searchArr.exists(str => str.startsWith(needle))

    private def containsEqual(searchArr: Array[String], needle: String): Boolean = searchArr.exists(str => str == needle)

    private def calculateCalibration(file: File): List[Int] = {
        val source = Source.fromFile(file)
        val lines = source.getLines()
        val list = new ListBuffer[Int]()

        for (line <- lines) {
            val digits = Array(0, 0)

            var i = 0
            var stringDigit = ""
            var searchArr = Array.empty[String]

            for (counter <- 0 until line.size) {
                val c = line(counter)
                if (c.isDigit) {
                    digits(i) = c.asDigit
                    stringDigit = ""
                    i = 1
                } else {
                    if (containsEqual(searchArr, stringDigit.appended(c))) {
                        digits(i) = digitsMap(stringDigit.appended(c))
                        stringDigit = ""
                        i = 1
                    }
                    if (stringDigit.isEmpty && searchMap.contains(c)) {
                        searchArr = searchMap(c)
                    }
                    if (counter < line.size - 1 && containsStartWith(searchArr, stringDigit.appended(c).appended(line(counter + 1)))) {
                        stringDigit = stringDigit.appended(c)
                    } else if (searchMap.contains(c)) {
                        searchArr = searchMap(c)
                        stringDigit = "" + c
                    } else {
                        stringDigit = ""
                    }
                }
            }

            digits(1) = if (digits(1) == 0) digits(0) else digits(1)
            list += digits.mkString.toInt
        }
        list.toList
    }

    def main(args: Array[String]): Unit = {
        val inputFile = new File("./input/trebuchet_calibration_input")
        println(calculateCalibration(inputFile).fold(0)(_ + _))
    }
}
