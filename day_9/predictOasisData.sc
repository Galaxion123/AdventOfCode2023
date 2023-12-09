import java.io.File
import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object PredictOasisData {
    private val digitsReg = """\d+|-\d+""".r

    def main(args: Array[String]): Unit = {
        val start = System.currentTimeMillis()
        val inputFile = new File("./input/OASIS_input")
        println(solverPart2(inputFile))
        println("Done in " + (System.currentTimeMillis() - start) + " ms.")
    }

    /** Part 1 solver */
    private def solverPart1(file: File): Int = {
        val source = Source.fromFile(file)
        val lines = source.getLines()
        var res = 0

        for (line <- lines) {
            val numArray = digitsReg.findAllMatchIn(line).map(_.toString.toInt).toArray
            res += predictNext(numArray, numArray.last)
        }
        res
    }

    @tailrec
    /** Part 1 algorithm */
    private def predictNext(numArray: Array[Int], topDiff: Int): Int = {
        // println(numArray.mkString("Array(", ", ", ")") + " | topdiff = " + topDiff)
        if (numArray.forall(_ == 0)) {
            topDiff
        } else {
            val resArray = new ArrayBuffer[Int]()
            numArray.tail.foldLeft(numArray.head)((prevNum, currNum) => {
                resArray += currNum - prevNum
                currNum
            })
            predictNext(resArray.toArray, topDiff + resArray.last)
        }
    }

    /** Part 2 solver */
    private def solverPart2(file: File): Int = {
        val source = Source.fromFile(file)
        val lines = source.getLines()
        var res = 0

        for (line <- lines) {
            val numArray = digitsReg.findAllMatchIn(line).map(_.toString.toInt).toArray
            val prev = predictPrev(numArray, ListBuffer(numArray.head))
            res += prev
        }
        res
    }

    @tailrec
    /** Part 2 algorithm */
    private def predictPrev(numArray: Array[Int], diffArr: ListBuffer[Int]): Int = {
        // println(numArray.mkString("Array(", ", ", ")") + " | topdiff = " + diffArr.mkString("Array(", ", ", ")"))
        if (numArray.forall(_ == 0)) {
            diffArr.tail.foldLeft(diffArr.head)((accum, curr) => {
                curr - accum
            })
        } else {
            val resArray = new ArrayBuffer[Int]()
            numArray.tail.foldLeft(numArray.head)((prevNum, currNum) => {
                resArray += currNum - prevNum
                currNum
            })
            predictPrev(resArray.toArray, resArray.head +=: diffArr)
        }
    }
}
