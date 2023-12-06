import java.io.File
import scala.io.Source
import scala.collection.mutable.{ListBuffer, ArrayBuffer, HashMap}

object RaceSolver {
    private val digitsRegex = """\d+""".r

    def main(args: Array[String]): Unit = {
        var startTime = System.currentTimeMillis()
        val inputFile = new File("./input/races_input")
        println(calculateWinningRaces2(parseRaceConditions2(inputFile)))
        println("Total time to run: " + (System.currentTimeMillis() - startTime) + " ms.")
    }

    /**
     * First part parsing function
     */
    private def parseRaceConditions(file: File): Array[(Int, Int)] = {
        val source = Source.fromFile(file)
        val numbers = source.getLines().toArray.map(digitsRegex.findAllMatchIn(_).map(_.toString.toInt).toArray)

        val resArray = new ArrayBuffer[(Int, Int)]()

        for (i <- 0 until numbers(0).size) {
            resArray += ((numbers(0)(i), numbers(1)(i)))
        }
        resArray.toArray
    }

    /**
     * Second part parsing function
     */
    private def parseRaceConditions2(file: File): (Long, Long) = {
        val source = Source.fromFile(file)
        val numbers = source.getLines().toArray.map(digitsRegex.findAllMatchIn(_).map(_.toString).mkString.toLong)
        (numbers(0), numbers(1))
    }

    /**
     * First part calcualtion function
     */
    private def calculateWinningRaces(races: Array[(Int, Int)]): Int = {
        var res = 1

        for ((availableTime, recordDistance) <- races) {
            var numberOfWinningOptions = 0
            for (i <- 0 to availableTime) {
                val distance = i * (availableTime - i)
                if (distance > recordDistance) numberOfWinningOptions += 1
            }
            res = res * numberOfWinningOptions
        }
        res
    }

    /**
     * Second part calcualtion function
     */
    private def calculateWinningRaces2(races: (Long, Long)): Long = {
        val (availableTime, recordDistance) = races

        var minSpeed = 0
        var maxSpeed = availableTime
        var tries = 0

        while (minSpeed * (availableTime - minSpeed) < recordDistance) {
            minSpeed += 1
            tries += 1
        }
        while (maxSpeed * (availableTime - maxSpeed) < recordDistance) {
            maxSpeed -= 1
            tries += 1
        }
        availableTime - tries + 1
    }
}
