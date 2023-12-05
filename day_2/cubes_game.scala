import java.io.File
import scala.io.Source
import scala.collection.mutable.{ListBuffer, ArrayBuffer, HashMap}

object CubesGame {
    case class CubeSet(red: Int = 0, green: Int = 0, blue: Int = 0)

    case class Game(id: Int, sets: List[CubeSet])

    private val blueRegex = """(\d+) blue""".r
    private val redRegex = """(\d+) red""".r
    private val greenRegex = """(\d+) green""".r

    def main(args: Array[String]): Unit = {
        val inputFile = new File("./input/games_input")
        println(solvePuzzle(parseGames(inputFile)))
    }

//    private def solvePuzzle(listOfGames: List[Game]): Int = {
//        var res = 0
//        var ruleRed = 12
//        var ruleGreen = 13
//        var ruleBlue = 14
//
//        for (game <- listOfGames) {
//            if (game.sets.forall(set => set.red <= ruleRed && set.green <= ruleGreen && set.blue <= ruleBlue)) res += game.id
//        }
//        res
//    }

    private def solvePuzzle(listOfGames: List[Game]): Int = {
        var res = 0

        for (game <- listOfGames) {
            var maxRed = 0
            var maxGreen = 0
            var maxBlue = 0

            for (set <- game.sets) {
                maxRed = Math.max(maxRed, set.red)
                maxGreen = Math.max(maxGreen, set.green)
                maxBlue = Math.max(maxBlue, set.blue)
            }

            // println(s"maxRed: $maxRed, maxGreen: $maxGreen, maxBlue: $maxBlue")
            res += (maxRed * maxGreen * maxBlue)
        }
        res
    }

    private def parseGames(file: File): List[Game] = {
        val source = Source.fromFile(file)
        val lines = source.getLines()
        val listOfGames = new ListBuffer[Game]()

        for (line <- lines) {
            val id = line.takeWhile(_ != ':').dropWhile(!_.isDigit).toInt
            val sets = line.dropWhile(_ != ':').drop(1).trim.split(';')
            val listOfCubeSets = new ListBuffer[CubeSet]()

            for (set <- sets) {
                val colors = set.split(',').map(_.trim)
                var cubeSet = CubeSet()

                listOfCubeSets += colors.foldLeft(CubeSet())((accum, curr) => curr match {
                    case blueRegex(number) => accum.copy(blue = number.toInt)
                    case redRegex(number) => accum.copy(red = number.toInt)
                    case greenRegex(number) => accum.copy(green = number.toInt)
                })
            }

            listOfGames += Game(id = id, sets = listOfCubeSets.toList)
//            println(
//                s"""The line: $line
//                   |  the id -> $id
//                   |  the list of sets -> ${listOfCubeSets.toList.mkString(", ")}
//                   |""".stripMargin
//            )
        }
        listOfGames.toList
    }
}
