import scala.collection.mutable
import scala.io.Source

object Dig {
    case class Dig(direction: Direction, steps: Int, color: String)

    object Dig {
        def from(letter: String, count: String, color: String): Dig = letter match {
            case "U" => Dig(Up, count.toInt, color)
            case "D" => Dig(Down, count.toInt, color)
            case "L" => Dig(Left, count.toInt, color)
            case "R" => Dig(Right, count.toInt, color)
        }
    }

    case class Direction(x: Int, y: Int)

    object Up extends Direction(0, -1)
    object UpRight extends Direction(1, -1)
    object Right extends Direction(1, 0)
    object DownRight extends Direction(1, 1)
    object Down extends Direction(0, 1)
    object DownLeft extends Direction(-1, 1)
    object Left extends Direction(-1, 0)
    object UpLeft extends Direction(-1, -1)

    private val dirArray = Array(Up, UpRight, Right, DownRight, Down, DownLeft, Left, UpLeft)

    case class Position(x: Int, y: Int) {
        @inline def +(dir: Direction): Position = Position(this.x + dir.x, this.y + dir.y)

        def isIn(grid: Array[Array[String]]): Boolean = (0 <= this.x && this.x < grid.head.length) && (0 <= this.y && this.y < grid.length)
    }

    val digRegex = """([UDLR]) (\d+) [(](#\w+)[)]""".r

    lazy val input = Source.fromInputStream(getClass.getResourceAsStream("./input/test_input")).getLines().toList

    /** Parser from List[String] to List[Dig] */
    private def parse(input: List[String]): List[Dig] = input.map {
        case digRegex(dirLetter, steps, color) => Dig.from(dirLetter, steps, color)
    }

    def main(args: Array[String]): Unit = {
        // Parse the instructions
        val instructions = parse(input)
        // Create the grid with the trench
        val grid = dig(instructions)
        // println(grid.map(_.mkString("")).mkString("\n"))

        // Create flooded grid
        val floodedGrid = floodFill(grid)
        println(floodedGrid.map(_.mkString("")).mkString("\n"))

        // Part 1
        println(floodedGrid.map(_.count(_ == "#")).sum)
    }

    /** Digs the trench */
    private def dig(digInstructions: List[Dig]): Array[Array[String]] = {
        // Determine maximum Y
        val Ycalc = digInstructions.filter(digInst => digInst.direction == Down || digInst.direction == Up).foldLeft((0, 0, 0))((accum, curr) => {
            curr.direction match {
                case Down => (accum._1 + curr.steps, Math.max(accum._1 + curr.steps, accum._2), accum._3)
                case Up => (accum._1 - curr.steps, accum._2, Math.min(accum._1 - curr.steps, accum._3))
            }
        })
        // println(Ycalc)
        val maxY = Math.abs(Ycalc._2) + Math.abs(Ycalc._3) + 1
        val startY = Math.abs(Ycalc._3)
        // Determine maximum X
        val Xcalc = digInstructions.filter(digInst => digInst.direction == Right || digInst.direction == Left).foldLeft((0, 0, 0))((accum, curr) => {
            curr.direction match {
                case Right => (accum._1 + curr.steps, Math.max(accum._1 + curr.steps, accum._2), accum._3)
                case Left => (accum._1 - curr.steps, accum._2, Math.min(accum._1 - curr.steps, accum._3))
            }
        })

        // println(Xcalc)
        val maxX = Math.abs(Xcalc._2) + Math.abs(Xcalc._3) + 1
        val startX = Math.abs(Xcalc._3)

        // println(s"Max y = $maxY, Max x = $maxX")

        // Create empty grid of maximum dimensions
        val grid = Array.fill[Array[String]](maxY)(Array.fill[String](maxX)("."))

        // Set current position to determined start position
        var currentPos = Position(startX, startY)

        // For all instructions move the position and fill it with #
        for (instruction <- digInstructions) {
            for (steps <- 1 to instruction.steps) {
                currentPos = currentPos + instruction.direction
                grid(currentPos.y)(currentPos.x) = "#"
            }
        }

        grid
    }

    /** BFS flood fill */
    private def floodFill(grid: Array[Array[String]]): Array[Array[String]] = {
        val floodedGrid = grid.clone()
        val queue = mutable.Queue[Position]()

        val fillStart = determineStartFloodFill(grid)
        queue.enqueue(fillStart)

        while (queue.nonEmpty) {
            val current = queue.dequeue()

            if (current.isIn(floodedGrid) && floodedGrid(current.y)(current.x) != "#") {
                floodedGrid(current.y)(current.x) = "#"
                dirArray.map(current + _).foreach(pos => queue.enqueue(pos))
            }
        }
        floodedGrid
    }

    /** Method to determine the starting point of the flood fill */
    private def determineStartFloodFill(grid: Array[Array[String]]): Position = {
        Position(grid.head.indexOf("#") + 1, 1)
    }
}
