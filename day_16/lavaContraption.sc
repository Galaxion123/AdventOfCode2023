import scala.io.Source
import scala.collection.mutable
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success}

object ContraptionDebug {
    case class Coordinates(x: Int, y: Int, direction: String) {
        def move(mv: Move): Coordinates = Coordinates(
            x = this.x + mv.xDir,
            y = this.y + mv.yDir,
            direction = mv.dir
        )
    }

    case class Move(xDir: Int, yDir: Int, dir: String)

    object Right extends Move(xDir = 1, yDir = 0, dir = "->")
    object Left extends Move(xDir = -1, yDir = 0, dir = "<-")
    object Up extends Move(xDir = 0, yDir = -1, dir = "^")
    object Down extends Move(xDir = 0, yDir = 1, dir = "D")

    val dirMap: Map[(String, Char), Array[Move]] = Map(
        ("->", '.') -> Array(Right),
        ("<-", '.') -> Array(Left),
        ("^", '.') -> Array(Up),
        ("D", '.') -> Array(Down),
        ("->", '|') -> Array(Up, Down),
        ("<-", '|') -> Array(Up, Down),
        ("^", '|') -> Array(Up),
        ("D", '|') -> Array(Down),
        ("->", '-') -> Array(Right),
        ("<-", '-') -> Array(Left),
        ("^", '-') -> Array(Right, Left),
        ("D", '-') -> Array(Right, Left),
        ("->", '\\') -> Array(Down),
        ("<-", '\\') -> Array(Up),
        ("^", '\\') -> Array(Left),
        ("D", '\\') -> Array(Right),
        ("->", '/') -> Array(Up),
        ("<-", '/') -> Array(Down),
        ("^", '/') -> Array(Right),
        ("D", '/') -> Array(Left),
    )

    lazy val input = Source.fromInputStream(getClass.getResourceAsStream("./input/contraption_input")).getLines().toArray.map(_.toCharArray)

    def main(args: Array[String]): Unit = {
        val start = System.currentTimeMillis()
        // Part 1
        // println(Await.result(traverseGrid(input), 10.seconds))
        // Part 2
        val startingPositions =
            input.indices.map(yIdx => Coordinates(0, yIdx, "->")).toList ++
                input(0).indices.map(xIdx => Coordinates(xIdx, 0, "D")).toList ++
                input.indices.map(yIdx => Coordinates(input(0).indices.last, yIdx, "<-")).toList ++
                input(0).indices.map(xIdx => Coordinates(xIdx, input.indices.last, "^")).toList

        // println(startingPositions.mkString("List(", ",", ")"))
        val max = Await.result(Future.sequence(startingPositions.map(start => {
            traverseGrid(input, start)
        })).map(_.max), 30.seconds)

        println(max)
        println(s"Done in ${(System.currentTimeMillis() - start) / 1000} seconds.")
    }

    /** BFS traversal */
    private def traverseGrid(contraption: Array[Array[Char]], start: Coordinates = Coordinates(0, 0, "->")): Future[Int] = Future {
        val traversalQueue = mutable.Queue[Coordinates](start)
        val visited = mutable.Set[Coordinates]()

        while (traversalQueue.nonEmpty) {
            // println(visited.mkString("Set(", ",", ")"))
            val coordinates = traversalQueue.dequeue()
            if (!visited.contains(coordinates) && (coordinates.y >= 0 && coordinates.y < contraption.length) && (coordinates.x >= 0 && coordinates.x < contraption.length)) {
                val currentTile = contraption(coordinates.y)(coordinates.x)
                val moves = dirMap((coordinates.direction, currentTile))

                traversalQueue ++= moves.map(coordinates.move)
                visited += coordinates
            }
        }

        visited.groupBy(coord => (coord.x, coord.y)).keys.size
    }
}
