import scala.io.Source

object Crucible{
    case class Grid(grid: Vector[Vector[Int]]) {
        def at(col: Int, row: Int): Int = grid(row)(col)

        def height: Int = grid.length

        def width: Int = grid(0).length

        def cols: Range = 0 until width

        def rows: Range = 0 until height

        def isInBounds(col: Int, row: Int): Boolean = cols.contains(col) && rows.contains(row)

        def neighborhood4(col: Int, row: Int): Seq[(Int, Int)] = neighborhood4Offset(col, row, 1)

        def neighborhood4Offset(col: Int, row: Int, offset: Int): Seq[(Int, Int)] =
            Seq(
                (col + offset, row),
                (col, row + offset),
                (col - offset, row),
                (col, row - offset),
            ).filter { case (c, r) => isInBounds(c, r) }
    }

    object Grid {
        def parse(input: String): Grid =
            Grid(input.linesIterator.map(_.map(_.asDigit).toVector).toVector)
    }

    type Position = (Int, Int)

    implicit class PositionOps(pos: Position) {
        def col: Int = pos._1

        def row: Int = pos._2

        def -(other: Position): Position = (pos._1 - other._1, pos._2 - other._2)

        def dot(other: Position): Int = pos._1 * other._1 + pos._2 * other._2

        def isOrthogonalTo(other: Position): Boolean = dot(other) == 0

        def manhattanLength: Int = pos._1.abs + pos._2.abs
    }

    case class Node(last2: Seq[Position]) {
        def position: Position = last2.head
    }

    lazy val input = Source.fromInputStream(getClass.getResourceAsStream("./input/map_input")).mkString.trim

    def main(args: Array[String]): Unit = {
        println("Part 1: " + minimumHeatLossWithStandardCrucible(input))
        println("Part 2: " + minimumHeatLossWithUltraCrucible(input))
    }

    def minimumHeatLossWithStandardCrucible(input: String): Int =
        minimumHeatLoss(input, 1 to 3)

    def minimumHeatLossWithUltraCrucible(input: String): Int =
        minimumHeatLoss(input, 4 to 10)

    def minimumHeatLoss(input: String, movementRange: Range): Int = {
        val grid = Grid.parse(input)
        dijkstra(adjacentNodes(grid, movementRange))(Node(Seq((0, 0)))).flatMap {
            case (k, v) => Option.when(k.position == (grid.cols.last, grid.rows.last))(v)
        }.min
    }

    def adjacentNodes(grid: Grid, offsets: Iterable[Int])(node: Node): Iterable[(Node, Int)] = {
        val current@(col, row) = node.position
        val possibleNeighbors = for {
            offset <- offsets.toSet
            neighbor <- grid.neighborhood4Offset(col, row, offset)
        } yield neighbor

        val adjacent = node.last2 match {
            case (col, row) :: Nil => possibleNeighbors
            case current :: previous :: Nil =>
                possibleNeighbors.filter { neighbor =>
                    (neighbor - current).isOrthogonalTo(current - previous)
                }
        }

        def heatLossInStraightLine(from: Position, toPos: Position): Int = {
            val difference = toPos - from
            val colOffset = if (difference.col.sign == 0) 1 else difference.col.sign
            val rowOffset = if (difference.row.sign == 0) 1 else difference.row.sign
            val colRange = from.col to toPos.col by colOffset
            val rowRange = from.row to toPos.row by rowOffset
            val heatLosses = for {
                col <- colRange
                row <- rowRange
            } yield grid.at(col, row)
            heatLosses.sum - grid.at(from.col, from.row)
        }

        adjacent.map { neighbor =>
            (Node(Seq(neighbor, current)), heatLossInStraightLine(current, neighbor))
        }
    }

    def dijkstra(adjacent: Node => Iterable[(Node, Int)])(start: Node): Map[Node, Int] = {
        import scala.collection.mutable

        implicit def ordering: Ordering[(Node, Int)] = Ordering.by(-_._2)

        val nodesToProcess = mutable.PriorityQueue((start, 0))
        val visitedNodes = mutable.HashMap.empty[Node, Int]

        while (nodesToProcess.nonEmpty) {
            val (node, totalWeight) = nodesToProcess.dequeue()
            if (!visitedNodes.contains(node) || visitedNodes(node) > totalWeight) {
                visitedNodes.put(node, totalWeight)
                val nextNodes = adjacent(node).map { case (n, weight) =>
                    (n, totalWeight + weight)
                }
                nextNodes.foreach { n =>
                    nodesToProcess.enqueue(n)
                }
            }
        }
        visitedNodes.toMap
    }
}
