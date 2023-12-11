import java.io.File
import scala.io.Source
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object PipesMaze {
    case class Connection(x: Int, y: Int)

    object Up extends Connection(x = 0, y = -1)
    object Down extends Connection(x = 0, y = 1)
    object Left extends Connection(x = -1, y = 0)
    object Right extends Connection(x = 1, y = 0)

    case class Position(x: Int, y: Int) {
        @inline def +(conn: Connection): Position = Position(this.x + conn.x, this.y + conn.y)

        override def toString: String = s"pos y: $y, x: $x"
    }

    sealed trait Tile {
        def isGround: Boolean
    }

    case class Pipe(symbol: Char, position: Position, connections: Array[Position]) extends Tile {
        def isConnectedTo(nextTile: Tile): Boolean = nextTile match {
            case nextPipe: Pipe => nextPipe.connections.contains(position)
            case _: Ground => false
        }

        override def isGround: Boolean = false

        override def toString: String = s"$symbol on $position, with connections: ${connections.mkString("Array(", ", ", ")")}"
    }

    case class Ground(position: Position) extends Tile {
        override def isGround: Boolean = true

        override def toString: String = s"Ground on $position"
    }

    def main(args: Array[String]): Unit = {
        val inputFile = new File("./input/pipes_input")
        val (startPos, map) = parseMap(inputFile)
        println(startPos)
        // println(map.map(_.mkString("Array(", ", ", ")\n")).mkString("Array(", ", ", ")"))
        val part1 = findFarthestTile(startPos, map)
        println("Part 1: steps until farther pipe - " + part1._1 + ".")
        println("Part 2: " + findTilesInsideLoop(part1._2.toSet, map) + " tile inside of the loop.")
    }

    /** Part 1 solver */
    private def findFarthestTile(startPos: Position, pipesMap: Array[Array[Tile]]): (Int, mutable.Set[Position]) = {
        val start = pipesMap(startPos.y)(startPos.x)
        val visited = mutable.Set[Position]()
        var res = 0

        start match {
            case startPipe: Pipe =>
                visited.add(startPipe.position)
                val validConnections =
                    startPipe.connections.filter(conn => startPipe.isConnectedTo(pipesMap(conn.y)(conn.x))).map(pos => {
                        pipesMap(pos.y)(pos.x)
                    }).map((_, true))
                while (validConnections.exists(_._2)) {
                    for (i <- validConnections.indices) {
                        validConnections(i) = navigatePar(validConnections(i)._1, visited, pipesMap)
                    }
                    res += 1
                }
                (res, visited)
            case _: Ground => (0, visited)
        }
    }

    /** Part 2 solver */
    private def findTilesInsideLoop(loopSet: Set[Position], pipesMap: Array[Array[Tile]]): Int = {
        printLoopOnMap(loopSet, pipesMap)
        val loopSetGroupedByY = loopSet.groupBy(_.y)
        val nonLoopTiles = pipesMap.flatMap(row => {
            row.filter {
                case pipe: Pipe => !loopSet.contains(pipe.position)
                case _ => true
            }
        })
        nonLoopTiles.count {
            case pipe: Pipe => isPositionInsideLoop(pipe.position, loopSetGroupedByY, pipesMap)
            case ground: Ground => isPositionInsideLoop(ground.position, loopSetGroupedByY, pipesMap)
        }
    }

    private def isPositionInsideLoop(position: Position, loopSetGroupedByY: Map[Int, Set[Position]], pipesMap: Array[Array[Tile]]): Boolean = {
        val leftWalls = loopSetGroupedByY.getOrElse(position.y, Set.empty[Position]).filter(_.x < position.x).toArray.sortBy(_.x)

        val cornerPairs = Map('F' -> '7', 'L' -> 'J')

        val wallCount = leftWalls.foldLeft((0, '.')) {
            case ((count, prevCorner), currWall) =>
                pipesMap(currWall.y)(currWall.x) match {
                    case pipe: Pipe => pipe.symbol match {
                        case '|' => (count + 1, prevCorner)
                        case c if cornerPairs.get(prevCorner).contains(c) => (count - 1, c)
                        case c if c == 'L' || c == 'F' => (count + 1, c)
                        case c if c == '7' || c == 'J' => (count, c)
                        case _ => (count, prevCorner)
                    }
                    case _ => (count, prevCorner)
                }
        }
        wallCount._1 % 2 == 1
    }

    /** Pretty printer for visual debugging */
    private def printLoopOnMap(loopSet: Set[Position], pipesMap: Array[Array[Tile]]): Unit = {
        val printMap = pipesMap.map(row => {
            row.map {
                case pipe: Pipe => if (loopSet.contains(pipe.position)) '0' else pipe.symbol
                case ground: Ground => if (loopSet.contains(ground.position)) '0' else '.'
            }.mkString("")
        })
        println(printMap.mkString("\n"))
    }

    /** Navigate to next tile */
    private def navigatePar(tile: Tile, visited: mutable.Set[Position], pipesMap: Array[Array[Tile]]): (Tile, Boolean) = tile match {
        case pipe: Pipe =>
            if (pipe.connections.forall(visited.contains)) {
                visited += pipe.position
                //println("End " + pipe)
                (tile, false)
            } else {
                val nextPosOpt = pipe.connections.find(
                    pos => !visited.contains(pos) &&
                        !pipesMap(pos.y)(pos.x).isGround &&
                        pipe.isConnectedTo(pipesMap(pos.y)(pos.x))
                )
                nextPosOpt match {
                    case Some(nextPos) =>
                        //println("Continue " + pipesMap(nextPos.y)(nextPos.x))
                        visited += pipe.position
                        (pipesMap(nextPos.y)(nextPos.x), true)
                    case _ =>
                        visited += pipe.position
                        //println("End " + pipe)
                        (tile, false)
                }
            }
        case _ => (tile, false)
    }

    /** Map parser */
    private def parseMap(file: File): (Position, Array[Array[Tile]]) = {
        val source = Source.fromFile(file)
        val lines = source.getLines.toArray

        val arrayOfTileRows = new ArrayBuffer[Array[Tile]]()
        var start = Position(0, 0)

        for (y <- lines.indices) {
            val arrayOfTiles = new ArrayBuffer[Tile]()

            for (x <- lines(y).indices) {
                lines(y)(x) match {
                    case '|' =>
                        val tile = Pipe(
                            symbol = '|',
                            position = Position(x, y),
                            connections =
                                (if (y != 0) Array(Position(x, y) + Up) else Array[Position]()) ++
                                    (if (y != lines.indices.last) Array(Position(x, y) + Down) else Array[Position]())
                        )
                        arrayOfTiles += tile
                    case '-' =>
                        val tile = Pipe(
                            symbol = '-',
                            position = Position(x, y),
                            connections =
                                (if (x != 0) Array(Position(x, y) + Left) else Array[Position]()) ++
                                    (if (x != lines(y).indices.last) Array(Position(x, y) + Right) else Array[Position]())
                        )
                        arrayOfTiles += tile
                    case 'L' =>
                        val tile = Pipe(
                            symbol = 'L',
                            position = Position(x, y),
                            connections =
                                (if (y != 0) Array(Position(x, y) + Up) else Array[Position]()) ++
                                    (if (x != lines(y).indices.last) Array(Position(x, y) + Right) else Array[Position]())
                        )
                        arrayOfTiles += tile
                    case 'J' =>
                        val tile = Pipe(
                            symbol = 'J',
                            position = Position(x, y),
                            connections =
                                (if (y != 0) Array(Position(x, y) + Up) else Array[Position]()) ++
                                    (if (x != 0) Array(Position(x, y) + Left) else Array[Position]())
                        )
                        arrayOfTiles += tile
                    case '7' =>
                        val tile = Pipe(
                            symbol = '7',
                            position = Position(x, y),
                            connections =
                                (if (x != 0) Array(Position(x, y) + Left) else Array[Position]()) ++
                                    (if (y != lines.indices.last) Array(Position(x, y) + Down) else Array[Position]())
                        )
                        arrayOfTiles += tile
                    case 'F' =>
                        val tile = Pipe(
                            symbol = 'F',
                            position = Position(x, y),
                            connections =
                                (if (y != lines.indices.last) Array(Position(x, y) + Down) else Array[Position]()) ++
                                    (if (x != lines(y).indices.last) Array(Position(x, y) + Right) else Array[Position]())
                        )
                        arrayOfTiles += tile
                    case '.' =>
                        arrayOfTiles += Ground(Position(x, y))
                    case 'S' =>
                        start = Position(x, y)
                        val tile = Pipe(
                            symbol = provideS(),
                            position = start,
                            connections =
                                (if (x != 0) Array(Position(x, y) + Left) else Array[Position]()) ++
                                    (if (x != lines(y).indices.last) Array(Position(x, y) + Right) else Array[Position]()) ++
                                    (if (y != 0) Array(Position(x, y) + Up) else Array[Position]()) ++
                                    (if (y != lines.indices.last) Array(Position(x, y) + Down) else Array[Position]())
                        )
                        arrayOfTiles += tile
                    case _ => println(s"Uknown tile ${lines(y)(x)} at position $y - $x")
                }
            }
            arrayOfTileRows += arrayOfTiles.toArray
        }
        (start, arrayOfTileRows.toArray)
    }

    private def provideS(): Char = {
        println("Enter S value")
        scala.io.StdIn.readChar()
    }
}
