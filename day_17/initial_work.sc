import scala.io.Source
import scala.collection.mutable

object Crucible {
    case class Node(x: Int, y: Int, direction: String = ">") {
        def neighbours(cityMap: Array[Array[Int]]): List[Node] = {
            List(
                Node(this.x + 1, this.y), // Right
                Node(this.x - 1, this.y, "<"), // Left
                Node(this.x, this.y + 1, "|"), // Down
                Node(this.x, this.y - 1, "^"), // Up
            ).filter(node => {
                (0 <= node.x && node.x < cityMap.head.length) && (0 <= node.y && node.y < cityMap.length)
            })
        }
    }

    implicit def orderingByHeatLoss: Ordering[(Node, Int)] = Ordering.by(tuple => -tuple._2)

    lazy val input = Source.fromInputStream(getClass.getResourceAsStream("./input/test_input")).getLines().toArray.map(_.toCharArray.map(_.asDigit))

    def main(args: Array[String]): Unit = {
        traverse(input)
    }

    def traverse(cityMap: Array[Array[Int]]): Unit = {
        val heatLossArray = cityMap.clone().map(_.map(_ => Int.MaxValue))
        val prevArray: Array[Array[Option[Node]]] = cityMap.clone().map(_.map(_ => None))
        val dirArray: Array[Array[Int]] = cityMap.clone().map(_.map(_ => 0))

        val queue = mutable.PriorityQueue[(Node, Int)]((Node(0, 0), 0))

        val target = Node(cityMap.head.indices.last, cityMap.indices.last)

        while (queue.nonEmpty) {
            val (currentNode, heatLoss) = queue.dequeue()

            // println(queue.mkString(","))
            // println(currentNode.neighbours(cityMap).mkString("List(", ",", ")"))

            currentNode.neighbours(cityMap).foreach(neighbour => {
                // Determine the HeatLoss on the neighbour
                val altHL = heatLoss + cityMap(neighbour.y)(neighbour.x)

                // If the HL is less than the current value in the HL array
                if (altHL < heatLossArray(neighbour.y)(neighbour.x) && !(neighbour.direction == currentNode.direction && dirArray(currentNode.y)(currentNode.x) == 2)) {
                    // Update the value in the HL array
                    heatLossArray(neighbour.y)(neighbour.x) = altHL
                    // Add the current Node to the prevArray cell of the neighbour
                    prevArray(neighbour.y)(neighbour.x) = Some(currentNode)
                    // Add the count of steps for the neighbour if the same direction is used
                    if (neighbour.direction == currentNode.direction) dirArray(neighbour.y)(neighbour.x) = dirArray(currentNode.y)(currentNode.x) + 1
                    // Enqueue the neighbour
                    queue.enqueue((neighbour, altHL))
                }
            })
        }

        println(heatLossArray.map(_.mkString(",")).mkString("\n"))

        println("\n" + heatLossArray(target.y)(target.x) + "\n")

        val resMap = cityMap.clone.map(_.map(_.toString))
        var currNode: Option[Node] = Some(target)

        while (currNode.nonEmpty && !currNode.contains(Node(0, 0))) {
            resMap(currNode.get.y)(currNode.get.x) = currNode.get.direction
            currNode = prevArray(currNode.get.y)(currNode.get.x)
        }

        println(resMap.map(_.mkString("")).mkString("\n"))
    }
}
