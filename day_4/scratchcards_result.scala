import java.io.File
import scala.io.Source
import scala.collection.mutable.HashSet
import scala.annotation.tailrec

object ScratchcardsOutput {
    def main(args: Array[String]): Unit = {
        val inputFile = new File("./input/cards_input")
        println(countCardsOutput(inputFile))
    }

    private def countCardsOutput(file: File): Int = {
        val source = Source.fromFile(file)
        val lines = source.getLines().toArray.map((_, 1))

        countCardsRec(lines, 0)
    }

    @tailrec
    private def countCardsRec(cards: Array[(String, Int)], finito: Int): Int = {
        if (cards.isEmpty) {
            finito
        } else {
            val parts = cards.head._1.dropWhile(_ != ':').drop(1).split('|')
            val winningSet = parseNumbers(parts(0))
            val mySet = parseNumbers(parts(1))

            val res = winningSet.filter(mySet.contains(_)).size

            countCardsRec(updatedTail(res, cards.tail, cards.head._2), finito + cards.head._2)
        }
    }

    private def parseNumbers(part: String): HashSet[Int] = {
        part.split(' ').filter(!_.isBlank).foldLeft(new HashSet[Int]())((winningSet, num) => {
            winningSet.addOne(num.toInt)
        })
    }

    private def updatedTail(res: Int, tail: Array[(String, Int)], times: Int): Array[(String, Int)] = {
        val end = if (res > tail.size) tail.size else res

        for {
            i <- 0 until end
            j <- 1 to times
        } {
            tail(i) = (tail(i)._1, tail(i)._2 + 1)
        }
        tail
    }
}
