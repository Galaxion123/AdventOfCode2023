import java.io.File
import scala.io.Source
import scala.collection.mutable

object CamelCardsGame {
    private val regex = """\w+""".r

    case class Hand(cards: String, value: Int, score: Int)

    object Hand {
        implicit def orderingByValue: Ordering[Hand] = (x: Hand, y: Hand) => {
            val diff = x.value - y.value
            if (diff == 0) orderingByHand.compare(x, y) else diff
        }

        def orderingByHand: Ordering[Hand] = handsOrdering.on(h => h.cards)

        def handsOrdering: Ordering[String] = (x: String, y: String) => {
            x.zip(y).foldLeft(0) {
                case (result, (cx, cy)) => if (result == 0) cardsOrdering.compare(cx, cy) else result
            }
        }

        /** A, K, Q, J, T, 9, 8, 7, 6, 5, 4, 3, or 2 */
        def cardsOrdering: Ordering[Char] = Ordering.by {
            case 'A' => 12
            case 'K' => 11
            case 'Q' => 10
            case 'T' => 9
            case '9' => 8
            case '8' => 7
            case '7' => 6
            case '6' => 5
            case '5' => 4
            case '4' => 3
            case '3' => 2
            case '2' => 1
            case 'J' => 0
            case _ => 100
        }
    }

    def main(args: Array[String]): Unit = {
        val inputFile = new File("./input/camelCards_input")
        println(parseAndStoreHands(inputFile))
    }

    def parseAndStoreHands(file: File): Int = {
        val source = Source.fromFile(file)
        val hands = source.getLines().toArray
            .map(regex.findAllMatchIn(_).map(_.toString).toArray)
            .map(item => (item(0), item(1).toInt))
            .map(input => Hand(input._1, calculateValueOfHand(input._1), input._2))

        hands.sorted.zip(LazyList.from(1)).foldLeft(0)((accum, item) => {
            accum + (item._1.score * item._2)
        })
    }

    def calculateValueOfHand(hand: String): Int = {
        val hashMap = new mutable.HashMap[Char, Int]()
        var jFlag = false

        hand.foreach(c => {
            if (c == 'J') jFlag = true
            if (hashMap.contains(c)) hashMap(c) += 1
            else hashMap += (c -> 1)
        })


        if (jFlag && hashMap.size != 1) {
            val addTo = hashMap.keys.foldLeft(('i', 0))((accum, key) => {
                if (key != 'J' && hashMap(key) > accum._2) {
                    (key, hashMap(key))
                } else accum
            })._1
            hashMap(addTo) += hashMap('J')
            hashMap -= 'J'
        }

        val size = hashMap.size
        if (size == 1) 7 // Five of a kind
        else if (size == 2 && hashMap.values.exists(_ == 4)) 6 // Four of a kind
        else if (size == 2) 5 // Full house
        else if (size == 3 && hashMap.values.exists(_ == 3)) 4 // Three of a kind
        else if (size == 3) 3 // Two pair
        else if (size == 4) 2 // One pair
        else 1
    }
}
