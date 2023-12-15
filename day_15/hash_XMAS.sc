import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object XMASHash {
    lazy val input = Source.fromInputStream(getClass.getResourceAsStream("./input/initialization_input")).mkString.split(',').map(_.trim())

    val boxMap = Array.fill(256)(ArrayBuffer[Lens]()).zipWithIndex.map(tuple => tuple._2 -> tuple._1).toMap

    val stepReg = """(\w+)[=-](\d+)?""".r

    case class Lens(label: String, focalLength: Int)

    trait Part {
        def hash(input: Array[String]): Int = input.map(XMASHash).sum
    }

    object Part1 extends Part

    object Part2 extends Part {
        def calculateFocusPower(input: Array[String]): Int = {
            input.foreach(parseLensAction)
            // println(boxMap(0))
            // println(boxMap(3))
            boxMap.map {
                case (key, arr) => arr.zip(LazyList.from(1)).map{
                    case (lens, pos) => (1 + key) * pos * lens.focalLength
                }.sum
            }.sum
        }
    }

    def main(args: Array[String]): Unit = {
        println(Part1.hash(input))
        println(Part2.calculateFocusPower(input))
    }

    /** Perform required actions */
    private def parseLensAction(step: String): Unit = step match {
        case stepReg(label, null) =>
            val box = XMASHash(label)
            boxMap(box).find(_.label == label) match {
                case Some(lens) =>
                    boxMap(box) -= lens
                case None =>
            }
        case stepReg(label, focal) =>
            val newLens = Lens(label, focal.toInt)
            val box = XMASHash(label)
            boxMap(box).find(_.label == label) match {
                case Some(lens) =>
                    boxMap(box)(boxMap(box).indexOf(lens)) = newLens
                case None =>
                    boxMap(box) += newLens
            }
    }


    /** Holiday ASCII String Helper */
    private def XMASHash(step: String): Int = {
        // println(step.mkString)
        val res = step.foldLeft(0)((accum, char) => {
            (accum + char.toInt) * 17 % 256
        })
        // println(res)
        res
    }
}
