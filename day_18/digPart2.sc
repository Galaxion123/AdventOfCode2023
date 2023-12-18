import scala.collection.AbstractIterator

object DigNotBruteForce {
    implicit class ZipIteratorOps[A](it: Iterator[A]) {
        def zipWithTail: Iterator[(A, A)] = {
            if (it.hasNext) {
                // TODO: can be done with unfold for Iterator?
                new AbstractIterator[(A, A)] {
                    private var prev: A = it.next()

                    override def hasNext: Boolean = it.hasNext

                    override def next(): (A, A) = {
                        val cur = it.next()
                        val ret = (prev, cur)
                        prev = cur
                        ret
                    }
                }
            }
            else
                Iterator.empty
        }

        /*def zipTail: Iterator[(A, A)] = {
          it.sliding(2).map({ case Seq(a, b) => (a, b) }) // simpler but slower (by ~3s)
        }*/

        def zipWithPrev: Iterator[(Option[A], A)] = new AbstractIterator[(Option[A], A)] {
            private var prevOption: Option[A] = None

            override def hasNext: Boolean = it.hasNext

            override def next(): (Option[A], A) = {
                val cur = it.next()
                val ret = (prevOption, cur)
                prevOption = Some(cur)
                ret
            }
        }
    }

    trait PosOps[A <: PosOps[A]] {
        def +(that: A): A
        def *:(k: Int): A

        def unary_- : A = -1 *: this
        def -(that: A): A = this + (-that)

        def manhattanDistance(that: A): Int
    }

    trait PosFactory[A <: PosOps[A]] {
        val zero: A
    }

    trait BoxPosOps[A <: BoxPosOps[A]] extends PosOps[A] {
        def <=(that: A): Boolean
        def min(that: A): A
        def max(that: A): A
    }

    case class Pos(x: Int, y: Int) extends BoxPosOps[Pos] {
        override def +(that: Pos): Pos =
            Pos(x + that.x, y + that.y)

        override def *:(k: Int): Pos =
            Pos(k * x, k * y)

        override def manhattanDistance(that: Pos): Int =
            (x - that.x).abs + (y - that.y).abs

        override def <=(that: Pos): Boolean =
            x <= that.x && y <= that.y

        override def min(that: Pos): Pos =
            Pos(x min that.x, y min that.y)

        override def max(that: Pos): Pos =
            Pos(x max that.x, y max that.y)

        def cross(that: Pos): Long =
            x.toLong * that.y - that.x * y.toLong
    }

    object Pos extends PosFactory[Pos] {
        override val zero: Pos = Pos(0, 0)

        val axisOffsets: Seq[Pos] = Seq(Pos(0, 1), Pos(-1, 0), Pos(1, 0), Pos(0, -1))
        val diagonalOffsets: Seq[Pos] = Seq(Pos(-1, 1), Pos(1, 1), Pos(-1, -1), Pos(1, -1))
        val allOffsets: Seq[Pos] = axisOffsets ++ diagonalOffsets
    }

    case class DigStep(direction: Char, length: Int, color: String)

    type DigPlan = Seq[DigStep]

    private val moveOffsets = Map(
        'U' -> Pos(0, -1),
        'D' -> Pos(0, 1),
        'L' -> Pos(-1, 0),
        'R' -> Pos(1, 0),
    )

    trait Part {
        def extractSteps(digPlan: DigPlan): Seq[(Char, Int)]

        def lagoonSize(digPlan: DigPlan): Long = {
            val steps = extractSteps(digPlan)
            val vertices = steps.scanLeft(Pos.zero)({ case (pos, (direction, length)) =>
                pos + length *: moveOffsets(direction)
            })
            val area = Geometry.polygonArea(vertices)
            val boundary = steps.map(_._2.toLong).sum
            val interior = area - boundary / 2 + 1 // Pick's theorem
            boundary + interior
        }
    }

    object Geometry {
        /**
         * Calculates the area of a simple polygon using the shoelace formula.
         * @see [[https://en.wikipedia.org/wiki/Shoelace_formula]]
         */
        def polygonArea(poss: collection.Seq[Pos]): Long = { // TODO: generalize return type
            ((poss.last +: poss).iterator
                .zipWithTail
                .map(tuple => tuple._1 cross tuple._2)
                .sum / 2).abs
        }
    }

    object Part1 extends Part {
        override def extractSteps(digPlan: DigPlan): Seq[(Char, Int)] =
            digPlan.map(step => (step.direction, step.length))
    }

    object Part2 extends Part {
        implicit class ParseRadixOps(s: String) {
            def toIntRadix(radix: Int): Int = Integer.parseInt(s, radix)

            def toLongRadix(radix: Int): Long = java.lang.Long.parseLong(s, radix)
        }

        def parseColor(color: String): (Char, Int) = {
            val direction = color.last match {
                case '0' => 'R'
                case '1' => 'D'
                case '2' => 'L'
                case '3' => 'U'
            }
            (direction, color.take(5).toIntRadix(16))
        }

        override def extractSteps(digPlan: DigPlan): Seq[(Char, Int)] =
            digPlan.map(step => parseColor(step.color))
    }


    def parseDigStep(s: String): DigStep = s match {
        case s"$direction $length (#$color)" =>
            DigStep(direction.head, length.toInt, color)
    }

    def parseDigPlan(input: String): DigPlan = input.linesIterator.map(parseDigStep).toSeq

    lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("./input/dig_input")).mkString.trim

    def main(args: Array[String]): Unit = {
        println(Part1.lagoonSize(parseDigPlan(input)))
        println(Part2.lagoonSize(parseDigPlan(input)))
    }
}
