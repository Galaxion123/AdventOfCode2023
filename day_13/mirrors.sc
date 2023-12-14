import java.io.File
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Mirrors {
    case class Pattern(pattern: Array[Array[Char]])

    case class PatternBuilder(builder: ArrayBuffer[String] = new ArrayBuffer()) {
        def toPattern: Pattern = Pattern(this.builder.map(_.toCharArray).toArray[Array[Char]])
    }

    def main(args: Array[String]): Unit = {
        val inputFile = new File("./input/mirrors_input")
        val parsedPatterns = parsePatterns(inputFile)
        println(parsedPatterns.length)

        println(part1(parsedPatterns))
        println(part2(parsedPatterns))
    }

    /** Part 1 solver */
    private def part1(patterns: Array[Pattern]): Int = {
        val horizontal = patterns.map(pat => findReflection(pat.pattern)).sum
        val vertical = patterns.map(pat => findReflection(pat.pattern.transpose)).sum

        // println(s"Horizontal = $horizontal and Vertical = $vertical")

        100 * horizontal + vertical
    }

    /** Part 2 solver */
    private def part2(patterns: Array[Pattern]): Int = {
        val horizontal = patterns.map(pat => findReflection(pat.pattern, 1)).sum
        val vertical = patterns.map(pat => findReflection(pat.pattern.transpose, 1)).sum

        // println(s"Horizontal = $horizontal and Vertical = $vertical")

        100 * horizontal + vertical
    }

    private def findReflection(pattern: Array[Array[Char]], allowedSmudges: Int = 0): Int = {
        val res = pattern.indices.map(currentLine => {
            var top = currentLine
            var bottom = currentLine + 1
            var smudgesLeft = allowedSmudges

            while (smudgesLeft >= 0 && top >= 0 && bottom < pattern.length) {
                val smudges = pattern(top).indices.count(i => {
                    pattern(top)(i) != pattern(bottom)(i)
                })
                smudgesLeft -= smudges
                top -= 1
                bottom += 1
            }
            if (smudgesLeft == 0 && currentLine != pattern.indices.last) currentLine + 1 else 0
        })

        // println(res.mkString(", "))

        res.max
    }

    /** Pattern parser */
    private def parsePatterns(file: File): Array[Pattern] = {
        val source = Source.fromFile(file)
        val lines = source.getLines()

        val patterBuilderArray = ArrayBuffer(PatternBuilder())

        for (line <- lines) {
            if (line.isBlank && lines.hasNext) {
                patterBuilderArray += PatternBuilder()
            } else {
                patterBuilderArray.last.builder += line
            }
        }

        patterBuilderArray.map(_.toPattern).toArray
    }
}
