import java.io.File
import scala.io.Source
import scala.collection.mutable.{ListBuffer, ArrayBuffer, HashMap}
import scala.util.control._

object AlmanacSolver {
    case class Almanac(
        seeds: Array[(Long, Long)],
        stsMap: HashMap[(Long, Long), (Long, Long)],
        stfMap: HashMap[(Long, Long), (Long, Long)],
        ftwMap: HashMap[(Long, Long), (Long, Long)],
        wtlMap: HashMap[(Long, Long), (Long, Long)],
        lttMap: HashMap[(Long, Long), (Long, Long)],
        tthMap: HashMap[(Long, Long), (Long, Long)],
        htlMap: HashMap[(Long, Long), (Long, Long)]
    )

    private val mybreaks = new Breaks
    import mybreaks.{break, breakable}

    private val digitsRegEx = """\d+""".r

    def main(args: Array[String]): Unit = {
        val inputFile = new File("./input/almanac_input")
        println(determineMinimalLocation(decipherAlmanac(inputFile)))
    }

    private def decipherAlmanac(file: File): Almanac = {
        val lines = Source.fromFile(file).getLines()

        // seeds array
        var seeds = Array.empty[(Long, Long)]

        // seed-to-soil flag and map
        var stsFlag = false
        var stsMap = new HashMap[(Long, Long), (Long, Long)]()

        // soil-to-fertilizer flag and map
        var stfFlag = false
        var stfMap = new HashMap[(Long, Long), (Long, Long)]()

        // fertilizer-to-water flag and map
        var ftwFlag = false
        var ftwMap = new HashMap[(Long, Long), (Long, Long)]()

        // water-to-light flag and map
        var wtlFlag = false
        var wtlMap = new HashMap[(Long, Long), (Long, Long)]()

        // light-to-temperature flag and map
        var lttFlag = false
        var lttMap = new HashMap[(Long, Long), (Long, Long)]()

        // temperature-to-humidity flag and map
        var tthFlag = false
        var tthMap = new HashMap[(Long, Long), (Long, Long)]()

        // humidity-to-location flag and map
        var htlFlag = false
        var htlMap = new HashMap[(Long, Long), (Long, Long)]()

        for (line <- lines) {
            if (line.startsWith("seeds:")) {
                seeds = digitsRegEx
                    .findAllMatchIn(line).toArray.map(_.toString.toLong)
                    .grouped(2).map(arr => (arr(0), arr(0) + arr(1) - 1)).toArray
            } else if (line.startsWith("seed-to-soil map:")) {
                stsFlag = true
            } else if (line.startsWith("soil-to-fertilizer map:")) {
                stfFlag = true
            } else if (line.startsWith("fertilizer-to-water map:")) {
                ftwFlag = true
            } else if (line.startsWith("water-to-light map:")) {
                wtlFlag = true
            } else if (line.startsWith("light-to-temperature map:")) {
                lttFlag = true
            } else if (line.startsWith("temperature-to-humidity map:")) {
                tthFlag = true
            } else if (line.startsWith("humidity-to-location map:")) {
                htlFlag = true
            } else if (!line.isBlank) {
                val arrayOfDigits = digitsRegEx.findAllMatchIn(line).toArray.map(_.toString.toLong)
                val mapEntry = Map((arrayOfDigits(1), arrayOfDigits(1) + arrayOfDigits(2) - 1) -> (arrayOfDigits(0), arrayOfDigits(0) + arrayOfDigits(2) - 1))
                (stsFlag, stfFlag, ftwFlag, wtlFlag, lttFlag, tthFlag, htlFlag) match {
                    case (true, false, false, false, false, false, false) => stsMap ++= mapEntry
                    case (true, true, false, false, false, false, false) => stfMap ++= mapEntry
                    case (true, true, true, false, false, false, false) => ftwMap ++= mapEntry
                    case (true, true, true, true, false, false, false) => wtlMap ++= mapEntry
                    case (true, true, true, true, true, false, false) => lttMap ++= mapEntry
                    case (true, true, true, true, true, true, false) => tthMap ++= mapEntry
                    case (true, true, true, true, true, true, true) => htlMap ++= mapEntry
                    case (_, _, _, _, _, _, _) => println(line)
                }
            }
        }

        Almanac(seeds, stsMap, stfMap, ftwMap, wtlMap, lttMap, tthMap, htlMap)
    }

    private def determineMinimalLocation(almanac: Almanac): Long = {
        var res = Long.MaxValue
        for (seed <- almanac.seeds) {
//            println("soil")
            val soil = determineMapping2(List(seed), almanac.stsMap)
//            println("fertilizer")
            val fertilizer = determineMapping2(soil, almanac.stfMap)
//            println("water")
            val water = determineMapping2(fertilizer, almanac.ftwMap)
//            println("light")
            val light = determineMapping2(water, almanac.wtlMap)
//            println("temp")
            val temp = determineMapping2(light, almanac.lttMap)
//            println("humidity")
            val humidity = determineMapping2(temp, almanac.tthMap)
//            println("location")
            val location = determineMapping2(humidity, almanac.htlMap)
            res = Math.min(res, location.map(_._1).min)
        }
        res
    }

    private def determineMapping(from: Long, map: HashMap[(Long, Long), (Long, Long)]): Long = {
        var to = from
        for (
            ((from_start, from_end), (to_start, to_end)) <- map
        ) {
            if (from_start <= from && from <= from_end) {
                to = to_start + (from - from_start)
            }
        }
        to
    }

    private def determineMapping2(fromRanges: List[(Long, Long)], map: HashMap[(Long, Long), (Long, Long)]): List[(Long, Long)] = {
        var stackOfFromfRanges = fromRanges
        var stackOfToRanges = List.empty[(Long, Long)]

//        println(map.mkString(", "))
//        println(stackOfFromfRanges.mkString(", "))

        while (stackOfFromfRanges.nonEmpty) {
            val (fromStart, fromEnd) = stackOfFromfRanges.head

            breakable {
                for (
                    ((from_start, from_end), (to_start, to_end)) <- map
                ) {
                    if (from_start <= fromStart && fromEnd <= from_end) {
                        stackOfFromfRanges = stackOfFromfRanges.tail
                        stackOfToRanges = (to_start + (fromStart - from_start), to_start + (fromEnd - from_start)) :: stackOfToRanges
                        break()
                    } else if (fromStart < from_start && from_start <= fromEnd && fromEnd <= from_end) {
                        stackOfFromfRanges = stackOfFromfRanges.tail
                        stackOfFromfRanges = List((fromStart, from_start - 1), (from_start, fromEnd)) ::: stackOfFromfRanges
                        break()
                    } else if (from_start <= fromStart && fromStart <= from_end && from_end < fromEnd) {
                        stackOfFromfRanges = stackOfFromfRanges.tail
                        stackOfFromfRanges = List((fromStart, from_end), (from_end + 1, fromEnd)) ::: stackOfFromfRanges
                        break()
                    }
                }
                stackOfToRanges = (fromStart, fromEnd) :: stackOfToRanges
                stackOfFromfRanges = stackOfFromfRanges.tail
            }
        }

        stackOfToRanges
    }
}
