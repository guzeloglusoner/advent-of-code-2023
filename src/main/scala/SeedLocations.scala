package org.guzeloglu.application

import scala.collection.immutable.NumericRange
import scala.io.Source
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParSeq

object SeedLocations extends App {
  private val lines: List[String] = Source.fromFile("src/main/resources/test_seed_to_location.txt").getLines().toList

  private val commonString: String = s"%s-to-%s map:"
  private val seeds: List[String] = lines.head.split(":").last.trim.split(" ").toList
  //val minLocation = seeds.map(e => getLocation(lines, e.toLong)).min
  // println(minLocation)
  private val seedMap: ParSeq[Long] = seeds.map(e => e.toLong).par

  private val seedMapPairs: ParSeq[(Long, Long)] = seedMap zip seedMap.tail
  private val seedMapPairRanges: ParSeq[NumericRange.Inclusive[Long]] = seedMapPairs.map(e => e._1 to e._1 + e._2)

  private val rangeCalculations: ParSeq[Long] = seedMapPairRanges.map(e => {
    e.toList.par.map(t => getLocation(lines, t)).min
  })

  println(rangeCalculations.min)

  private def getNextSourceNumber(lines: List[String], source: String, destination: String, searchSourceNumber: Long): Long = {
    val theMap = lines.dropWhile(_.equals(commonString.format(source, destination)) == false).takeWhile(!_.equals("")).par
    val destSrcRangeLines = theMap.tail.par
    val result: Option[Long] = destSrcRangeLines.map(e => {
      val Array(destRangeStart, srcRangeStart, range) = e.split(" ")
      if (searchSourceNumber >= srcRangeStart.toLong && searchSourceNumber <= srcRangeStart.toLong + range.toLong)
        searchSourceNumber + (destRangeStart.toLong - srcRangeStart.toLong)
      else
        searchSourceNumber
    }).find(_ != searchSourceNumber)

    result match {
      case Some(value) => value
      case None => searchSourceNumber
    }
  }

  private def getLocation(lines: List[String], seedNumber: Long): Long = {
    val seed2SoilNumber = getNextSourceNumber(lines, "seed", "soil", seedNumber)
    val soil2FertilizerNumber = getNextSourceNumber(lines, "soil", "fertilizer", seed2SoilNumber)
    val fertilizer2WaterNumber = getNextSourceNumber(lines, "fertilizer", "water", soil2FertilizerNumber)
    val water2LightNumber = getNextSourceNumber(lines, "water", "light", fertilizer2WaterNumber)
    val light2TemperatureNumber = getNextSourceNumber(lines, "light", "temperature", water2LightNumber)
    val temperature2HumidityNumber = getNextSourceNumber(lines, "temperature", "humidity", light2TemperatureNumber)
    val humidity2LocationNumber = getNextSourceNumber(lines, "humidity", "location", temperature2HumidityNumber)
    humidity2LocationNumber
  }
}
