package org.guzeloglu.application

import scala.Int.*
import scala.io.Source

object CalibrationValue extends App {

  private val lines: List[String] = Source.fromFile("src/main/resources/calibration_values.txt").getLines().toList
  println(
    lines
      .map(e => getCalibration(e))
      .sum
  )

  private def getCalibration(str: String): Int = {
    val firstDigit: Int = getTheFirstDigit(str)
    val lastDigit: Int = getTheLastDigit(str)
    println((firstDigit * 10) + lastDigit)
    (firstDigit * 10) + lastDigit
  }

  private def getTheFirstDigit(str: String): Int = {
    val indexOfFirstDigit = str.indexOf(str.toList.find(_.isDigit).get)
    val candidateString = str.substring(0, indexOfFirstDigit)
    val candidateDigit = getFirstWrittenDigit(candidateString, indexOfFirstDigit)
    if (candidateDigit != 0) candidateDigit
    else str.charAt(indexOfFirstDigit).asDigit

  }

  private def getFirstWrittenDigit(str: String, indexOfFirstDigit: Int): Int = {
    for (offset <- 0 to indexOfFirstDigit) {
      if (str.startsWith("one", offset)) return 1
      if (str.startsWith("two", offset)) return 2
      if (str.startsWith("three", offset)) return 3
      if (str.startsWith("four", offset)) return 4
      if (str.startsWith("five", offset)) return 5
      if (str.startsWith("six", offset)) return 6
      if (str.startsWith("seven", offset)) return 7
      if (str.startsWith("eight", offset)) return 8
      if (str.startsWith("nine", offset)) return 9
    }
    0
  }
  private def getTheLastDigit(str:String): Int = {
    val indexOfLastDigit = str.lastIndexOf(str.toList.findLast(_.isDigit).get)
    val candidateString = str.substring(indexOfLastDigit + 1, str.length)
    val candidateDigit = getLastWrittenDigit(candidateString)
    if (candidateDigit != 0) candidateDigit
    else str.charAt(indexOfLastDigit).asDigit
  }

  private def getLastWrittenDigit(str: String): Int = {
    var myStr = str
    for (offset <- 0 to myStr.length) {
      if (myStr.endsWith("one")) return 1
      else if (myStr.endsWith("two")) return 2
      else if (myStr.endsWith("three")) return 3
      else if (myStr.endsWith("four")) return 4
      else if (myStr.endsWith("five")) return 5
      else if (myStr.endsWith("six")) return 6
      else if (myStr.endsWith("seven")) return 7
      else if (myStr.endsWith("eight")) return 8
      else if (myStr.endsWith("nine")) return 9
      else
        myStr = myStr.dropRight(1)
    }
    0
  }


}
