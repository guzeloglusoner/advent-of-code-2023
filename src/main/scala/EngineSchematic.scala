package org.guzeloglu.application

import scala.io.Source
import scala.util.matching.Regex

object EngineSchematic extends App {

  private case class NumberLocations(number: Int, location: Int, lineNumber: Int)
  private val schematicLines: List[(String, Int)] = Source.fromFile("src/main/resources/engine_schematic.txt").getLines().toList.zipWithIndex

  private val extractedNumberLocations: List[Int] =
    schematicLines
      .flatMap(e => {
      extractNumberLocations(e._1, e._2)
    })
      .filter(f => {
      checkIfNumberEligible(schematicLines, f)
    })
      .map(_.number)

  println(extractedNumberLocations.sum)

  private def checkIfNumberEligible(schematicLines: List[(String, Int)], number: NumberLocations) = {
    val lineNumber = number.lineNumber
    val numberStartIndex = number.location
    val numberLength = number.number.toString.length

    val searchRange: Seq[Int] = Math.max(0, numberStartIndex - 1) to Math.min(numberStartIndex + numberLength, schematicLines.head._1.length - 1)
    val searchSpace: List[(String, Int)] = schematicLines.filter(e => e._2 == lineNumber || e._2 == lineNumber - 1 || e._2 == lineNumber + 1)
    //println("checking the lineNumber: "+lineNumber + " and the number: " + number.number)
    val searchResult = searchSpace.map(e => {
      !checkTheStringRange(e._1, searchRange)
    })
    println("LINE NUMBER: " +lineNumber + " AND Returned " + searchResult + " for the number: " + number.number)
    searchResult.exists(_.self == true)
  }

  private def checkTheStringRange(searchStr: String, searchRange:Seq[Int]): Boolean = {
    val resultCalc = searchRange.forall(index => {
      val candidateChar: Char = searchStr.charAt(index)
      val dotEquality = candidateChar.compare('.') == 0
      val digitEquality = candidateChar.isDigit
      val res = dotEquality || digitEquality
      res
    })

    resultCalc
  }
  private def extractNumberLocations(schematicLine: String, lineNumber: Int): List[NumberLocations] = {
    val pattern = "\\d+".r
    val numbers = pattern.findAllIn(schematicLine).toList
    var globalIndex: Int = 0
    val numLocList = numbers.map(e => {
      val number = e.toInt
      val startingIndex = schematicLine.indexOf(e, globalIndex)
      globalIndex = Math.max(globalIndex, startingIndex ) + number.toString.length
      NumberLocations(number, startingIndex , lineNumber)
    })

    numLocList
  }
}
