package org.guzeloglu.application

import scala.io.Source
import scala.util.matching.Regex

object EngineSchematic extends App {

  private case class NumberLocations(number: Int, location: Int, lineNumber: Int)

  private case class GearLocations(location: Int, lineNumber: Int)

  private val schematicLines: List[(String, Int)] = Source.fromFile("src/main/resources/engine_schematic.txt").getLines().toList.zipWithIndex

  private val gearLocations: List[GearLocations] = schematicLines
    .flatMap(e => getGearLocations(e._1, e._2))

  private val eligibleGears: List[List[Int]] = gearLocations
    .map(b => getListOfGearAdjacentNumbers(schematicLines, b))

  private val filteredGears: List[List[Int]] = eligibleGears.filter(_.size == 2)

  private val multipliedGears: List[Int] = filteredGears.map(gearNumberList => {
    gearNumberList.head * gearNumberList.last
  })

  println(multipliedGears.sum)


  /*  private val extractedNumberLocations: List[Int] =
      schematicLines
        .flatMap(e => {
        extractNumberLocations(e._1, e._2)
      })
        .filter(f => {
        checkIfNumberEligible(schematicLines, f)
      })
        .map(_.number)


    println(extractedNumberLocations.sum)*/

  private def getGearLocations(schematicLine: String, lineNumber: Int): List[GearLocations] = {
    val pattern = "\\*".r
    val gears = pattern.findAllIn(schematicLine).toList
    var globalIndex: Int = 0
    val gearsList: List[GearLocations] = gears.map(e => {
      val startingIndex = schematicLine.indexOf(e, globalIndex)
      globalIndex = Math.max(globalIndex, startingIndex) + 1
      GearLocations(startingIndex, lineNumber)
    })

    gearsList
  }

  private def extractNumberLocations(schematicLine: String, lineNumber: Int): List[NumberLocations] = {
    val pattern = "\\d+".r
    val numbers = pattern.findAllIn(schematicLine).toList
    var globalIndex: Int = 0
    val numLocList = numbers.map(e => {
      val number = e.toInt
      val startingIndex = schematicLine.indexOf(e, globalIndex)
      globalIndex = Math.max(globalIndex, startingIndex) + number.toString.length
      NumberLocations(number, startingIndex, lineNumber)
    })

    numLocList
  }

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
    println("LINE NUMBER: " + lineNumber + " AND Returned " + searchResult + " for the number: " + number.number)
    searchResult.exists(_.self == true)
  }

  private def getListOfGearAdjacentNumbers(schematicLines: List[(String, Int)], gear: GearLocations): List[Int] = {
    val lineNumber = gear.lineNumber
    val gearStartIndex = gear.location
    val gearLengh = 1

    val searchRange = Math.max(0, gearStartIndex - 1) to Math.min(gearStartIndex + gearLengh, schematicLines.head._1.length - 1)
    val searchSpace: List[(String, Int)] = schematicLines.filter(e => e._2 == lineNumber || e._2 == lineNumber - 1 || e._2 == lineNumber + 1)

    val searchResult: List[Int] = searchSpace.flatMap(e => {
      getGearNumber(e._1, searchRange)
    }).filter(_ != 0)

    searchResult
  }

  private def getGearNumber(searchStr: String, searchRange: Seq[Int]): Seq[Int] = {
    var counter = 0
    searchRange.map(index => {
      val isPrevCandidateCharDigit = searchStr.charAt(index - 1).isDigit
      val candidateChar = searchStr.charAt(index)
      val digitEquality = candidateChar.isDigit
      if (digitEquality) {
        val num = getNumber(searchStr, index)
        if (counter != 0 && isPrevCandidateCharDigit) {
          counter += 1
          0
        } else {
          counter +=1
          num
        }
      } else {
        0
      }
    })
  }

  private def getNumber(str: String, strIndex: Int) = {
    val pattern = "\\d+".r

    val lastChar = if(str.take(strIndex).last.equals('*')) "*" else "."

    val candidateRange = str.substring(Math.max(0, str.take(strIndex).lastIndexOf(lastChar) + 1))
    val number = pattern.findFirstIn(candidateRange).get.toInt

    number

  }

  private def checkTheStringRange(searchStr: String, searchRange: Seq[Int]): Boolean = {
    val resultCalc = searchRange.forall(index => {
      val candidateChar: Char = searchStr.charAt(index)
      val dotEquality = candidateChar.compare('.') == 0
      val digitEquality = candidateChar.isDigit
      val res = dotEquality || digitEquality
      res
    })

    resultCalc
  }

}
