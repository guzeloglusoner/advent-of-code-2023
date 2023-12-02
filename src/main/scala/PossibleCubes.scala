package org.guzeloglu.application

import scala.io.Source

object PossibleCubes extends App {

  private val games = Source.fromFile("src/main/resources/possible_cubes.txt").getLines().toList
  private val possibleGames = games.filter(isPossible)
  private val result: Int = possibleGames
    .map(e => {
      val endIndex = e.indexOf(":")
      val beginningIndex = e.indexOf(" ")
      e.substring(beginningIndex + 1, endIndex).toInt
  })
    .sum

  println(result)
  private def isPossible(str: String): Boolean = {
    val semicolonIndex = str.indexOf(":") + 1
    val setOfCubes: List[String] = str
      .substring(semicolonIndex)
      .split(";")
      .toList

    setOfCubes.forall(c => {
      checkPossibility(c)
    })

  }

  private def checkPossibility(str: String): Boolean = {
    val maxRedCubes = 12
    val maxGreenCubes = 13
    val maxBlueCubes = 14
    val numOfRedCubes: Int = extractValue(str,"red")
    val numOfBlueCubes: Int = extractValue(str,"blue")
    val numOfGreenCubes: Int = extractValue(str,"green")

    if(maxRedCubes < numOfRedCubes
      || maxBlueCubes < numOfBlueCubes
      || maxGreenCubes < numOfGreenCubes){
      println(str + " this set is not possible")
      false
    }else
      println(str + " this set is possible")
      true
  }

  private def extractValue(set: String, color: String): Int = {
    set
      .split(",")
      .toList
      .filter(_.contains(color))
      .map(_.replaceAll("\\D", "").toInt)
      .headOption
      .getOrElse(0)
  }
}
