package org.guzeloglu.application

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object ScratchCards extends App {
  private val scratchCards = Source.fromFile("src/main/resources/scratchcards.txt").getLines().toList

  val resultingPoint = scratchCards.zipWithIndex.map(card => {
    val pattern = "\\d+".r
    val numbers: List[String] = card._1.split("\\|").toList
    val pickedNumbersStr: String = numbers.last.trim
    val winningNumbersStr: String = numbers.head.split(":")(1).trim

    val winningNumbers: List[String] = pattern.findAllMatchIn(winningNumbersStr).toList.map(_.toString)
    val pickedNumbers: List[String] = pattern.findAllMatchIn(pickedNumbersStr).toList.map(_.toString)

    val numOfCommonValues = winningNumbers.intersect(pickedNumbers).length
    Math.pow(2, numOfCommonValues - 1).toInt
  })

  //println(resultingPoint.sum)
  case class ScratchCardCollection(card_id: Int, num_of_winners: Int, copies: List[Int])

  private val totalNumOfCards = scratchCards.size
  private val scratchCardCollection: List[ScratchCardCollection] = scratchCards.zipWithIndex.map(card => {
    val pattern = "\\d+".r
    val numbers: List[String] = card._1.split("\\|").toList
    val pickedNumbersStr: String = numbers.last.trim
    val winningNumbersStr: String = numbers.head.split(":")(1).trim

    val winningNumbers: List[String] = pattern.findAllMatchIn(winningNumbersStr).toList.map(_.toString)
    val pickedNumbers: List[String] = pattern.findAllMatchIn(pickedNumbersStr).toList.map(_.toString)

    val numOfCommonValues = winningNumbers.intersect(pickedNumbers).length
    val copiesList = 2 + card._2 to Math.min(totalNumOfCards, card._2 + numOfCommonValues + 1)
    val collection = ScratchCardCollection(card._2 + 1, numOfCommonValues, copiesList.toList)
    //println("Card " + (card._2 + 1) + " has " + numOfCommonValues + " match(es).")
    //println(collection.toString)
    collection
  })

  private val scratchCardMap = scratchCardCollection.map(e => (e.card_id, e.copies)).toMap
  println(scratchCardMap)
  import scala.collection.mutable._

  private var q1: mutable.Queue[Int] = Queue.empty

  (1 to scratchCardCollection.length).foreach(e => q1.enqueue(e))
  var counter = 0

  while (q1.nonEmpty) {
    val head = q1.head
    q1.dequeue()
    counter += 1
    val copies = scratchCardMap
      .getOrElse(head, List.empty)
    copies
      .foreach(copy => {
        q1.enqueue(copy)
      })

  }

  println(counter)
}
