package uk.co.chrisloy.crossword

import scala.io.Source._
import scala.util.Random

object Thesaurus {
  
  private lazy val thesaurus = {
    val source = fromInputStream(getClass getResourceAsStream "/thesaurus.txt").mkString
    val rows: List[String] = Random.shuffle((source split "\n").toList)
    val words = rows map (_.toUpperCase split ",") map (words => (words.head, words.tail.toList))
    new Thesaurus(words: _*)
  }
  
  def apply() = thesaurus
}

class Thesaurus(val words: Map[String, List[String]]) {
  
  def this(words: (String, List[String]) *) = this(Map(words: _*))
  
  def apply(word: String) = words.get(word.toUpperCase)
  
  def random(f: (String => Boolean)) = words find { case (word, list) => f(word) }
}