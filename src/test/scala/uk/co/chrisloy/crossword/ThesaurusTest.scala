package uk.co.chrisloy.crossword

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ThesaurusTest extends FlatSpec with ShouldMatchers {
  
  private val words = Map(
    "TIRED" -> List("SLEEPY", "EXHAUSTED", "KNACKERED"),
    "CONFUSED" -> List("BAFFLED", "FLUMMOXED", "BEFUDDLED"),
    "TORN" -> List("RIPPED")
  )
  
  "A thesaurus" should "load correctly" in {
    val thesaurus = new Thesaurus(words)
    thesaurus.words.size should equal (3)
    thesaurus("TIRED").get should equal ("SLEEPY" :: "EXHAUSTED" :: "KNACKERED" :: Nil)
  }
  
  it should "be able to find any word" in {
    val thesaurus = new Thesaurus(words)
    thesaurus.random(_ != null).isDefined should be (true)
  }
  
  it should "be able to find a specific word" in {
    val thesaurus = new Thesaurus(words)
    thesaurus.random(_ == "TIRED").isDefined should be (true)
    thesaurus.random(_ == "TIRED").get._1 should be ("TIRED")
    thesaurus.random(_ == "TIRED").get._2 should be ("SLEEPY" :: "EXHAUSTED" :: "KNACKERED" :: Nil)
  }
  
  it should "be able to find a word" in {
    val thesaurus = new Thesaurus(words)
    thesaurus.random(_ matches ".*RE.*").isDefined should be (true)
    thesaurus.random(_ matches ".*RE.*").get._1 should be ("TIRED")
    thesaurus.random(_ matches ".*RE.*").get._2 should be ("SLEEPY" :: "EXHAUSTED" :: "KNACKERED" :: Nil)
  }
  
  "The default thesaurus" should "be loaded from disk" in {
    val thesaurus = Thesaurus()
    thesaurus.words.size should equal (30195)
  }
  
  it should "not be loaded multiple times" in {
    System.identityHashCode(Thesaurus()) should equal (System.identityHashCode(Thesaurus()))
  }
}