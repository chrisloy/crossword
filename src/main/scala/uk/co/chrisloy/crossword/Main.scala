package uk.co.chrisloy.crossword

object Main {
  
  def main(args: Array[String]): Unit = {
    println(Grid() add ("enchilada", 1, -1, Down) add ("tricksy", -3, -2, Down))
  }
  
  def show(word: String) = {
    val synonyms = Thesaurus()(word)
    println("Word:     " + word.toUpperCase)
    println("Synonyms: " + (synonyms getOrElse Nil))
    println
  }
}
