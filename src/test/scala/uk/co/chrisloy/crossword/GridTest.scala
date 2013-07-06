package uk.co.chrisloy.crossword

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

class GridTest extends FlatSpec with ShouldMatchers {
  
  val data: Map[(Int, Int), Char] = Map.empty
  
  "A grid" should "print correctly" in {
    new Grid(Map((3, 4) -> 'F')).toString should equal ("F")
  }
  
  it should "create with one word across" in {
    val grid = Grid.toGrid("HELLO", 4, 5, Across)
    grid(4, 5) should equal ('H')
    grid(5, 5) should equal ('E')
    grid(6, 5) should equal ('L')
    grid(7, 5) should equal ('L')
    grid(8, 5) should equal ('O')
  }
  
  it should "create one word down" in {
    val grid = Grid.toGrid("NEPTUNE", 5, 4, Down)
    grid(5, 4 ) should equal ('N')
    grid(5, 5 ) should equal ('E')
    grid(5, 6 ) should equal ('P')
    grid(5, 7 ) should equal ('T')
    grid(5, 8 ) should equal ('U')
    grid(5, 9 ) should equal ('N')
    grid(5, 10) should equal ('E')
  }
  
  it should "add a valid new word" in {
    val grid = Grid.toGrid("BIN", 0, 0, Across)
    val thes = new Thesaurus("IGLOO" -> Nil)
    val newGrid = grid.grow(thes, Down)
    println(newGrid)
    newGrid(0, 0) should equal ('B')
    newGrid(1, 0) should equal ('I')
    newGrid(2, 0) should equal ('N')
    newGrid(1, 1) should equal ('G')
    newGrid(1, 2) should equal ('L')
    newGrid(1, 3) should equal ('O')
    newGrid(1, 4) should equal ('O')
  }
}