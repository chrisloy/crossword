package uk.co.chrisloy.crossword


object Grid {
  
  def apply() = toGrid("infinite", -3, 0, Across)
  
  def toGrid(word: String, x: Int, y: Int, dir: Direction) = new Grid({
    for (i <- 0 until word.length)
      yield
        (if (dir == Across)
          (x + i, y)
        else
          (x, y + i)) -> word(i).toUpper
  }.toMap)
}

class Grid(val chars: Map[(Int, Int), Char]) {
  
  type Square = (Option[Char], Int, Int)
  type Row = IndexedSeq[Square]
  
  val (minX, maxX, minY, maxY) = {
    val xs = chars map (_._1._1)
    val ys = chars map (_._1._2)
    (xs.min, xs.max, ys.min, ys.max)
  }
  
  val rows: List[Row] = {
    (minY to maxY).toList map (y => for (x <- (minX to maxX)) yield (chars.get(x, y), x, y))
  }
  
  val cols: List[Row] = {
    (minX to maxX).toList map (x => for (y <- (minY to maxY)) yield (chars.get(x, y), x, y))
  }
  
  def add(word: String, x: Int, y: Int, dir: Direction) = {
    new Grid(chars ++ Grid.toGrid(word, x, y, dir).chars)
  }
  
  private def toRegex(seq: Row) = ".*" + seq.map(_._1 getOrElse '.').mkString + ".*"
  
  def grow(thesaurus: Thesaurus, dir: Direction) = {
    def findPosition(word: String, row: Row): Int = {
      0
    }
    
    def getWord(rows: List[Row], rowPos:Int): Option[(String, Int, Int)] = {
      rows match {
        case Nil => None
        case head::tail => {
          thesaurus random (_ matches toRegex(head)) match {
            case None => getWord(tail, rowPos + 1)
            case Some((word, synonyms)) => Some(word, rowPos, findPosition(word, head))
          }
        }
      }
    }
    (if (dir == Across) getWord(rows.toList, minY) else getWord(cols.toList, minX)) match {
      case None => throw new NoSuchElementException
      case Some((word, row, index)) => add(word, row, index, dir)
    }
  }
  
  def apply(x: Int, y:Int) = chars(x, y)
  
  override def toString = {
    rows map (_ map (_._1 getOrElse "_") mkString " ") mkString "\n"
  }
}