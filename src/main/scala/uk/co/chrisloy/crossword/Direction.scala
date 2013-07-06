package uk.co.chrisloy.crossword

sealed trait Direction

case object Across extends Direction
case object Down extends Direction