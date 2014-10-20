package com.generalprocessingunit.music

import com.generalprocessingunit.music.Accidental.Accidental
import com.generalprocessingunit.music.NoteLetter.NoteLetter

case class NoteName(letter: NoteLetter, octave: Integer, accidental: Accidental) {}

object NoteName {
  val default = NoteName(NoteLetter.C, 0, Accidental.Natural)
}

object NoteLetter extends Enumeration {
  type NoteLetter = Value
  val A, B, C, D, E, F, G = Value

  def next(n: NoteLetter) = NoteLetter((n.id + 1) % 7)

  def prev(n: NoteLetter) = NoteLetter((7 + n.id - 1) % 7)
}

object Accidental extends Enumeration {
  type Accidental = Value
  val Natural, Flat, Sharp = Value
}