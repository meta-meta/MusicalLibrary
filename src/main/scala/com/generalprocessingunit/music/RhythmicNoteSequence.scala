package com.generalprocessingunit.music

case class RhythmicNoteSequence(name: String, sequence: Vector[Int], rhythm: Vector[Float])
  extends Sequence with Rhythmic with Notes {
  require(rhythm.length == sequence.length, "Rhythm length must equal sequence length")
}