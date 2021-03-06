package com.generalprocessingunit.music

import scala.collection.mutable

trait Notes extends Sequence {
  def toDeltaSequence = DeltaSequence(name, getDeltaSequence(this))

  // TODO: this might be inappropriate and unnecessary
  def toScale: Scale = {
    val s = toDeltaSequence.sequence
    Scale(name, s :+ (12 - (s reduce (_ + _))))
  }

  def restrictToOneOctave: Sequence with Notes = {
    NoteSequence(name, sequence map (n => (n - sequence.head) % 12 + sequence.head))
  }


  /**
   * traverse a note sequence moving to a new index by steps of a delta sequence. useful for creating scale excercises
   * @param startingIndex defaults to 0
   * @param deltaSequence
   * @return
   */
  def traverseByDeltaSequence(startingIndex: Int = 0, deltaSequence: Sequence with Deltas): Sequence with Notes = {
    //TODO: require the sequence stays in bounds of the Note Sequence or option to loop over
    var i = startingIndex
    val seq = Vector(sequence(i)) ++
      (deltaSequence.sequence map (d => {
        i += d
        sequence(i)
      }))
    //TODO: map with accumulator instead of side-effect?

    //TODO: match by trait to generate either a NoteSequence or a RhythmicNoteSequence?
    NoteSequence(name + "TraversedBy" + deltaSequence.name, seq)
  }


  protected def getDeltaSequence(noteSequence: Sequence with Notes): Vector[Int] = {
    require(noteSequence.sequence.length > 1, "Sequence length must be greater than 1")
    val deltaList = mutable.MutableList[Int]()
    val seq = noteSequence.sequence
    for ((a, b) <- seq.slice(0, seq.length - 1) zip seq.slice(1, seq.length))
      deltaList += b - a
    deltaList.toVector
  }
}