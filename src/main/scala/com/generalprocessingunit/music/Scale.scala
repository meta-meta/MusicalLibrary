package com.generalprocessingunit.music

/**
 * A Scale is defined as strictly an ascending DeltaSequence whose deltas total an octave
 * @param name
 * @param sequence
 */
case class Scale(name: String, sequence: Vector[Int]) extends Sequence with Deltas {
  require(sequence.reduce(_ + _) == 12, "DeltaSequence of Scale must traverse exactly one octave")
  require(sequence.filter(_ < 1).isEmpty, "DeltaSequence of Scale must be ascending")

  override def toNoteSequence(firstNote: Int): NoteSequence = NoteSequence(name, getNoteSequence(firstNote).dropRight(1) )

  def toRangeExercise(firstNote: Int, range: Range = MusicalLibrary.AllNotes, asc: Boolean = true): NoteSequence = {
    val n = toKey(firstNote).range(range)
    NoteSequence(name + " Range " + (if(asc) "Ascending" else "Descending"),
      generateRangeExercise(firstNote, if(asc) n else n.reverse)
    )
  }

  /**
   *
   * @param firstNote - the "tonic"
   * @return a Key consisting of pitch classes contained in this Scale
   */
  def toKey(firstNote: Int = 0): Key = {
    val scaleNotes = getNoteSequence(firstNote)
      .dropRight(1) // last note in getNoteSequence is an octave up. that note is redundant in this case so drop it
      .map(_ % 12)

    Key(NoteSequence("KeyOf" + (firstNote % 12) + name, scaleNotes))
  }

  private def generateRangeExercise(firstNote: Int, notes: Vector[Int]) = {
    notes.slice(notes.indexOf(firstNote), notes.length) ++
      notes.dropRight(1).reverse ++
      notes.slice(1, notes.indexOf(firstNote) + 1)
  }

  def toRangeExerciseByDeltaSequence(firstNote: Int, deltaSequence: Sequence with Deltas, range: Range = MusicalLibrary.AllNotes, asc: Boolean = true): Vector[Sequence with Notes] = {
    generateRangeExerciseModulatedByDeltaSequence(firstNote, deltaSequence, range, asc)
  }

  private def generateRangeExerciseModulatedByDeltaSequence(firstNote: Int, deltaSequence: Sequence with Deltas, range: Range = MusicalLibrary.AllNotes, asc: Boolean): Vector[Sequence with Notes] = {
    modulateByDeltaSequence(firstNote, deltaSequence)
      .map(seq => Scale(seq.name, seq.toDeltaSequence.sequence).toRangeExercise(seq.sequence.head, range, asc))
  }
}