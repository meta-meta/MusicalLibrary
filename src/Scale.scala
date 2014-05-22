/**
 * A Scale is defined as strictly an ascending DeltaSequence whose deltas total an octave
 * @param name
 * @param sequence
 */
case class Scale(name: String, sequence: Vector[Int]) extends Sequence with Deltas {
  require(sequence.reduce(_ + _) == 12, "DeltaSequence of Scale must traverse exactly one octave")
  require(sequence.filter(_ < 1).isEmpty, "DeltaSequence of Scale must be ascending")

  def toRangeExerciseAsc(firstNote: Int, range: Range = MusicalLibrary.allNotes): NoteSequence = {
    NoteSequence(name + " Range Ascending",
      generateRangeExercise(firstNote, getNotesInRange(firstNote, range))
    )
  }

  def toRangeExerciseDesc(firstNote: Int, range: Range = MusicalLibrary.allNotes): NoteSequence = {
    NoteSequence(name + " Range Descending",
      generateRangeExercise(firstNote, getNotesInRange(firstNote, range).reverse)
    )
  }

  private def getNotesInRange(firstNote: Int, range: Range): Vector[Int] = {
    val scaleNotes = getNoteSequence(firstNote)
      .dropRight(1)
      .map(_ % 12)

    range
      .filter(n => scaleNotes.contains(n % 12))
      .toVector
  }

  private def generateRangeExercise(firstNote: Int, notes: Vector[Int]) = {
    notes.slice(notes.indexOf(firstNote), notes.length) ++
      notes.dropRight(1).reverse ++
      notes.slice(1, notes.indexOf(firstNote) + 1)
  }
}