import scala.collection.mutable

case class DeltaSequence(name: String, sequence: Vector[Int], rhythm: Vector[Float]) {
  def this(name: String, sequence: Vector[Int]) = this(name, sequence, sequence.map(_ => 1f))

  def toNoteSequence(firstNote: Int) = NoteSequence(name, DeltaSequence.getNoteSequence(firstNote, this), rhythm)

  def toRangeExerciseAsc(firstNote: Int, range: Range = MusicalLibrary.allNotes) = {
    DeltaSequence.getRangeExercise(firstNote, DeltaSequence.getNotesOfScaleInRange(firstNote, this, range))
  }

  def toRangeExerciseDesc(firstNote: Int, range: Range = MusicalLibrary.allNotes) = {
    DeltaSequence.getRangeExercise(firstNote, DeltaSequence.getNotesOfScaleInRange(firstNote, this, range).reverse)
  }
}

object DeltaSequence {
  implicit def dToVectorInt(d: DeltaSequence) = d.sequence

  private def getNoteSequence(firstNote: Int, deltaSequence: DeltaSequence): Vector[Int] = {
    val noteList = mutable.MutableList(firstNote)
    for (d <- deltaSequence.sequence)
      noteList += (noteList.last + d)
    noteList.toVector
  }

  private def getNotesOfScaleInRange(firstNote: Int, deltaSequence: DeltaSequence, range: Range): Vector[Int] = {
    // TODO: constraints like this should be handled by a more nuanced set of types
    // TODO: a Scale can be defined as a special case of DeltaSequence
    require(deltaSequence.sequence.reduce(_ + _) == 12)
    require(deltaSequence.sequence.filter(_ < 1).isEmpty)

    val scaleNotes = getNoteSequence(firstNote, deltaSequence)
      .dropRight(1)
      .map(_ % 12)

    range
      .filter (n => scaleNotes.contains(n % 12))
      .toVector
  }

  private def getRangeExercise(firstNote: Int, notes: Vector[Int]) = {
    notes.slice(notes.indexOf(firstNote), notes.length) ++
    notes.dropRight(1).reverse ++
    notes.slice(1, notes.indexOf(firstNote) + 1)
  }

}