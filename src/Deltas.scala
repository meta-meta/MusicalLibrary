import scala.collection.mutable

trait Deltas extends Sequence {
  def toNoteSequence(firstNote: Int) = NoteSequence(name, getNoteSequence(firstNote))

  def modulateByNoteSequence(noteSequence: Sequence with Notes): Vector[NoteSequence] = {
    noteSequence map (n => toNoteSequence(n))
  }

  def modulateByDeltaSequence(firstNote: Int, deltaSequence: Sequence with Deltas, restrictModulatorToOneOctave: Boolean = true): Vector[NoteSequence] = {
    val n = deltaSequence.toNoteSequence(firstNote)
    val modulator = if (restrictModulatorToOneOctave) n.restrictToOneOctave else n
    modulateByNoteSequence(modulator)
  }

  protected def getNoteSequence(firstNote: Int): Vector[Int] = {
    val noteList = mutable.MutableList(firstNote)
    for (d <- sequence)
      noteList += (noteList.last + d)
    noteList.toVector
  }
}
