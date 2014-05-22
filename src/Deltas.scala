import scala.collection.mutable

trait Deltas extends Sequence {
  def toNoteSequence(firstNote: Int) = NoteSequence(name, getNoteSequence(firstNote))

  def modulateByNoteSequence(noteSequence: Sequence with Notes): Vector[Sequence with Notes] = {
    noteSequence map (n => toNoteSequence(n))
  }

  protected def getNoteSequence(firstNote: Int): Vector[Int] = {
    val noteList = mutable.MutableList(firstNote)
    for (d <- sequence)
      noteList += (noteList.last + d)
    noteList.toVector
  }
}
