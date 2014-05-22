import scala.collection.mutable

trait Notes extends Sequence {
  def toDeltaSequence = DeltaSequence(name, getDeltaSequence(this))

  def restrictToOneOctave: Sequence with Notes = {
    NoteSequence(name, sequence map (n => n % 12 + sequence.head))
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
