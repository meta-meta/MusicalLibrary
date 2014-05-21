import scala.collection.mutable

case class NoteSequence(name: String, sequence: Vector[Int], rhythm: Vector[Float]) {
  def this(name: String, sequence: Vector[Int]) = this(name, sequence, sequence.map(_ => 1f))
  def toDeltaSequence = NoteSequence.getDeltaSequence(this)
}

object NoteSequence {
  implicit def nToVectorInt(n: NoteSequence) = n.sequence

  private def getDeltaSequence(noteSequence: NoteSequence): DeltaSequence = {
    require(noteSequence.sequence.length > 1)
    val deltaList = mutable.MutableList[Int]()
    val seq = noteSequence.sequence
    for ((a, b) <- seq.slice(0, seq.length - 1) zip seq.slice(1, seq.length) )
      deltaList += b - a
    DeltaSequence(noteSequence.name, deltaList.toVector, noteSequence.rhythm)
  }
}