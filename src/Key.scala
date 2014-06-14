/**
 * A Key is an ordered(for now) set of notes over the full range of notes that fall in the pitch classes contained in generatingSequence.
 * @param generatingSequence
 */
case class Key(generatingSequence: Sequence with Notes) extends Sequence with Notes {
  val name = generatingSequence.name
  val sequence = MusicalLibrary.AllNotes
    .filter(n => generatingSequence.contains(n % 12))
    .toVector

  override def traverseByDeltaSequence(startingNote: Int = 0, deltaSequence: Sequence with Deltas): Sequence with Notes = {
    require(sequence contains startingNote)
    super.traverseByDeltaSequence(sequence.indexOf(startingNote), deltaSequence)
  }

  def range(range: Range): Sequence with Notes = {
    NoteSequence(name, sequence.filter(n => range contains n))
  }
}
