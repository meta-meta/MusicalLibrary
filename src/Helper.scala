trait Helper {
  def vectorOfDeltaSequencesModulatedByDeltaSequence(firstNote: Int, sequences: Vector[Sequence with Deltas], modulator: Sequence with Deltas): Map[String, Vector[Sequence with Notes]] = {
    val tuples = sequences map (s => (s.name + " by " + modulator.name, s.modulateByDeltaSequence(firstNote, modulator)))
    tuples.toMap
  }
}
