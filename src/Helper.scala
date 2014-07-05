trait Helper {
  def vectorOfDeltaSequencesModulatedByDeltaSequence(firstNote: Int, sequences: Vector[Sequence with Deltas], modulator: Sequence with Deltas): Map[String, Vector[Sequence with Notes]] = {
    val tuples = sequences map (s => (s.name + " by " + modulator.name, s.modulateByDeltaSequence(firstNote, modulator)))
    tuples.toMap
  }

  val W = 1f
  val H = 0.5f
  val Q = 0.25f
  val E = 0.125f
  val S = 0.0625f

  protected def tuplet(n: Int, d: Float) = (d * 2) / n

  val Q3 = tuplet(3, Q)
  val E3 = tuplet(3, E)

}
