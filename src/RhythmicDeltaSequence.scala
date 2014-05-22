case class RhythmicDeltaSequence(name: String, sequence: Vector[Int], rhythm: Vector[Float])
  extends Sequence with Rhythmic with Deltas {
  require(rhythm.length == sequence.length + 1, "Rhythm length must be 1 less than sequence length")
}
