object MusicalLibrary {

  def allNotes = 0 until 128

  def churchModes: Vector[DeltaSequence] = {
    def majorScale = Vector(2, 2, 1, 2, 2, 2, 1)
    for ((name, i) <- Vector("Ionian", "Dorian", "Phrygian", "Lydian", "Mixolydian", "Aeolian", "Locrian").zipWithIndex)
      yield new DeltaSequence(name, majorScale.slice(i, majorScale.length) ++ majorScale.slice(0, i))
  }


  def arpeggios: Vector[DeltaSequence] =
    new DeltaSequence("Major",            Vector(4, 3, 5))    +:
    new DeltaSequence("Minor",            Vector(3, 4, 5))    +:
    new DeltaSequence("Diminished",       Vector(3, 3, 6))    +:
    new DeltaSequence("Augmented",        Vector(4, 4, 4))    +:
    new DeltaSequence("Major 7",          Vector(4, 3, 4, 1)) +:
    new DeltaSequence("Minor 7",          Vector(3, 4, 3, 2)) +:
    new DeltaSequence("Dominant 7",       Vector(4, 3, 3, 2)) +:
    new DeltaSequence("Minor Major 7",    Vector(3, 4, 4, 1)) +:
    new DeltaSequence("Diminished 7",     Vector(3, 3, 3, 3)) +:
    new DeltaSequence("Half-diminished",  Vector(3, 3, 4, 2)) +:
    new DeltaSequence("Augmented 7",      Vector(4, 4, 2, 2)) +:
    Vector()

}
