object MusicalLibrary {

  def allNotes = 0 until 128

  def churchModes: Vector[Scale] = {
    def majorScale = Vector(2, 2, 1, 2, 2, 2, 1)
    for ((name, i) <- Vector("Ionian", "Dorian", "Phrygian", "Lydian", "Mixolydian", "Aeolian", "Locrian").zipWithIndex)
      yield Scale(name, majorScale.slice(i, majorScale.length) ++ majorScale.slice(0, i))
  }

  def arpeggios: Vector[Scale] =
    Scale("Major",            Vector(4, 3, 5))    +:
    Scale("Minor",            Vector(3, 4, 5))    +:
    Scale("Diminished",       Vector(3, 3, 6))    +:
    Scale("Augmented",        Vector(4, 4, 4))    +:
    Scale("Major 7",          Vector(4, 3, 4, 1)) +:
    Scale("Minor 7",          Vector(3, 4, 3, 2)) +:
    Scale("Dominant 7",       Vector(4, 3, 3, 2)) +:
    Scale("Minor Major 7",    Vector(3, 4, 4, 1)) +:
    Scale("Diminished 7",     Vector(3, 3, 3, 3)) +:
    Scale("Half-diminished",  Vector(3, 3, 4, 2)) +:
    Scale("Augmented 7",      Vector(4, 4, 2, 2)) +:
    Vector()

  def melodies: Vector[RhythmicDeltaSequence] =
    RhythmicDeltaSequence("Home Alone",
      Vector(-3, 3, -3, 8, -5, -5, 7, -2, -7, 5, -1, -2, 5, -3, 3, -3, 8, -5, 2, 2, 1, -5, 2, 2, 1, -5, 2, 2, 1, -5, -7, 5, -1, -2, -2 ),
      Vector(2f, 2f, 2f, 2f, 4f, 4f, 2f, 2f, 2f, 1f, 1f, 4f, 4f, 2f, 2f, 2f, 2f, 4f, 6f, 0.5f, 0.5f, 1f, 6f, 0.5f, 0.5f, 1f, 6f, 0.5f, 0.5f, 1f, 2f, 1f, 1f, 4f, 4f, 8f)
    ) +:
    RhythmicDeltaSequence("Jurassic Park",
      Vector(-1, 1, -5, -2, 7, -1, 1, -5, -2, 7, -1, 1, 2, 0, 3, 0),
      Vector(1f, 1f, 2f, 2f, 2f, 1f, 1f, 2f, 2f, 2f, 1f, 1f, 3f, 1f, 3f, 1f, 3f)
    ) +:
    RhythmicDeltaSequence("Close Encounters",
      Vector(2, -4, -12, 7),
      Vector(1f, 1f, 1f, 1f, 4f)
    ) +:
    RhythmicDeltaSequence("Mozart",
      Vector(-5, 5, -5, 5, -5, 5, 4, 3, -2, -3, 3, -3, 3, -3, -3, 3, -7),
      Vector(1.5f, 0.5f, 1.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 2f, 1.5f, 0.5f, 1.5f, 0.5f, 0.5f, 0.5f, 0.5f, 0.5f, 2f)
    ) +:
    Vector()



  def circleOfFifths = DeltaSequence("Circle Of Fifths", 1 to 12 map (_ => 7) toVector)

  def modulateDeltaSequenceByCircleOfFifths(startingNote: Int, deltaSequence: Sequence with Deltas): Vector[Sequence with Notes] = {
    val modulator = circleOfFifths.toNoteSequence(startingNote).restrictToOneOctave
    deltaSequence.modulateByNoteSequence(modulator)
  }

  def vectorOfDeltaSequencesByCircleOfFifths(startingNote: Int, sequences: Vector[Sequence with Deltas]): Map[String, Vector[Sequence with Notes]] = {
    val tuples = sequences map (s => {
      (s.name + " by fifths", modulateDeltaSequenceByCircleOfFifths(startingNote, s))
    })
    tuples.toMap
  }

  def rangeExerciseAscByCircleOfFifths(startingNote: Int, scale: Scale, range: Range = allNotes): Vector[Sequence with Notes] = {
    rangeExerciseByCircleOfFifths(startingNote, scale, range, asc = true)
  }

  def rangeExerciseDescByCircleOfFifths(startingNote: Int, scale: Scale, range: Range = allNotes): Vector[Sequence with Notes] = {
    rangeExerciseByCircleOfFifths(startingNote, scale, range, asc = false)
  }

  private def rangeExerciseByCircleOfFifths(startingNote: Int, scale: Scale, range: Range = allNotes, asc: Boolean): Vector[Sequence with Notes] = {
    modulateDeltaSequenceByCircleOfFifths(startingNote, scale)
      .map(seq => {
        if(asc) Scale(seq.name, seq.toDeltaSequence.sequence).toRangeExerciseAsc(seq.sequence.head, range)
        else Scale(seq.name, seq.toDeltaSequence.sequence).toRangeExerciseDesc(seq.sequence.head, range)
      })
  }

  def churchModesByCircleOfFifths(startingNote: Int) = vectorOfDeltaSequencesByCircleOfFifths(startingNote, churchModes)

  def arpeggiosByCircleOfFifths(startingNote: Int) = vectorOfDeltaSequencesByCircleOfFifths(startingNote, arpeggios)

}
