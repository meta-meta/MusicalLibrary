object MusicalLibrary extends Helper {

  final val AllNotes = 0 until 128

  final val Chromatic = Scale(Names.Chromatic, 1 to 12 map (_ => 1) toVector)

  final val Modes: Vector[Scale] = {
    val majorScale = Vector(2, 2, 1, 2, 2, 2, 1)
    for ((name, i) <- Names.WesternModesMajorScaleOrder.zipWithIndex)
      yield Scale(name, majorScale.slice(i, majorScale.length) ++ majorScale.slice(0, i))
  }

  final val Arpeggios: Vector[Scale] =
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

  final val Melodies: Vector[RhythmicDeltaSequence] =
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

  final val CircleOfFifths = DeltaSequence(Names.CircleOfFifths, 1 until 12 map (_ => 7) toVector)

  def churchModesByCircleOfFifths(firstNote: Int) = vectorOfDeltaSequencesModulatedByDeltaSequence(firstNote, Modes, CircleOfFifths)

  def arpeggiosByCircleOfFifths(firstNote: Int) = vectorOfDeltaSequencesModulatedByDeltaSequence(firstNote, Arpeggios, CircleOfFifths)


  def hanon(ascTemplate: Vector[Int], ascRepetitions: Int, transition1: Int,
            descTemplate: Vector[Int], descRepetitions: Int, transition2: Int): Sequence with Notes = {
    def deltas: Vector[Int] = {
      def repeatTemplate(rep: Int, t: Vector[Int]) = (1 to rep flatMap (n => t)).dropRight(1).toVector
      val h1Asc = repeatTemplate(ascRepetitions, ascTemplate)
      val h1Desc = repeatTemplate(descRepetitions, descTemplate)
      (h1Asc :+ transition1) ++ (h1Desc :+ transition2)
    }

    val notes = Modes(0).toKeyOverRange(48, 47 to 79)
    NoteSequence("HanonNotes", notes).traverseByDeltaSequence(1, DeltaSequence("HanonDeltas", deltas))
  }

  final val Hanon1: Sequence with Notes = {
    val h1T = Vector(2, 1, 1, 1, -1, -1, -1, -1)

    hanon(
      h1T, 14, 3,
      DeltaSequence("", h1T).negate, 15, -2
    )
  }

  final val Hanon2: Sequence with Notes = {
    hanon(
      Vector(2, 3, -1, -1, 1, -1, -1, -1), 14, 3,
      Vector(-3, -2, 1, 1, -1, 1, 1, 1), 14, -3
    )
  }

}
