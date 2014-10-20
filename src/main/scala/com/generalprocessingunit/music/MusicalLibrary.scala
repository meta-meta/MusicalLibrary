package com.generalprocessingunit.music

import com.generalprocessingunit.music.NoteName.{Accidental, NoteLetter}

object MusicalLibrary extends Helper with Hanon{

  final val AllNotes = 0 until 128

  final val Chromatic = Scale(Names.Chromatic, 1 to 12 map (_ => 1) toVector)

  final val Diatonic = Scale(Names.Diatonic, Vector(2, 2, 1, 2, 2, 2, 1))

  final val Pentatonic = Scale(Names.Pentatonic, Vector(3, 2, 3, 2, 2))

  final val WholeTone = Scale(Names.WholeTone, Vector(2, 2, 2, 2, 2, 2))

  final val Octatonic = Scale(Names.Octatonic, Vector(1, 2, 1, 2, 1, 2, 1, 2))

  final val KeyOfC  = Diatonic.toKey(0)
  final val KeyOfDb = Diatonic.toKey(1)
  final val KeyOfD  = Diatonic.toKey(2)
  final val KeyOfEb = Diatonic.toKey(3)
  final val KeyOfE  = Diatonic.toKey(4)
  final val KeyOfF  = Diatonic.toKey(5)
  final val KeyOfFs = Diatonic.toKey(6)
  final val KeyOfG  = Diatonic.toKey(7)
  final val KeyOfAb = Diatonic.toKey(8)
  final val KeyOfA  = Diatonic.toKey(9)
  final val KeyOfBb = Diatonic.toKey(10)
  final val KeyOfB  = Diatonic.toKey(11)

  final val Modes: Vector[Scale] = {
    for ((name, i) <- Names.WesternModesMajorScaleOrder.zipWithIndex)
      yield Scale(name, Diatonic.slice(i, Diatonic.length) ++ Diatonic.slice(0, i))
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
      Vector(.5f, .5f, 1f, 1f, 1f, .5f, .5f, 1f, 1f, 1f, .5f, .5f, 1.5f, .5f, 1.5f, .5f, 4f)
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


  final val Hanon1: Sequence with Notes = {
    val h1T = Vector(2, 1, 1, 1, -1, -1, -1, -1)

    hanon(Modes(0).toKey(), 48,
          h1T, 14, 3,
          DeltaSequence("", h1T).negate, 15, -2
    )
  }

  final val Hanon2: Sequence with Notes = {
    hanon(Modes(0).toKey(), 48,
          Vector(2, 3, -1, -1, 1, -1, -1, -1), 14, 3,
          Vector(-3, -2, 1, 1, -1, 1, 1, 1), 14, -3
    )
  }

  final val Hanon3: Sequence with Notes = {
    hanon(Modes(0).toKey(), 48,
          Vector(2, 3, -1, -1, -1, 1, 1, -3), 14, 1,
          Vector(-3, -2, 1, 1, 1, -1, -1, 3), 14, -1
    )
  }

  final val Hanon4: Sequence with Notes = {
    hanon(Modes(0).toKey(), 48,
          Vector(1, -1, 2, 3, -1, -1, -1, -1), 14, 3,
          Vector(-1, 1, -3, -2, 1, 1, 1, 1), 14, -3
    )
  }

  final val Hanon5: Sequence with Notes = {
    hanon(Modes(0).toKey(), 48,
          Vector(5, -1, 1, -2, 1, -2, 1, -2), 14, -2,
          Vector(1, -1, 2, -1, 2, -1, 2, -5), 14, -5
    )
  }

  final val Hanon6: Sequence with Notes = {
    hanon(Modes(0).toKey(), 48,
          Vector(5, -1, 1, -2, 2, -3, 3, -4), 14, 5,
          Vector(-5, 1, -1, 2, -2, 3, -3, 4), 14, -2 //negate
    )
  } //TODO: the final measure in this exercise varies from the rest

  final val Hanon7: Sequence with Notes = {
    hanon(Modes(0).toKey(), 48,
          Vector(2, -1, 2, -1, 2, -1, -1, -1), 14, 3,
          Vector(-2, 1, -2, 1, -2, 1, 1, 1), 14, -3 //negate
    )
  }

  final val Hanon8: Sequence with Notes = {
    hanon(Modes(0).toKey(), 48,
          Vector(2, 2, 1, -2, 1, -2, 1, -2), 14, 2,
          Vector(-2, -2, -1, 2, -1, 2, -1, 2), 14, -2 //negate
    )
  }

  final val Hanon9: Sequence with Notes = {
    hanon(Modes(0).toKey(), 48,
          Vector(2, 1, -1, 2, -1, 2, -1, -3), 14, 1,
          Vector(-2, -1, 1, -2, 1, -2, 1, 3), 14, -2 //negate
    ) //TODO: the final measure in this exercise varies from the rest
  }

  final val Hanon10: Sequence with Notes = {
    hanon(Modes(0).toKey(), 48,
          Vector(5, -1, -1, -1, 1, -1, 1, -2), 14, 2,
          Vector(-5, 1, 1, 1, -1, 1, -1, 2), 14, -2 //negate
    )
  }

  final val Hanon11: Sequence with Notes = {
    hanon(Modes(0).toKey(), 48,
          Vector(2, 3, -1, 1, -1, -1, 1, -3), 14, 1,
          Vector(-3, -2, 1, -1, 1, 1, -1, 3), 14, -1 //negate
    )
  }

  final val Hanon12: Sequence with Notes = {
    hanon(Modes(0).toKey(), 55,
          Vector(-4, 2, -1, -1, 1, 1, -2, 6), 14, -3,
          Vector(4, -2, 1, 1, -1, -1, 2, -6), 15, -3 //negate
    ) //TODO: the final measure in this exercise varies from the rest
  }

  final val Hanon13: Sequence with Notes = {
    hanon(Modes(0).toKey(), 52,
          Vector(-2, 3, -2, 3, -2, 1, 1, -1), 14, -1,
          Vector(2, -3, 2, -1, -2, 1, 1, -1), 14, -3 //negate
    )
  }

  final val Hanon14: Sequence with Notes = {
    hanon(Modes(0).toKey(), 48,
      Vector(1, 2, -1, 1, -1, 2, -1, -2), 14, 2,
      Vector(-1, -2, 1, -1, 1, -2, 1, 2), 14, -2 //negate
    )
  }

  final val Hanon15: Sequence with Notes = {
    hanon(Modes(0).toKey(), 48,
      Vector(2, -1, 2, -1, 2, -1, 2, -4), 14, 1,
      Vector(-2, 1, -2, 1, -2, 1, -2, 4), 14, -1 //negate
    ) //TODO: the final measure in this exercise varies from the rest
  }

  final val Hanon16: Sequence with Notes = {
    hanon(Modes(0).toKey(), 48,
      Vector(2, -1, 1, 3, -1, -1, 1, -3), 14, 1,
      Vector(-3, 1, -1, -2, 1, 1, -1, 3), 14, -1
    )
  }

  final val Hanon17: Sequence with Notes = {
    hanon(Modes(0).toKey(), 48,
      Vector(2, 3, -1, 2, -1, -1, 1, -4), 14, 2,
      Vector(-3, -2, 1, -2, 1, 1, -2, 5), 13, -1
    ) //TODO: the final measure in this exercise varies from the rest
  }

  final val Hanon18: Sequence with Notes = {
    hanon(Modes(0).toKey(), 48,
      Vector(1, 2, -1, 2, -1, -2, 1, -1), 14, 3,
      Vector(-1, -2, 1, -2, 1, 2, -1, 1), 14, -3 //negate
    )
  }

  final val Hanon19: Sequence with Notes = {
    hanon(Modes(0).toKey(), 48,
      Vector(5, -2, 1, 1, -2, -1, 2, -3), 14, 1,
      Vector(-5, 2, -1, -1, 2, 1, -2, 3), 14, -1 //negate
    )
  }

  final val Hanon20: Sequence with Notes = {
    hanon(Modes(0).toKey(), 52,
      Vector(2, 3, 2, -2, -1, 1, -2, -2), 14, 5,
      Vector(-2, -3, -2, 2, -1, 1, -2, 6), 15, -1
    )
  }

  final val HanonPart1: Vector[Sequence with Notes] = Vector(
    Hanon1, Hanon2, Hanon3, Hanon4, Hanon5,
    Hanon6, Hanon7, Hanon8, Hanon9, Hanon10,
    Hanon11, Hanon12, Hanon13, Hanon14, Hanon15,
    Hanon16, Hanon17, Hanon18, Hanon19, Hanon20
  )

  //TODO: Friedmann Appendix 1 Group 1-3 (solo instrument)
  final val Freidmann1 = {
    val deltas = DeltaSequence("", Vector(1, 1, 1, -1, -1, -1, -1, 0, 1,
                                          0, 1, 1, 1, -1, -1, -1, -1, 0, 1,
                                          0, 1, 1, 1, -1, -1, -1, -1, 0, 1, 0,
                                          0, 1, 1, 1, -1, -1, -1, -1, -1, -1, -1))

    // bpm: Q = 116
    val rhythm = Vector(S, S, E, S, S, S, S, E, E, Q,
                        S, S, S, S, E, S, S, E, E, Q,
                        S, S, E, S, S, S, S, E, E, E, E,
                        S, S, S, S, S, S, S, S, E, E, Q)

    RhythmicNoteSequence(
      "Stravinsky, Petrouchka, \"Danse russe,\" mm. 1-8, flute",
      KeyOfC.traverseByDeltaSequence(83, deltas),
      rhythm
    )
  }

  //TODO: figure out how to notate rests. one option could be subclassing Int to treat a designated number as a rest. operators would have to be overridden

  final val Freidmann2 = {
    val deltas = DeltaSequence("", Vector(0/*rest*/, 1, 1, 0/*rest*/, 1, -1, -1, -1, -2, 3, -2))

    // bpm: Q = 44
    val rhythm = Vector(Q/*rest*/, H, E, E, Q/*rest*/, Q, Q3, Q3, Q3, H/*quarters tied*/, Q, H)

    RhythmicNoteSequence(
      "Debussy, Preludes for Piano, book 1, no. 6, \"Footsteps in the Snow,\" mm. 2-5",
      KeyOfF.traverseByDeltaSequence(70, deltas),
      rhythm
    )
  }

}
