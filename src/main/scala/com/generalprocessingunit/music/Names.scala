package com.generalprocessingunit.music

object Names {
  final val Chromatic = "Chromatic"
  final val Diatonic = "Diatonic"
  final val Pentatonic = "Pentatonic"
  final val WholeTone = "WholeTone"
  final val Octatonic = "Octatonic"

  final val Ionian = "Ionian"
  final val Dorian = "Dorian"
  final val Phrygian = "Phrygian"
  final val Lydian = "Lydian"
  final val Mixolydian = "Mixolydian"
  final val Aeolian = "Aeolian"
  final val Locrian = "Locrian"
  
  final val WesternModesMajorScaleOrder = Vector(Ionian, Dorian, Phrygian, Lydian, Mixolydian, Aeolian, Locrian)
  final val WesternModesBrightToDarkOrder = Vector(Lydian, Ionian, Mixolydian, Dorian, Aeolian, Phrygian, Locrian)

  final val CircleOfFifths = "Circle Of Fifths"

  final val Oh = "oh"
  final val One = "one"
  final val Two = "two"
  final val Three = "three"
  final val Four = "four"
  final val Five = "five"
  final val Six = "six"
  final val Sev = "sev"
  final val Eight = "eight"
  final val Nine = "nine"
  final val Ten = "ten"
  final val El = "el"

  final val PitchClasses = Vector(Oh, One, Two, Three, Four, Five, Six, Sev, Eight, Nine, Ten, El)
}
