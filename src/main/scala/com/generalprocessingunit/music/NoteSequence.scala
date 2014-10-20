package com.generalprocessingunit.music

case class NoteSequence(name: String, sequence: Vector[Int]) extends Sequence with Notes
