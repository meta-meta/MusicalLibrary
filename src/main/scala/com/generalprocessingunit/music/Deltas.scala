package com.generalprocessingunit.music

import scala.collection.mutable

trait Deltas extends Sequence {
  def toNoteSequence(firstNote: Int) = NoteSequence(name, getNoteSequence(firstNote))

  def modulateByNoteSequence(noteSequence: Sequence with Notes): Vector[NoteSequence] = {
    noteSequence map (n => toNoteSequence(n))
  }

  def modulateByDeltaSequence(firstNote: Int, deltaSequence: Sequence with Deltas, restrictModulatorToOneOctave: Boolean = true): Vector[NoteSequence] = {
    val n = deltaSequence.toNoteSequence(firstNote)
    val modulator = if (restrictModulatorToOneOctave) n.restrictToOneOctave else n
    modulateByNoteSequence(modulator)
  }

  def negate: Sequence with Deltas = {
    DeltaSequence(name, sequence map (d => -d))
  }

  protected def getNoteSequence(firstNote: Int): Vector[Int] = {
    val noteList = mutable.MutableList(firstNote)
    for (d <- sequence)
      noteList += (noteList.last + d)
    noteList.toVector
  }
}

//TODO: what would be the implications of making delta sequence the de facto format, the first number in the sequence is the distance from 0. "Playing" the first number in the delta sequence therefore only sets the cursor. It is the 0th element and 0 in t. Is this a more elegant solution? Then every melody is nothing but composites of delta sequences. Modulation of a note sequence by a delta sequence becomes recursive application and expansion. Composition can be performed by starting from 0 and composing delta sequences together. An interface would be a grid where outline representations of the delta sequence are framed on top of each other, different looking frames for different operations(addition, modulation, traversal, multiplication) and the entire composition evaluated as one delta sequence applied to 0. Traversal may require the NoteSequence datatype to keep the operation trivial. NoteSequence and other types might be thought of as transitory abstractions that reduce back down to delta sequences. A similar type of composition can be applied in harmony-space. Harmony-space is a view of the composition as a note sequence. DeltaSets or harmonic collections of intervals can be placed on the root notes to form chords. This seems a very limited way to think about harmony. Instead of "harmony-space" the composition should just render in a way that shows the compositional frames and what they evaluate to. Each evaluated note is then a potential starting node for a delta sequence.