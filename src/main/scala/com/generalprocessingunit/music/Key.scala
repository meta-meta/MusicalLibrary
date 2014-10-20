package com.generalprocessingunit.music

import com.generalprocessingunit.music.MusicalLibrary._
import com.generalprocessingunit.music.NoteLetter.NoteLetter
import com.generalprocessingunit.music.Accidental.Accidental

/**
 * A Key is an ordered(for now) set of notes over the full range of notes that fall in the pitch classes contained in generatingSequence.
 * @param generatingSequence
 */
case class Key(generatingSequence: Sequence with Notes) extends Sequence with Notes {
  val name = generatingSequence.name
  val sequence = MusicalLibrary.AllNotes
    .filter(n => generatingSequence.contains(n % 12))
    .toVector

  val tonic = generatingSequence(0)

  override def traverseByDeltaSequence(startingNote: Int = 0, deltaSequence: Sequence with Deltas): Sequence with Notes = {
    require(sequence contains startingNote)
    super.traverseByDeltaSequence(sequence.indexOf(startingNote), deltaSequence)
  }

  def range(range: Range): Sequence with Notes = {
    NoteSequence(name, sequence.filter(n => range contains n))
  }

  def isNoteInKey(n: Int): Boolean = sequence.contains(n)

  /**
   * Note Names only make sense within the conext of a Key
   * @return all notes in this key with their proper names
   */
  def toNoteNames: Vector[NoteName] = sequence map (n => getNoteName(n))

  def getNoteName(n: Int): NoteName = chromaticNoteNames.getOrElse(n, NoteName.default)

  /**
   *
   * @return all notes with their proper names with respect to this key
   */
  def chromaticNoteNames: Map[Int, NoteName] = {
    val scale = Diatonic.toNoteSequence(tonic) // 0 => 0, 2, 4, 5, 7, 9, 11
    val tonicName = keysToNoteNames.getOrElse(this, NoteName.default)

    // 0, 2, 4, 5, 7, 9, 11 => C, D, E, F, G, A, B
    val numbersToLetters = (0 to 6 map (n => {
      (scale(n) % 12, NoteLetter((n + tonicName.letter.id) % 7))
    })).toMap

    def getAccidental(noteLetter: NoteLetter, noteNumber: Int): Accidental = {
      val numberOfNaturalNote = noteLetterToNumber.getOrElse(noteLetter, 0)
      noteNumber - numberOfNaturalNote match {
        case 0 => Accidental.Natural
        case -1 => Accidental.Flat
        case 1 => Accidental.Sharp
      }
    }

    val firstOctave = scala.collection.mutable.Map[Int, NoteName]()

    numbersToLetters.foreach(m => {
      val currNum = m._1
      val currLetter = m._2
      val currAccidental = getAccidental(currLetter, currNum)

      firstOctave += currNum -> NoteName(currLetter, 0, currAccidental)

      val nextNum = (currNum + 1) % 12
      if(!numbersToLetters.contains(nextNum)) {
        val nextLetter = if(currLetter == NoteLetter.B || currLetter == NoteLetter.E) {
          if(currAccidental == Accidental.Flat) currLetter else NoteLetter.next(currLetter)
        } else {
          if(currAccidental == Accidental.Sharp) NoteLetter.next(currLetter) else currLetter
        }

        firstOctave += nextNum -> NoteName(nextLetter, 0, getAccidental(nextLetter, nextNum))
      }
    })

    val b = Map.newBuilder[Int, NoteName]
    0 to 127 map (n => {
      firstOctave.get(n % 12) match {
        case Some(noteName) => b += n -> NoteName(noteName.letter, n / 12, noteName.accidental)
        case None => None
      }
    })
    b.result()
  }

  // TODO: WTF defining this as val results in a Map with only 10 elements
  private def keysToNoteNames = Map (
    KeyOfC ->   NoteName(NoteLetter.C, 0, Accidental.Natural),
    KeyOfDb ->  NoteName(NoteLetter.D, 0, Accidental.Flat),
    KeyOfD ->   NoteName(NoteLetter.D, 0, Accidental.Natural),
    KeyOfEb ->  NoteName(NoteLetter.E, 0, Accidental.Flat),
    KeyOfE ->   NoteName(NoteLetter.E, 0, Accidental.Natural),
    KeyOfF ->   NoteName(NoteLetter.F, 0, Accidental.Natural),
    KeyOfFs ->  NoteName(NoteLetter.F, 0, Accidental.Sharp),
    KeyOfG ->   NoteName(NoteLetter.G, 0, Accidental.Natural),
    KeyOfAb ->  NoteName(NoteLetter.A, 0, Accidental.Flat),
    KeyOfA ->   NoteName(NoteLetter.A, 0, Accidental.Natural),
    KeyOfBb ->  NoteName(NoteLetter.B, 0, Accidental.Flat),
    KeyOfB ->   NoteName(NoteLetter.B, 0, Accidental.Natural)
  )
  private val noteLetterToNumber = Map(
    NoteLetter.C -> 0,
    NoteLetter.D -> 2,
    NoteLetter.E -> 4,
    NoteLetter.F -> 5,
    NoteLetter.G -> 7,
    NoteLetter.A -> 9,
    NoteLetter.B -> 11
  )

}
