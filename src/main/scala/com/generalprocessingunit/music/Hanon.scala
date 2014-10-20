package com.generalprocessingunit.music

trait Hanon {
  protected def hanon(key: Key, startingNote: Int,
            ascTemplate: Vector[Int], ascRepetitions: Int, transition1: Int,
            descTemplate: Vector[Int], descRepetitions: Int, transition2: Int): Sequence with Notes = {
    def deltas: Vector[Int] = {
      def repeatTemplate(rep: Int, t: Vector[Int]) = (1 to rep flatMap (n => t)).dropRight(1).toVector
      val deltasAsc = repeatTemplate(ascRepetitions, ascTemplate)
      val deltasDesc = repeatTemplate(descRepetitions, descTemplate)
      (deltasAsc :+ transition1) ++ (deltasDesc :+ transition2)
    }

    key.traverseByDeltaSequence(startingNote, DeltaSequence("HanonDeltas", deltas))
  } // another way to express this is modulating a scale by h1Asc and h1Desc
}
