abstract class Sequence {
  def name: String

  def sequence: Vector[Int]
}

object Sequence {
    implicit def sToVectorInt(s: Sequence): Vector[Int] = s.sequence
}
