abstract class Sequence {
  def name: String

  def sequence: Vector[Int]

  implicit def sToVectorInt(s: Sequence): Vector[Int] = s.sequence
}
