package ifmo.fpscala.monoid

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

class FoldableTree extends Foldable[Tree] {

  def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = {
    as match {
      case Leaf(value) => f(value, z)

      case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
    }
  }

  def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = {
    as match {
      case Leaf(value) => f(z, value)

      case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
    }
  }

  def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = {
    foldLeft(as)(mb.zero){ case (a, b) => mb.op(a, f(b))}
  }

}