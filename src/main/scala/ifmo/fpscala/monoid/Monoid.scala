package ifmo.fpscala.monoid

trait Monoid[A] {
  def op(f: A, s: A): A
  def zero: A
}
