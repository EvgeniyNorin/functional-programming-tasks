package ifmo.fpscala.monad

trait Functor[F[_]] {

  def map[A, B](as: F[A])(f: A => B): F[B]

}
