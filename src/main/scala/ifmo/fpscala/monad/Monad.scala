package ifmo.fpscala.monad

trait Monad[F[_]] extends Functor[F] {

  def map[A, B](as: F[A])(f: A => B): F[B] = flatMap(as)(a => unit(f(a)))

  def unit[A](a: => A): F[A]

  def flatMap[A, B](as: F[A])(a: A => F[B]): F[B]

  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] = flatMap(ma)(a => map(mb)(b => f(a,b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] = {
    lma.foldLeft(unit(List.empty[A])){case (acc, b) => map2(acc, b)((unF, unS) => unS :: unF)}
  }

  def traverse[A,B](la: List[A])(f: A => F[B]): F[List[B]] = sequence(la.map(f))

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

}

object Monads {

  val optionMonad = new Monad[Option] {

    def unit[A](a: => A): Option[A] = Option(a)

    def flatMap[A, B](as: Option[A])(a: A => Option[B]): Option[B] = as.flatMap(a)

  }

}




