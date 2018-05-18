package ifmo.fpscala.monoid

object Monoids {

  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(f: String, s: String): String = f concat s

    def zero: String = ""
  }

  val intAdditionMonoid = new Monoid[Int] {
    override def op(f: Int, s: Int): Int = f + s

    override def zero: Int = 0
  }

  def optionMonoid[A](implicit monoid: Monoid[A]) = new Monoid[Option[A]] {
    def op(f: Option[A], s: Option[A]): Option[A] = {
      f match {
        case fper@Some(funw) => s match {
          case Some(sunw) => Some(monoid.op(funw, sunw))
          case None => fper
        }
        case None => s
      }
    }

    def zero: Option[A] = None
  }

  def endMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(f: A => A, s: A => A): A => A = f andThen s

    override def zero: A => A = identity
  }

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.foldLeft(m.zero) { case (acc, item) => m.op(acc, f(item)) }
  }

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.isEmpty) {
      m.zero
    } else if (v.length == 1) {
      f(v.head)
    } else {
      val (h, t) = v.splitAt(v.length / 2)
      m.op(foldMapV(h, m)(f), foldMapV(t, m)(f))
    }
  }

}
