import Stream._

trait Stream[+A] {
    def headOption: Option[A] = this match {
        case Empty => None
        case Cons(h, t) => Some(h())
    }
    
    def toList: List[A] = {
        @annotation.tailrec
        def toList_h(st: Stream[A], as: List[A]): List[A] = st match {
            case Empty => as
            case Cons(h, t) => toList_h(t(), h() :: as)
        }
        toList_h(this, List()).reverse
    }
    
    def take(n: Int): Stream[A] = this match {
        case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
        case Cons(h, _) if n == 1 => cons(h(), empty)
        case _ => empty
    }
    
    @annotation.tailrec
    final def drop(n: Int): Stream[A] = this match {
        case Cons(_, t) if n > 0 => t().drop(n - 1)
        case _ => this
    }
    
    def takeWhile(p: A => Boolean): Stream[A] = this match {
        case Cons(h, t) if (p(h())) => cons(h(), t() takeWhile p)
        case _ => empty
    }
    
    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }
    
    def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

    def takeWhile_fold(f: A => Boolean): Stream[A] =
        foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else empty)
    
    //  Option[A] is necessary in the first parameter for compiler to behave
    def headOption_fold: Option[A] =
        foldRight(None: Option[A])((a, b) => Some(a))
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
    def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = t1
        Cons(() => head, () => tail)
    }
    
    def empty[A]: Stream[A] = Empty
    
    def apply[A](as: A*): Stream[A] = {
        if (as.isEmpty) this.empty
        else cons(as.head, apply(as.tail: _*))
    }
}