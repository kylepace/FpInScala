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
    
    def map[B](f: A => B): Stream[B] =
        foldRight(empty[B])((h, t) => cons(f(h), t))
    
    def filter(f: A => Boolean): Stream[A] =
        foldRight(empty[A]: Stream[A])((h, t) =>
            if (f(h)) cons(h, t)
            else t
        )
        
    def append[B>:A](s: => Stream[B]): Stream[B] =
        foldRight(s)((h, t) => cons(h, t))
        
    def flatMap[B](f: A => Stream[B]): Stream[B] =
        foldRight(empty[B])((h, t) => f(h) append t)
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

object Chapter5 {
    def constant[A](a: A): Stream[A] = cons(a, constant(a))
    
    // from example code, just references itself.
    def optimizedConstant[A](a: A): Stream[A] = {
        lazy val tail: Stream[A] = Cons(() => a, () => tail)
        tail
    }
    
    def from(n: Int): Stream[Int] =
        cons(n, from(n + 1))
        
    def fib: Stream[Int] = {
        def fib_h(a: Int, b: Int): Stream[Int] =
            cons(a, fib_h(b, a + b))
            
        fib_h(0, 1)
    }
    
    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
        f(z) match {
            case Some((a, s)) => cons(a, unfold(s)(f))
            case None => empty
        }
}