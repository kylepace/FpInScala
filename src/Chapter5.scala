import Stream._
import Chapter5._

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
    
    def takeWithUnfold(n: Int): Stream[A] =
        unfold((this, n)) {
            case (Cons(h, t), 1) => Some((h(), (empty, 0)))
            case (Cons(h, t), n) => Some((h(), (t(), n - 1)))
            case _ => None
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
    
    def takeWhileWithUnfold(p: A => Boolean): Stream[A] =
        unfold(this) {
            case Cons(h, t) if (p(h())) => Some((h(), t()))
            case _ => None
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
    
    def mapWithUnfold[B](f: A => B): Stream[B] =
        unfold(this) {
            case Cons(h, t) => Some((f(h()), t()))
            case Empty => None
        }
    
    def filter(f: A => Boolean): Stream[A] =
        foldRight(empty[A]: Stream[A])((h, t) =>
            if (f(h)) cons(h, t)
            else t
        )
        
    def append[B>:A](s: => Stream[B]): Stream[B] =
        foldRight(s)((h, t) => cons(h, t))
        
    def flatMap[B](f: A => Stream[B]): Stream[B] =
        foldRight(empty[B])((h, t) => f(h) append t)

    def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] =
        unfold((this, s2)) {
            case ((Cons(h1, t1), Cons(h2, t2))) =>
                Some((f(h1(), h2()), (t1(), t2())))
            case _ => None
        }
    
    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
        unfold((this, s2)) {
            case ((Cons(h1, t1), Cons(h2, t2))) =>
                Some((Some(h1()), Some(h2())), (t1(), t2()))
            case ((Cons(h1, t1), Empty)) =>
                Some((Some(h1()), None), (t1(), empty))
            case (Empty, Cons(h2, t2)) =>
                Some((None, Some(h2())), (empty, t2()))
            case _ => None
        }
    
    def tails: Stream[Stream[A]] =
        unfold(this) {
            case Cons(h, t) => Some((cons(h(), t()), t()))
            case _ => None
        }
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
    
    val fib_unfold = unfold((0, 1)) {
        case (a, b) => Some((a, (a, a + b)))
    }
    
    def from_unfold(n: Int): Stream[Int] = unfold(n)(a => Some((a, a + 1)))
    
    def constant_unfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))
    
    val ones_unfold = unfold(1)(_ => Some((1, 1)))
    
    def printStream[A](s: Stream[A]) = s map(a => println(a))
}