sealed trait Stream[+A] {
    
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
        if (as.isEmpty) empty
        else cons(as.head, apply(as.tail: _*))
    }
}