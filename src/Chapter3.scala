sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons [+A] (head: A, tail: List[A]) extends List[A]

object List {
    def apply[A](as: A*): List[A] =
        if (as.isEmpty) Nil
        else Cons(as.head, apply(as.tail: _*))
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Chapter3 {
    def tail[A](l: List[A]): List[A] = 
        l match {
            case Nil => sys.error("tail of empty list")
            case Cons(_,t) => t
        }
    
    def setHead[A](l: List[A], newHead: A): List[A] = 
        l match {
            case Nil => Cons(newHead, Nil)
            case Cons(_, t) => Cons(newHead, t)
        }
    
    @annotation.tailrec
    def drop[A](l: List[A], itemsToDrop: Int): List[A] =
        if (itemsToDrop == 0) l
        else l match {
            case Nil => Nil
            case Cons(_, t) => drop(t, itemsToDrop - 1)
        }
    
    @annotation.tailrec
    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
        l match {
            case Nil => Nil
            case Cons(h, t) =>
                if (f(h)) dropWhile(t)(f)
                else Cons(h, t)
        }
    
    def init[A](l: List[A]): List[A] =
        l match {
            case Nil => sys.error("empty list.")
            case Cons(_, Nil) => Nil
            case Cons(h, t) => Cons(h, init(t))
        }
    
    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
            case Nil => z
            case Cons(h, t) => f(h, foldRight(t, z)(f))
        }
    
    @annotation.tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
        as match {
            case Nil => z
            case Cons(h, t) => foldLeft(t, f(z, h))(f)
        }
    
    def sum(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)
    
    def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
    
    def append[A](l: List[A])(r: List[A]): List[A] = foldRight(l, r)(Cons(_, _))
    
    def addOne(ints: List[Int]): List[Int] = foldLeft(ints, List[Int]())((t, h) => Cons(h + 1, t))
    
    def doubleToString(dbls: List[Double]): List[String] =
        foldRight(dbls, List[String]())((x, xs) => Cons(x.toString(), xs))
        
    // not stack safe
    def map[A, B](as: List[A])(f: A => B): List[B] =
        foldRight(as, List[B]())((h, t) => Cons(f(h), t))
        
    def zipInt(a: List[Int], b: List[Int]): List[Int] = (a, b) match{
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipInt(t1, t2))
    }
    
    def size[A](t: Tree[A]): Int = t match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
    }
    
    def maximum(t: Tree[Int]): Int = t match {
        case Leaf(v) => v
        case Branch(l, r) => maximum(l) max maximum(r)
    }
    
    def depth[A](t: Tree[A]): Int = t match {
        case Leaf(v) => 0
        case Branch(l, r) => 1 + (depth(l) max depth(r))
    }
}