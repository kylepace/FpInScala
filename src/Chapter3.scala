sealed trait TestList[+A]
case object TestNil extends TestList[Nothing]
case class TestCons [+A] (head: A, tail: TestList[A]) extends TestList[A]

object TestList {
    def apply[A](as: A*): TestList[A] =
        if (as.isEmpty) TestNil
        else TestCons(as.head, apply(as.tail: _*))
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Chapter3 {
    def tail[A](l: TestList[A]): TestList[A] = 
        l match {
            case TestNil => sys.error("tail of empty list")
            case TestCons(_, t) => t
        }
    
    def setHead[A](l: TestList[A], newHead: A): TestList[A] = 
        l match {
            case TestNil => TestCons(newHead, TestNil)
            case TestCons(_, t) => TestCons(newHead, t)
        }
    
    @annotation.tailrec
    def drop[A](l: TestList[A], itemsToDrop: Int): TestList[A] =
        if (itemsToDrop == 0) l
        else l match {
            case TestNil => TestNil
            case TestCons(_, t) => drop(t, itemsToDrop - 1)
        }
    
    @annotation.tailrec
    def dropWhile[A](l: TestList[A])(f: A => Boolean): TestList[A] =
        l match {
            case TestNil => TestNil
            case TestCons(h, t) =>
                if (f(h)) dropWhile(t)(f)
                else TestCons(h, t)
        }
    
    def init[A](l: TestList[A]): TestList[A] =
        l match {
            case TestNil => sys.error("empty list.")
            case TestCons(_, TestNil) => TestNil
            case TestCons(h, t) => TestCons(h, init(t))
        }
    
    def foldRight[A, B](as: TestList[A], z: B)(f: (A, B) => B): B =
        as match {
            case TestNil => z
            case TestCons(h, t) => f(h, foldRight(t, z)(f))
        }
    
    @annotation.tailrec
    def foldLeft[A, B](as: TestList[A], z: B)(f: (B, A) => B): B =
        as match {
            case TestNil => z
            case TestCons(h, t) => foldLeft(t, f(z, h))(f)
        }
    
    def sum(ints: TestList[Int]): Int = foldLeft(ints, 0)(_ + _)
    
    def reverse[A](l: TestList[A]): TestList[A] = foldLeft(l, TestList[A]())((acc, h) => TestCons(h, acc))
    
    def append[A](l: TestList[A])(r: TestList[A]): TestList[A] = foldRight(l, r)(TestCons(_, _))
    
    def addOne(ints: TestList[Int]): TestList[Int] = foldLeft(ints, TestList[Int]())((t, h) => TestCons(h + 1, t))
    
    def doubleToString(dbls: TestList[Double]): TestList[String] =
        foldRight(dbls, TestList[String]())((x, xs) => TestCons(x.toString(), xs))
        
    // not stack safe
    def map[A, B](as: TestList[A])(f: A => B): TestList[B] =
        foldRight(as, TestList[B]())((h, t) => TestCons(f(h), t))
        
    def zipInt(a: TestList[Int], b: TestList[Int]): TestList[Int] = (a, b) match {
        case (TestNil, _) => TestNil
        case (_, TestNil) => TestNil
        case (TestCons(h1, t1), TestCons(h2, t2)) => TestCons(h1 + h2, zipInt(t1, t2))
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