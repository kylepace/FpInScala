

trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
}

object Chapter10 {
    val stringMonoid = new Monoid[String] {
        def op(a1: String, a2: String) = a1 + a2
        def zero = ""
    }
    
    def listMonoid[A] = new Monoid[List[A]] {
        def op(a1: List[A], a2: List[A]) = a1 ++ a2
        def zero = Nil
    }
    
    val intAddition = new Monoid[Int] {
        def op(a1: Int, a2: Int) = a1 + a2
        def zero = 0
    }
    
    val intMultiplication = new Monoid[Int] {
        def op(a1: Int, a2: Int) = a1 * a2
        def zero = 1
    }
    
    val booleanOr = new Monoid[Boolean] {
        def op(a1: Boolean, a2: Boolean) = a1 || a2
        def zero = false
    }
    
    val booleanAnd = new Monoid[Boolean] {
        def op(a1: Boolean, a2: Boolean) = a1 && a2
        def zero = true
    }
    
    def optionMonoid[A] = new Monoid[Option[A]] {
        def op(a1: Option[A], a2: Option[A]) = a1 orElse a2
        def zero = None
    }
    
    def endoMonoid[A] = new Monoid[A => A] {
        // alternatively
        def op2(a1: A => A, a2: A => A) = a1 compose a2 // from the book shorthand for below
        def op(a1: A => A, a2: A => A) = a => {
            a1(a2(a))
        }
        def zero = a => a
    }
    
    def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)
    
    def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
        as.foldLeft(m.zero)((b, a) => m.op(b, f(a)))
}