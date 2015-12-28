trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
}

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

object Chapter10 {
    val wcMonoid = new Monoid[WC] {
        def op(a1: WC, a2: WC) = (a1, a2) match {
            case (Part(l1, c1, r1), Part(l2, c2, r2)) => {
                val middle = if ((r1 + l2).isEmpty) 0 else 1
                Part(l1, c1 + middle + c2, r2)
            }
            case (Part(l, c, r), Stub(ch)) => Part(l, c, r + ch)
            case (Stub(ch), Part(l, c, r)) => Part(ch + l, c, r)
            case (Stub(ch1), Stub(ch2)) => Stub(ch1 + ch2)
        }
        def zero = Stub("")
    }
    
    // Def not my first pass at the implementation
    def countWords(s: String): Int = {
        def charCount(c: Char): WC = {
            if (c.isWhitespace)
                Part("", 0, "")
            else
                Stub(c.toString)
        }
        def getMaxLength(s: String) = s.length min 1
        foldMapV(s.toIndexedSeq, wcMonoid)(charCount) match {
            case Part(l, w, r) => getMaxLength(l) + w + getMaxLength(l)
            case Stub(s) => getMaxLength(s)
        }
    }
        
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
        
    def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
        if (v.length == 0)
            m.zero
        else if (v.length == 1)
            f(v(0))
        else {
            val sequences = v.splitAt(v.length / 2)
            m.op(foldMapV(sequences._1, m)(f), foldMapV(sequences._2, m)(f))
        }
    }
    
    // foldable stuff was pretty self-explanatory skipping to composing monoids
    
    def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
        def op(x: (A, B), y: (A, B)): (A, B) = {
            (A.op(x._1, y._1), B.op(x._2, y._2))
        }
        def zero = (A.zero, B.zero)
    }
    
    def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
        def op(f: A => B, g: A => B): (A => B) =
            (a) => {
                B.op(f(a), g(a))
            }
        def zero = a => B.zero
    }
}