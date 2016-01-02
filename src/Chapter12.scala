trait Applicative[F[_]] {
    def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C]
    def unit[A](a: => A): F[A]
    
    def map[A, B](fa: F[A])(f: A => B): F[B] = 
        map2(fa, unit())((a, _) => f(a))
        
    def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
        as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))
        
    def sequence[A](fas: List[F[A]]): F[List[A]] =
        traverse(fas)(fa => fa)
    
    // alternatively as per book
    // sequence(List.fill(n)(fa)) Fill does the work of the generator
    def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
        sequence((for (i <- 0 until n) yield fa).toList)
    
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
        map2(fa, fb)((_, _)) // shorthand for (a, b) => (a, b)

    def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] =
        map2(fa, fab)((a, b) => b(a)) // can be written shorthand as map2(fa, fab)(_(_))
        
    def mapByApply[A, B](fa: F[A])(f: A => B): F[B] =
        apply(unit(f))(fa)
        
    def map2ByApply[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
        apply(map(fa)(f.curried))(fb)  
}

object Chapter12 {
    
}