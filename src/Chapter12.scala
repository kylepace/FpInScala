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
    def replicateM[A](n: Int, fa: F[A]): F[List[A]] = {
        val items = (for (i <- 0 until n) yield fa)
        sequence(items.toList)
    }
    
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
        map2(fa, fb)((_, _)) // shorthand for (a, b) => (a, b)
}

object Chapter12 {
    
}