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
        
    def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = {
        apply(apply(apply(unit(f.curried))(fa))(fb))(fc)
    }
    
    def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = {
        apply(apply(apply(apply(unit(f.curried))(fa))(fb))(fc))(fd)
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E] = Vector()) extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]

object Chapter12 {
    def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f] {
        def unit[A](a: => A): Either[E, A] = Right(a)
        def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
            ma.flatMap(f)
        
        def flatMapFromBook[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ma match {
            case Right(a) => f(a)
            case Left(e) => Left(e)
        }
    }
    
    def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] = new Applicative[({type f[x] = Validation[E,x]})#f] {
        def unit[A](a: => A): Validation[E, A] = Success(a)
        def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C): Validation[E, C] = (fa, fb) match {
            case (Success(a), Success(b)) => Success(f(a, b))
            case (Failure(h1, t1), Failure(h2, t2)) => Failure(h1, t1 ++ Vector(h2) ++ t2)
            case (Success(_), Failure(h1, t1)) => Failure(h1, t1)
            case (Failure(h1, t1), Success(_)) => Failure(h1, t1)
//          can also be written in the shorthand with alias            
//          case (Success(_), e@Failure(_, _)) => e
//          case (e@Failure(_, _), Success(_)) => e
        }
    }
}