trait Monad[F[_]] {
    def unit[A](a: => A): F[A]
    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
    
    def map[A, B](ma: F[A])(f: A => B): F[B] =
        flatMap(ma)(a => unit(f(a)))
}

object Chapter11 {
    val genMonad = new Monad[Gen] {
        def unit[A](a: => A): Gen[A] = Gen.unit(a)
        def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f
    }
    
    val optionMonad = new Monad[Option] {
        def unit[A](a: => A): Option[A] = Some(a)
        def flatMap[A, B](ma: Option[A])(f: A => Option[B]) = ma flatMap f
    }
    
    val streamMonad = new Monad[Stream] {
        def unit[A](a: => A): Stream[A] = Stream(a)
        def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]) = ma flatMap f
    }
    
    val listMonad = new Monad[List] {
        def unit[A](a: => A): List[A] = List(a)
        def flatMap[A, B](ma: List[A])(f: A => List[B]) = ma flatMap f
    }
}