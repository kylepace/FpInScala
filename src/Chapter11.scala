case class Id[A](value: A) {
    def flatMap[B](f: A => Id[B]): Id[B] = f(value)
    def map[B](f: A => B): Id[B] = Id(f(value))
}

trait Monad[F[_]] {
	def unit[A](a: => A): F[A]
    def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]
    
    def map[A, B](ma: F[A])(f: A => B): F[B] =
        flatMap(ma)(a => unit(f(a)))
     
    def map2[A, B, C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
        flatMap(ma)(a => map(mb)(b => f(a, b)))
        
    def sequence[A](lma: List[F[A]]): F[List[A]] =
	    lma.foldRight(unit(List[A]()))((h, t) => map2(h, t)(_ :: _)) 	
	
	def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
	    la.foldRight(unit(List[B]()))((h, t) => map2(f(h), t)(_ :: _))
	    
	def replicateM[A](n: Int, ma: F[A]): F[List[A]] = {
	    if (n <= 0) unit(List[A]())
	    else
	        map2(ma, replicateM(n - 1, ma))(_ :: _)
	}
	
	def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = ms match {
	    case Nil => unit(Nil)
	    case h :: t => flatMap(f(h))(b =>
	        if (!b) filterM(t)(f)
	        else map(filterM(t)(f))(h :: _)
	    )
	}
	
	def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] = a => {
	    flatMap(f(a))(g)
	}
	
	def flatMapByCompose[A, B](ma: F[A])(f: A => F[B]): F[B] = {
	    val unwrapFunc = (_:Unit) => ma
	    compose(unwrapFunc, f)(())
	}
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
    
    val idMonad = new Monad[Id] {
        def unit[A](a: => A): Id[A] = Id(a)
        def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma flatMap f
    }
}