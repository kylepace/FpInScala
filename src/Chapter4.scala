import scala.collection.immutable._

sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
        case None => None
        case Some(a) => Some(f(a))
    }
    
    def getOrElse[B >: A](default: => B): B = this match {
        case None => default
        case Some(b) => b
    }
    
    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
    
    def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob
   
    def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) this else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
        case Right(a) => Right(f(a))
        case Left(e) => Left(e)
    }
    
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
        case Right(a) => f(a)
        case Left(e) => Left(e)
    }
    
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
        case Right(a) => Right(a)
        case Left(_) => b
    }
    
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
        this flatMap (a => b map (bb => f(a, bb)))
        //for { a <- this; b1 <- b } yield f(a, b1)    
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Chapter4 {
    def mean(xs: Seq[Double]): Option[Double] = if (xs.size == 0) None else Some(xs.sum / xs.size)
    
    def variance(xs: Seq[Double]): Option[Double] = {
        mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }
    
    def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
        a flatMap (aa => b map (bb => f(aa, bb)))
    
    def sequence[A](a: List[Option[A]]): Option[List[A]] =
        a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
        
    def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
        case Nil => Some(Nil)
        case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }
    
    def traverse_Either[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
        case Nil => Right(Nil)
        case h::t => (f(h) map2 traverse_Either(t)(f))(_ :: _)
        // below is the less syntactic sugar way of writing the above
        //case h::t => f(h).map2(traverse_Either(t)(f))((a, b) => a :: b)
    }
    
    def sequence_Either[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
        traverse_Either(es)(x => x)
}