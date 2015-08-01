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

object Chapter4 {
    def mean(xs: Seq[Double]): Option[Double] = if (xs.size == 0) None else Some(xs.sum / xs.size)
    
    def variance(xs: Seq[Double]): Option[Double] = {
        mean(xs) flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
    }
}