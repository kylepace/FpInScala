trait Prop {
    def check: Boolean
    
    def &&(p: Prop): Prop = new Prop {
        def check = Prop.this.check && p.check
    }
    
    //def check: Either[(FailedCase, SuccessCount), SuccessCount]
}

case class Gen[A](sample: State[RNG, A]) {
    
    def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(s => f(s).sample))
    
    
}

object Chapter8 {
    def choose(start: Int, stopExclusive: Int): Gen[Int] =
        Gen(State(Chapter6.nonNegativeInt).map(n => start + n % (stopExclusive-start)))
        
    def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
}

object Prop {
    type FailedCase = String
    type SuccessCount = Int
    
    
}