import State._

trait RNG {
    def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
    def nextInt(): (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }
}

case class State[S, +A] (run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
        flatMap(a => unit(f(a)))
        
    def flatMap[B](f: A => State[S, B]): State[S, B] =
        State(s => {
            val (a, s1) = run(s)
            f(a).run(s1)
        })
        
    def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
        flatMap(a => rb map(b => f(a, b)))
}

object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))
    
    def set[S](s: S): State[S, Unit] = State(_ => ((), s))
    
    def get[S]: State[S, S] = State(s => (s, s))
    
    def modify[S](f: S => S): State[S, Unit] = for {
        s <- get
        _ <- set(f(s))
    } yield ()
}

object Chapter6 {
    type State[S, +A] = S => (A, S)
    
    type Rand[A] = State[RNG, A]
    
    def unit[A](a: A): Rand[A] =
        rng => (a, rng)
    
    def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
        rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }
    
    def mapViaFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
        flatMap(s)(a => unit(f(a)))
        
    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
        rng => {
            val (a, rng2) = f(rng)
            g(a)(rng2)
        }
    
    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
        rng => {
            val (a, rng2) = ra(rng)
            val (b, rng3) = rb(rng2)
            (f(a, b), rng3)
        }
        
    def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
        flatMap(ra)(a => map(rb)(b => f(a, b)))
        
    def doubleWithMap(rng: RNG): Rand[Double] =
        map(nonNegativeInt)(i => (1 / (i + 1) toDouble))
    
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
        val (nextInt, newRng) = rng.nextInt
        if (nextInt == Int.MinValue) (0, newRng)
        else (Math.abs(nextInt), newRng)
    }
    
    def double(rng: RNG): (Double, RNG) = {
        val (nextInt, newRng) = nonNegativeInt(rng)
        (1 / (nextInt + 1) toDouble, newRng)
    }
    
    def intDouble(rng: RNG): ((Int, Double), RNG) = {
        val (nextInt, newRng) = rng.nextInt
        val (nextDouble, dblRng) = double(newRng)
        ((nextInt, nextDouble), dblRng)
    }
    
    def doubleInt(rng: RNG): ((Double, Int), RNG) = {
        val (pair, nextRng) = intDouble(rng)
        ((pair._2, pair._1), nextRng)
    }
    
    def double3(rng: RNG): ((Double, Double, Double), RNG) = {
        val (db1, rng1) = double(rng)
        val (db2, rng2) = double(rng1)
        val (db3, rng3) = double(rng2)
        ((db1, db2, db3), rng3)
    }
    
    def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        @annotation.tailrec    
        def ints_h(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
            if (count <= 0) (acc, rng)
            else {
                val (i, r) = rng.nextInt
                ints_h(count -1, r, i :: acc)
            }
        }
        ints_h(count, rng, List())
    }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candles: Int, coins: Int)

// Taken directly from the answer sheet (lack of sequence) and confusing post-fix on the compose / update 
object Candy {
    def update = (i: Input) => (s: Machine) =>
        (i, s) match {
            case (_, Machine(_, 0, _)) => s
            case (Coin, Machine(false, _, _)) => s
            case (Turn, Machine(true, _, _)) => s
            case (Coin, Machine(true, candy, coin)) =>
                Machine(false, candy, coin + 1)
            case (Turn, Machine(false, candy, coin)) =>
                Machine(true, candy - 1, coin)
        }
    
//    def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
//        _ <- sequence(inputs map (modify[Machine] _ compose update))
//        s <- get
//    } yield (s.coins, s.candies)
}



