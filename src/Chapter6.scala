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

object Chapter6 {
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