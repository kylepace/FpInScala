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
}