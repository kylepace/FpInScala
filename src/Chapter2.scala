object Chapter2 {
    def fib(n: Int): Int = {
        @annotation.tailrec
        def fibh(n: Int, first: Int, second: Int): Int = {
            if (n == 0) first
            else if (n == 1) second
            else {
                fibh(n - 1, second, first + second)
            }
        }
        fibh(n, 0, 1)
    }
  
    def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
        @annotation.tailrec
        def loop(n: Int): Boolean = {
            if (n <= 0 || n == 1) true
            else if (ordered(as(n), as(n - 1))) loop(n - 1)
            else false
        }
        
        loop(as.length - 1)
    }
    
    def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
        a => (b: B) => f(a, b)
    }
    
    def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
        (a, b) => f(a)(b)
    }
    
    def compose[A, B, C](f: B => C, g: A => B): A => C = {
        a => f(g(a))    
    }
}