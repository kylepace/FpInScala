// Answer 7.1
// def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C]
import java.util.concurrent._
import java.util.concurrent.atomic._

object Par {
    type Par[A] = ExecutorService => Future[A]
    
    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)
    
    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
    
    private case class UnitFuture[A](get: A) extends Future[A] {
        def isDone = true
        def get(timeout: Long, units: TimeUnit) = get
        def isCancelled = false
        def cancel(evenIfRunning: Boolean): Boolean = false
    }
    
    private case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
        @volatile var cache: Option[C] = None
        def cancel(evenIfRunning: Boolean) = a.cancel(evenIfRunning) || b.cancel(evenIfRunning)
        def get = compute(Long.MaxValue)
        def get(timeout: Long, units: TimeUnit): C =
            compute(TimeUnit.NANOSECONDS.convert(timeout, units))
        def isCancelled = a.isCancelled || b.isCancelled
        def isDone = cache.isDefined
        
        private def compute(timeoutInNanos: Long): C = cache match {
            case Some(c) => c
            case None =>
                val start = System.nanoTime
                val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
                val stop = System.nanoTime;val aTime = stop-start
                val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
                val ret = f(ar, br)
                cache = Some(ret)
                ret
            }
        }
    
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
        (es: ExecutorService) => {
            val af = a(es)
            val bf = b(es)
            Map2Future(af, bf, f)
        }
        
    def fork[A](a: => Par[A]): Par[A] =
        es => es.submit(new Callable[A] {
            def call = a(es).get  
        })
        
    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
        
    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))
    
    def sequence[A](as: List[Par[A]]): Par[List[A]] =
        as.foldRight[Par[List[A]]](unit(List()))((h, t) => map2(h, t)(_ :: _))
    
    def map[A, B](pa: Par[A])(f: A => B): Par[B] =
        map2(pa, unit(()))((a, _) => f(a))
        
    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
        val pars: List[Par[List[A]]] =
            as map(asyncF((a: A) => if (f(a)) List(a) else List()))
        map(sequence(pars))(_.flatten)
    }
    
    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = es => {
        val index = run(es)(n).get
        choices(index)(es)
    }
    
    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
        choiceN(map(cond)(a => if (a) 0 else 1))(t :: List(f))
        
    def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = es => {
        run(es)(choices(run(es)(pa).get))
    }
    
    def choiceViaChooser[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = chooser(cond) { a => if (a) t else f }
    
    def choiceNViaChooser[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = chooser(n) { a => choices(a) }
    
    def join[A](a: Par[Par[A]]): Par[A] = es => {
        val unwrap = run(es)(a).get
        run(es)(unwrap)
    }
}

object Chapter7 {
    
}