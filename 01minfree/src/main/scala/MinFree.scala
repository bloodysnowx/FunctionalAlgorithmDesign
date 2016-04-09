object MinFree extends App {
  println(DivideMinFree.minFree(Seq(0, 1, 3, 5, 2, 9, 8, 7, 6, 10)))
}

trait MinFreeSolver {
  def minFree(xs: Seq[Int]): Int
}

object ArrayMinFree extends MinFreeSolver {
  override def minFree(xs: Seq[Int]): Int = {
    val bits = scala.collection.mutable.BitSet.empty
    xs.foreach(bits.add)
    xs.indices.find { bits.contains(_) == false }.getOrElse(xs.length)
  }
}

object DivideMinFree extends MinFreeSolver {
  override def minFree(xs: Seq[Int]): Int = minFrom(0, xs.length, xs)

  @scala.annotation.tailrec
  private def minFrom(a: Int, n: Int, xs: Seq[Int]): Int = {
    // println("a = " + a.toString + ", n = " + n.toString + ", xs = " + xs.toString)

    if(n == 0) return a

    val b = a + 1 + n / 2
    val (us, vs) = xs.partition(b.>)
    val m = us.length

    if(m == b - a) minFrom(b, n - m, vs) else minFrom(a, m, us)
  }
}