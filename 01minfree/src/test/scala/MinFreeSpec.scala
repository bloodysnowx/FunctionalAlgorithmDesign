import org.scalatest._
import org.scalatest.Matchers._

trait MinFreeSpec extends FunSpec {
  def solver: MinFreeSolver

  describe("searchMinFree") {
    def testSearchMinFree(args: Seq[Int], expected: Int) = {
      it(args.toString + " -> " + expected.toString) {
        solver.minFree(args) should be(expected)
      }
    }

    testSearchMinFree(Seq(), 0)
    testSearchMinFree(Seq(0), 1)
    testSearchMinFree(Seq(1), 0)
    testSearchMinFree(Seq(2, 1, 0), 3)
    testSearchMinFree(Seq(0, 2, 3, 1, 5, 8, 9), 4)
  }
}

class ArrayMinFreeSpec extends MinFreeSpec {
  lazy val solver = ArrayMinFree
}

class DivideMinFreeSpec extends MinFreeSpec {
  lazy val solver = DivideMinFree
}