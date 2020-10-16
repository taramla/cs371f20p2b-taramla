package functional.modular

import org.scalatest.wordspec.AnyWordSpec

import scala.collection.compat.immutable.ArraySeq

class TestLineCountFunctional extends AnyWordSpec {

  /** Refers to the existing immutable SUT instance. */
  val sut = LineCountFunctionalModular

  "The functional LineCounter" when {
    "given an empty iterator" should {
      "produce an empty output" in {
        // exercise SUT
        val result: Iterator[movingStats.Stats] = sut.run(Iterator.empty)
        // check effect on output observer
        assert(result.isEmpty)
      }
    }

    "given a nonempty iterator" should {
      "produce the correct nonempty output" in {
        // input data for this test case
        val args = new Array[String](2)
        val data = Seq("3", "5", "6", "7")
        // exercise SUT
        val result: Iterator[movingStats.Stats] = sut.run(data.iterator, args)
        // check effect on output observer
        assert(result.toSeq === (1 to data.length).zip(data))
      }
    }

    "given a nonempty iterator" should {
      "exhibit the correct interactive behavior" in {
        // input data for this test case
        val input = Iterator("3", "4", "5")
        val args = Array("3")
        // exercise SUT
        val trace = Tracing.runWithTracing(sut.run)(input, args)
        // check correctness of resulting interactions
        import Tracing.{ InputEvent => i, OutputEvent => o }
        assert(trace === Seq(
          i("3"), o((3.0,1,ArraySeq(None))),
          i("4"), o((4.0,2,ArraySeq(None)),
          i("5"), o((5.0,3,ArraySeq(Some((3.0,4.0,5.0,0.81649658092773))))))))
      }
    }
  }
}