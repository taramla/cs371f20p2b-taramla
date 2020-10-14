package functional.modular

import org.scalatest.wordspec.AnyWordSpec

class TestLineCountFunctional extends AnyWordSpec {

  /** Refers to the existing immutable SUT instance. */
  val sut = LineCountFunctionalModular

  "The functional LineCounter" when {
    "given an empty iterator" should {
      "produce an empty output" in {
        // exercise SUT
        val result: Iterator[(Int, String)] = sut.run(Iterator.empty)
        // check effect on output observer
        assert(result.isEmpty)
      }
    }

    "given a nonempty iterator" should {
      "produce the correct nonempty output" in {
        // input data for this test case
        val data = Seq("3", "5", "6", "7")
        // exercise SUT
        val result: Iterator[(Int, String)] = sut.run(data.iterator)
        // check effect on output observer
        assert(result.toSeq === (1 to data.length).zip(data))
      }
    }

    "given a nonempty iterator" should {
      "exhibit the correct interactive behavior" in {
        // input data for this test case
        val input = Iterator("3", "4", "5", "6")
        // exercise SUT
        val trace = Tracing.runWithTracing(sut.run)(input)
        // check correctness of resulting interactions
        import Tracing.{ InputEvent => i, OutputEvent => o }
        assert(trace === Seq(
          i("3"), o((1, "3")),
          i("4"), o((2, "4")),
          i("5"), o((3, "5")),
          i("6"), o((4, "6"))))
      }
    }
  }
}