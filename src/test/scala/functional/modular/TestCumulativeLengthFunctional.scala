package functional.modular

import org.scalatest.wordspec.AnyWordSpec

class TestCumulativeLengthFunctional extends AnyWordSpec {

  /** Refers to the existing immutable SUT instance. */
  val sut = CumulativeLengthFunctionalModular

  "The functional CumulativeLength calculator" when {
    "given an empty iterator" should {
      "produce an empty output" in {
        // exercise SUT
        val result: Iterator[(String, Int)] = sut.run(Iterator.empty)
        // check resulting iterator
        assert(result.isEmpty)
      }
    }

    "given a nonempty iterator" should {
      "produce the correct nonempty output" in {
        // input data for this test case
        val data = Seq("hello", "world", "what", "up")
        val expected = data.zip(Seq(5, 10, 14, 16))
        // exercise SUT
        val actual: Iterator[(String, Int)] = sut.run(data.iterator)
        // check resulting iterator
        assert(actual.toSeq === expected)
      }
    }

    // TODO test incremental correctness
  }
}