package functional.modular

import org.scalameter.api._

object BenchmarkCumulativeLengthFunctional extends Bench.LocalTime {

  val sut = LineCountFunctionalModular

  val sizes = Gen.exponential("size")(1000, 10000000, 10)

  performance of "CumulativeLengthFunctional" in {
    measure method "run" in {
      using(sizes) in {
        size => sut.run(Iterator.fill(size)("hello")).length
      }
    }
  }
}
