package imperative.modular

//import org.scalameter.api._
//
//object BenchmarkCumulativeLengthImperative extends Bench.LocalTime {
//
//  val sut = new AccumulateLength {
//    var length = 0
//    override def doOutput(result: (String, Int)): Unit = { length += 1 }
//  }
//
//  val sizes = Gen.exponential("size")(1000, 10000000, 10)
//
//  performance of "CumulativeLengthImperative" in {
//    var args = 3
//    measure method "run" in {
//      using(sizes) in {
//        size => sut.run(Iterator.fill(size)("hello"), args )
//      }
//    }
//  }
//}
