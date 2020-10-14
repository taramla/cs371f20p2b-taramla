package functional.simple

object CumAvgFunctional extends App {
  val lines = scala.io.Source.stdin.getLines()
  // convert each input line to a number
  val values = lines.map(_.toDouble)
  // for each number, produce a count and a cumulative sum
  val countsWithSums = values.scanLeft((0, 0.0)) {
    case ((count, sum), value) => (count + 1, sum + value)
  }
  // convert the count and sum pairs to count and cumulative average pairs
  val countsWithAvgs = countsWithSums.map {
    case (count, sum) => (count, sum / count)
  }
  // print the results except for the first one
  countsWithAvgs.drop(1).foreach {
    case (count, avg) => println(s"$count: $avg")
  }
}
