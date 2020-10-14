package functional.simple

/** Reads lines and prints cumulative length of all lines so far along with most recent line itself. */
object CumulativeLengthFunctional extends App {

  val lines = scala.io.Source.stdin.getLines()

  def accumulateCount(acc: (String, Int), next: String): (String, Int) =
    (next, acc._2 + next.length)

  val results = lines.scanLeft("dummy", 0)(accumulateCount).drop(1)

  results.foreach { r => println(r) }
}
