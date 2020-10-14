package functional.simple

/** Reads lines and prints line count along with line itself. */
object LineCountFunctional extends App {

  val lines = scala.io.Source.stdin.getLines()

  val counts = Iterator.from(1)
  val results = counts.zip(lines)

  results.foreach { r => println(r) }
}
