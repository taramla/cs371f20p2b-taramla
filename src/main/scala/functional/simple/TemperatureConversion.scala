package functional.simple

object TemperatureConversion extends App {
  val lines = scala.io.Source.stdin.getLines()
  val values = lines.map(_.toDouble)
  val results = values.map(r => math.round(r / 20))
  results.foreach { println(_) }
}
