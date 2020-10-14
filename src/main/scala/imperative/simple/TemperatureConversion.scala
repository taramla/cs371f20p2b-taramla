package imperative.simple

object TemperatureConversion extends App {
  var line = scala.io.StdIn.readLine()
  while (line != null) {
    val raw = line.toInt
    val celsius = math.round(raw.toFloat / 20)
    println(celsius)
    line = scala.io.StdIn.readLine()
  }
}