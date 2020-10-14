package imperative.simple

object CumAvgImperative extends App {
  var count = 0
  var sum = 0.0
  var line = scala.io.StdIn.readLine()
  while (line != null) {
    count += 1
    sum += line.toDouble
    val avg = sum / count
    println(s"$count: $avg")
    line = scala.io.StdIn.readLine()
  }
}
