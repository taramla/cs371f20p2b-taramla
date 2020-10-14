package imperative.modular

/**
 * Provides a main method for reading lines and printing the cumulative length
 * of all lines so far along with the most recent line itself.
 * Depends on a suitable Output provider.
 */
trait AccumulateLength extends Task[String] with Output[(String, Int)] {

  def run(input: Iterator[String]) = {
    var line = "dummy"
    var length = 0
    for (next <- input) {
      line = next
      length = length + next.length
      doOutput((line, length))
    }
  }
}

/** A concrete main application object. */
object CumulativeLengthImperativeModular extends Main[(String, Int)] with AccumulateLength {
  override def run(input: Iterator[String], args: Array[String]): Unit = ???
}