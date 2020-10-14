package imperative.modular

/** Defines a dependency (plug-in contract) on an output method (Observer). */
trait Output[Result] {
  def doOutput(result: Result): Unit
}

/** Provides a reusable output observer tied to println/stdout. */
trait OutputToStdOut[Result] extends Output[Result] {
  override def doOutput(result: Result): Unit = println(result)
}

/** Defines a dependency (plug-in contract) on a run method that processes an input stream. */
trait Task[Input] {
  def run(input: Iterator[Input], args: Array[String] = Array.empty): Unit
}

/**
 * Provides a reusable main task tied to stdin and stdout.
 * Depends on a suitable run method.
 */
trait Main[Result] extends Task[String] with OutputToStdOut[Result] {
  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.stdin.getLines()
    run(lines, args)
  }
}