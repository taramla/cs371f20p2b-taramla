package functional

/**
 * Provides a reusable method for invoking a task with stdin and stdout.
 * Depends on a suitable task (run method) that transforms an input stream to an output stream.
 */
package object modular {

  type Task[-Input, +Result] = Iterator[Input] => Iterator[Result]

  def runWithStdIO[Result](run: Task[String, Result]): Unit = {
    val lines = scala.io.Source.stdin.getLines()
    val result = run(lines)
    result.foreach { r => println(r) }
  }
}