package functional.modular

import scala.collection.mutable.Buffer

/** A mini-framework for trace-based testing of interactive behavior. */
object Tracing {

  sealed trait TraceEvent[Input, Result]
  case class InputEvent[Input, Result](value: Input) extends TraceEvent[Input, Result]
  case class OutputEvent[Input, Result](value: Result) extends TraceEvent[Input, Result]

  /**
   * Invokes the original run method on the instrumented input,
   * instruments the output, and returns the trace.
   */
  def runWithTracing[Input, Result](run: Task[Input, Result])(input: Iterator[Input]): Seq[TraceEvent[Input, Result]] = {
    val trace = Buffer.empty[TraceEvent[Input, Result]]
    val tracedInput = input.map { s => trace.append(InputEvent(s)); s }
    run(tracedInput).foreach { s => trace.append(OutputEvent(s)) }
    trace.toSeq
  }
}
