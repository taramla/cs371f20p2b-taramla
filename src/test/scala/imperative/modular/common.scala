package imperative.modular

import scala.collection.mutable.Buffer

/** Provides an output observer that accumulates the results in a buffer one can inspect later. */
trait OutputToBuffer[Result] extends Output[Result] {

  private val buffer = Buffer.empty[Result]

  def getResults: Seq[Result] = buffer.toSeq

  override def doOutput(result: Result) = { buffer.append(result) }
}

/** A mini-framework for trace-based testing of interactive behavior. */
trait Tracing[Input, Result] extends Task[Input] with Output[Result] {

  sealed trait TraceEvent
  case class InputEvent(value: Input) extends TraceEvent
  case class OutputEvent(value: Result) extends TraceEvent

  val trace = Buffer.empty[TraceEvent]

  /** Instruments the input such that accessing the item appends the corresponding event to the trace. */
  protected def traced(input: Iterator[Input]) = input.map { s => trace.append(InputEvent(s)); s }

  /** Adds the output to the trace. */
  override def doOutput(result: Result) = trace.append(OutputEvent(result))

  /**
   * Invokes the original run method on the instrumented input.
   * `abstract override` lets us override a run method that is still abstract
   * as we are defining this trait but will be available later from a provider such as `CountLines`.
   */
  abstract override def run(input: Iterator[Input], args: Array[String]) = super.run(traced(input), args)
}
