package functional.modular
import imperative.modular.movingStats.show
import sun.misc.Signal
import scala.collection.immutable

object movingStats {
  type Stat = (Double, Double, Double, Double)
  type OptionalStat = Option[Stat]
  def show(x: movingStats.OptionalStat): String = x match {
    case Some(s) => s._1 + " " + s._2 + " " + s._3 + " " + s._4
    case None => " ? ? ? ?"

  }
  type Stats = (Double, Int, Seq[OptionalStat])
}

/** Reads lines and prints line count along with line itself. */
object LineCountFunctionalModular extends App {

  //runWithStdIO(run, args)

  val lines = scala.io.Source.stdin.getLines()
  val result = run(lines, args)
  result.foreach { r => output(r) }

  def run(lines: Iterator[String], args: Array[String] = Array.empty): Iterator[movingStats.Stats] = {
    if (!System.getProperty("os.name").contains("Windows"))
      Signal.handle(new Signal("PIPE"), _ => scala.sys.exit())

    val input = lines.flatMap(_.split("\\W"))
    val num = input.map(_.toDouble)
    val windowSizes = args.toSeq.map(_.toInt)

    val queues = num.scanLeft {

      (immutable.Queue.empty[Double], 0)
    } {
      case ((queue, count), next) =>
        val r = queue.enqueue(next)
        val MaxWindowSize = windowSizes.max
        val s = if (r.size > MaxWindowSize)
          r.dequeue._2
        else r
        (s, count + 1)
    }
    queues.drop(1).map {
      case (queue, count) =>
        (queue.last, count, windowSizes.map(w => calculation(queue, w)))

    }
  }
  def calculation(queue: immutable.Queue[Double], n: Int): Option[(Double, Double, Double, Double)] = {

    if (queue.size < n) return None

    val queue1 = queue.takeRight(n)


    val min = queue1.min
    val max = queue1.max
    val sum = queue1.sum
    val avg = sum / n

    val variance = queue1.map(a => math.pow(a - avg, 2)).sum / queue1.size
    val stdDev = math.sqrt(variance)

    Option(min, avg, max, stdDev)
  }

  def output(result: movingStats.Stats): Unit = {
    print(result._1 + " ")
    print(result._2 + " ")
    result._3.foreach(s => print(show(s)))
    println()
  }
}

