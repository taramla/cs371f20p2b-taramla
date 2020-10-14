package imperative.modular
import imperative.modular.movingStats.show
import sun.misc.Signal

import scala.collection.mutable

/**
 * Provides a main method for reading lines and printing line count along with line itself.
 * Depends on a suitable Output provider.
 */

object movingStats {
  type Stat = (Double, Double, Double, Double)
  type OptionalStat = Option[Stat]
  def show(x: movingStats.OptionalStat): String = x match {
    case Some(s) => s._1 + " " + s._2 + " " + s._3 + " " + s._4
    case None => " ? ? ? ?"

  }
  type Stats = (Double, Int, Seq[OptionalStat])
}

trait movingStats extends Task[String] with Output[movingStats.Stats] {

  override def run(input: Iterator[String], args: Array[String]): Unit = {
    if (!System.getProperty("os.name").contains("Windows"))
      Signal.handle(new Signal("PIPE"), _ => scala.sys.exit())

    var count = 0
    val queue: mutable.Queue[Double] = mutable.Queue.empty[Double]
    val words = input.flatMap(_.split("\\W"))
    val num = words.map(_.toDouble)
    val windowSizes = args.map(_.toInt)
    var MaxWindowSize = 1

    if (windowSizes.length > 0) {
      MaxWindowSize = windowSizes.max
    }

    for (n <- num) {
      if (n <= 0) {
        System.err.println("Input must be a natural number ")
        System.exit(1)
      }
      count += 1

      if (queue.length >= MaxWindowSize) {
        queue.dequeue()
      }
      queue.enqueue(n)

      val columns = windowSizes.map(w => calculation(queue, w))

      doOutput((n, count, columns.toSeq))
    }

  }

  def calculation(queue: mutable.Queue[Double], n: Int): Option[(Double, Double, Double, Double)] = {
    // TODO add logic for window size
    if (queue.size < n) return None

    val queue1 = queue.takeRight(n)

    var squaredSum = 0.0
    val min = queue1.min
    val max = queue1.max
    val sum = queue1.sum
    val avg = sum / n

    val squared = ((n - avg) * (n - avg)) * ((n - avg) * (n - avg))
    squaredSum += squared
    val variance = squaredSum * (1 / sum)
    val stdDev = math.sqrt(variance)

    Option(min, avg, max, stdDev)
  }
}

trait OutputMovingStats extends Output[movingStats.Stats] {
  override def doOutput(result: movingStats.Stats): Unit = {
    print(result._1 + " ")
    print(result._2 + " ")
    result._3.foreach(s => print(show(s)))
    println()
  }

}

/*** A concrete main application object. */
object movingStatsModular extends movingStats with OutputMovingStats {
  def main(args: Array[String]): Unit = {
    val lines = scala.io.Source.stdin.getLines()
    run(lines, args)
  }
}
