package functional.modular

/** Reads lines and prints cumulative length of all lines so far along with most recent line itself. */
object CumulativeLengthFunctionalModular extends App {

  runWithStdIO(run, args)

  def accumulateCount(acc: (String, Int), nextItem: String): (String, Int) =
    (nextItem, acc._2 + nextItem.length)

  def run(lines: Iterator[String], args: Array[String] = Array.empty): Iterator[(String, Int)] = {
    if (args.length > 0) {
      System.err.println("args: " + args.toSeq)
    }
    lines.scanLeft("dummy", 0)(accumulateCount).drop(1)
  }
}