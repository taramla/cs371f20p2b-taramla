package imperative.simple

/** Reads lines and prints line count along with line itself. */
object LineCountImperative extends App {

  import sun.misc.Signal
  if (!System.getProperty("os.name").contains("Windows"))
    Signal.handle(new Signal("PIPE"), _ => scala.sys.exit())

  var count = 0

  for (line <- scala.io.Source.stdin.getLines()) {
    count += 1
    println((count, line))
  }
}
