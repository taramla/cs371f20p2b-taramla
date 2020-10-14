# Overview
ATTEMPTED extra credit: standard deviation 

Examples of using Scala iterators to implement efficient Unix-style filters.

# Objectives

An understanding of

- stream processing (finite vs. infinite/unbounded)
- pipes and filters architecture
- representing streams using the Iterator design pattern
- immutability
- separation of processing and I/O concerns
- testability using the Observer design pattern
- time/space complexity and scalability
- performance benchmarking

# Prerequisites

- Java Development Kit (JDK) through your package management system or from [Oracle](http://www.oracle.com/technetwork/java/javase/downloads)
- [sbt](http://www.scala-sbt.org/)

These really are the only required prerequisites.

# Running the tests

This will run the unit tests along with the performance benchmarks.

    $ sbt test

# Running the sample applications

During development:

    $ sbt run

Or choose one of the following:

    $ sbt 'runMain imperative.simple.LineCount'
    $ sbt 'runMain imperative.modular.CumulativeLength'
    ...

During production, first create the startup scripts:

    $ sbt stage

Then run the filters outside of sbt like this:

    $ ./target/universal/stage/bin/line-count-imperative
    hello
    (1,hello)
    world
    (2,world)
    what
    (3,what)
    up
    (4,up)

    $ yes hello | head -n 4 | ./target/universal/stage/bin/line-count-imperative
    (1,hello)
    (2,hello)
    (3,hello)
    (4,hello)

    $ ./target/universal/stage/bin/cumulative-length-imperative
    hello
    (5,hello)
    world
    (10,world)
    what
    (14,what)
    up
    (16,up)

    $ yes hello | head -n 4 | ./target/universal/stage/bin/cumulative-length-imperative
    (5,hello)
    (10,hello)
    (15,hello)
    (20,hello)
