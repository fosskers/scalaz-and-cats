package svc

import java.util.concurrent.TimeUnit

import cats.implicits._
import org.openjdk.jmh.annotations._
import scalaz.Scalaz._

// --- //

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class StateBench {

  /* A single call to `get`, which fetches the current "stateful" value */
  @Benchmark
  def oneGetCats: (Int, Int) = Kitties.oneGet

  @Benchmark
  def oneGetScalaz: (Int, Int) = Zed.oneGet

  /* Binding speed */
  @Benchmark
  def bindCats: (Int, Unit) = Kitties.bind.run(1).value

  @Benchmark
  def flatMapCats: (Int, Unit) = Kitties.flatmap.run(1).value

  @Benchmark
  def bindScalaz: (Int, Unit) = Zed.bind.run(1)

  @Benchmark
  def flatMapScalaz: (Int, Unit) = Zed.flatmap.run(1)

  /* Count down by assigning each `n` to the State */
  @Benchmark
  def countdownCats: (Int, Int) = Kitties.countdown.run(10000).value

  @Benchmark
  def countdownScalaz: (Int, Int) = Zed.trampolineCountdown.run(10000).run

  @Benchmark
  def countdownCatsStateTEitherT: Either[String, (Int, Unit)] =
    Kitties.countdownT.run(10000).value.value

}
