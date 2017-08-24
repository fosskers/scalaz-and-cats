package svc

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._

// --- //

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class Bench {
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

  /* Silly summing using Applicative functors */
  @Benchmark
  def cartesianSumCats: Option[Int] = Kitties.dumbSum(List.range(0, 1000))

  @Benchmark
  def applicativeSumCats: Option[Int] = Kitties.dumbSum2(List.range(0, 1000))

  @Benchmark
  def cartesianSumScalaz: Option[Int] = Zed.dumbSum(List.range(0, 1000))

  @Benchmark
  def applicativeSumScalaz: Option[Int] = Zed.dumbSum2(List.range(0, 1000))
}
