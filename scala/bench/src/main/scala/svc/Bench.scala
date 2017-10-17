package svc

import java.util.concurrent.TimeUnit

import cats.implicits._
import org.openjdk.jmh.annotations._
import scalaz.Scalaz._

// --- //

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class Bench {
  /* Speed to render to String */
  @Benchmark
  def showVanilla: String = List.range(1, 1000).toString

  @Benchmark
  def showCats: String = Kitties.showAll(List.range(1, 1000))

  @Benchmark
  def showScalaz: String = Zed.showAll(List.range(1, 1000))

  /* Speed to test equality */
  @Benchmark
  def equalVanilla: Boolean = {
    val list = List.range(1, 1000)

    list == list
  }

  @Benchmark
  def equalCats: Boolean = {
    val list = List.range(1, 1000)

    Kitties.equalAll(list, list)
  }

  @Benchmark
  def equalScalaz: Boolean = {
    val list = List.range(1, 1000)

    Zed.equalAll(list, list)
  }

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

  /* Side Effects */
  @Benchmark
  def ioCats0: Int = Kitties.recurseIO(1000).unsafeRunSync
  @Benchmark
  def ioCats1: Int = Kitties.recurseIO(10000).unsafeRunSync
  @Benchmark
  def ioCats2: Int = Kitties.recurseIO(100000).unsafeRunSync

  @Benchmark
  def ioScalaz0: Int = Zed.recurseIO(1000).unsafePerformIO
  @Benchmark
  def ioScalaz1: Int = Zed.recurseIO(10000).unsafePerformIO
  @Benchmark
  def ioScalaz2: Int = Zed.recurseIO(100000).unsafePerformIO
}
