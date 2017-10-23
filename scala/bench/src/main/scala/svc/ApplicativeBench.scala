package svc

import java.util.concurrent.TimeUnit

import cats.implicits._
import org.openjdk.jmh.annotations._
import scalaz.Scalaz._

// --- //

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class ApplicativeBench {
  var list: List[Int] = _

  @Setup
  def setup(): Unit = {
    list = List.range(1, 1000)
  }

  /* Silly summing using Applicative functors */
  @Benchmark
  def cartesianSumCats: Option[Int] = Kitties.dumbSum(list)

  @Benchmark
  def applicativeSumCats: Option[Int] = Kitties.dumbSum2(list)

  @Benchmark
  def cartesianSumScalaz: Option[Int] = Zed.dumbSum(list)

  @Benchmark
  def applicativeSumScalaz: Option[Int] = Zed.dumbSum2(list)

}
