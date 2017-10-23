package svc

import java.util.concurrent.TimeUnit

import cats.implicits._
import org.openjdk.jmh.annotations._
import scalaz.Scalaz._

// --- //

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class EqualBench {
  var list0: List[Int] = _
  var list1: List[Int] = _

  @Setup
  def setup(): Unit = {
    list0 = List.range(1, 1000)
    list1 = List.range(1, 1000)
  }

  @Benchmark
  def equalSameVanilla: Boolean = list0 == list0
  @Benchmark
  def equalDiffVanilla: Boolean = list0 == list1

  @Benchmark
  def equalSameCats: Boolean = Kitties.equalAll(list0, list0)
  @Benchmark
  def equalDiffCats: Boolean = Kitties.equalAll(list0, list1)

  @Benchmark
  def equalSameScalaz: Boolean = Zed.equalAll(list0, list0)
  @Benchmark
  def equalDiffScalaz: Boolean = Zed.equalAll(list0, list1)

}
