package svc

import java.util.concurrent.TimeUnit

import cats.implicits._
import org.openjdk.jmh.annotations._
import scalaz.Scalaz._

// --- //

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class MonoidBench {
  var intList: List[Int] = _
  var optList: List[Option[Int]] = _

  @Setup
  def setup(): Unit = {
    intList = List.range(1, 1000)
    optList = intList.map(Some(_))
  }

  @Benchmark
  def foldableScalazL: Int = Zed.combineAllL(intList)
  @Benchmark
  def foldableScalazR: Int = Zed.combineAllR(intList)
  @Benchmark
  def foldableCats: Int = Kitties.combineAll(intList)
  @Benchmark
  def foldableVanillaSum: Int = intList.sum
  @Benchmark
  def foldableVanillaFoldL: Int = intList.foldLeft(0)(_ + _)

  @Benchmark
  def foldableScalazLMaybes: Option[Int] = Zed.combineAllL(optList)
  @Benchmark
  def foldableScalazRMaybes: Option[Int] = Zed.combineAllR(optList)
  @Benchmark
  def foldableCatsMaybes: Option[Int] = Kitties.combineAll(optList)

}
