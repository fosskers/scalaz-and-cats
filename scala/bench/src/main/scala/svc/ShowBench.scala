package svc

import java.util.concurrent.TimeUnit

import cats.implicits._
import org.openjdk.jmh.annotations._
import scalaz.Scalaz._

// --- //

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class ShowBench {
  var list: List[String] = _
  var str: String = _

  @Setup
  def setup(): Unit = {
    list = List.range(1, 1000).map(_.toString)
    str = "How fast is this" * 1000
  }

  /* Render a large List into a single String */
  @Benchmark
  def showListVanilla: String = list.toString
  @Benchmark
  def showListCats: String = Kitties.show(list)
  @Benchmark
  def showListScalaz: String = Zed.show(list)

  /* Render a String... into a String. Is this just `id`? */
  @Benchmark
  def showStrVanilla: String = str.toString
  @Benchmark
  def showStrCats: String = Kitties.show(str)
  @Benchmark
  def showStrScalaz: String = Zed.show(str)
  @Benchmark
  def showCordStrScalaz: scalaz.Cord = Zed.showCord(str)
  @Benchmark
  def showCordStringStrScalaz: String = Zed.showCord(str).toString

}
