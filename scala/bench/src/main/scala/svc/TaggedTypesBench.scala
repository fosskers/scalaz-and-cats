package svc

import java.util.concurrent.TimeUnit

import cats.implicits._
import org.openjdk.jmh.annotations._
import scalaz.Scalaz._

// --- //

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class TaggedTypesBench {
  // var tile0: Zed.Tile[Zed.UInt] = _
  var tile1: Zed.Tile[Int] = _
  var tile2: Zed.Tile[Zed.Word] = _


  @Setup
  def setup(): Unit = {
    // tile0 = Array.range(1, 10000).map(Zed.Unsigned(_))
    tile1 = Array.range(1, 10000)
    tile2 = Array.range(1, 10000).map(new Zed.Word(_))
  }

  // @Benchmark
  // def withUIntFold: Zed.UInt = Zed.tileSumFoldUInt(tile0)
  @Benchmark
  def withIntFold: Int = Zed.tileSumFoldInt(tile1)
  @Benchmark
  def withWordFold: Zed.Word = Zed.tileSumFoldWord(tile2)
  // @Benchmark
  // def withUIntWhile: Zed.UInt = Zed.tileSumWhileUInt(tile0)
  @Benchmark
  def withIntWhile: Int = Zed.tileSumWhileInt(tile1)
  @Benchmark
  def withWordWhile: Zed.Word = Zed.tileSumWhileWord(tile2)
}
