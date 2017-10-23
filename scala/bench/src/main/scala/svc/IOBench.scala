package svc

import java.util.concurrent.TimeUnit

import cats.implicits._
import org.openjdk.jmh.annotations._
import scalaz.Scalaz._

// --- //

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class IOBench {

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
