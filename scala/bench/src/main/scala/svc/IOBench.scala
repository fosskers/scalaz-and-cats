package svc

import java.util.concurrent.TimeUnit

import cats.implicits._
import org.openjdk.jmh.annotations._
import scalaz.Scalaz._
import scalaz.ioeffect.{IO => ZIO}
import cats.effect.{IO => CIO}
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

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
  def ioScalaz0: Int = Zed.unsafePerformIO(Zed.recurseIO(1000))
  @Benchmark
  def ioScalaz1: Int = Zed.unsafePerformIO(Zed.recurseIO(10000))
  @Benchmark
  def ioScalaz2: Int = Zed.unsafePerformIO(Zed.recurseIO(100000))

  @Benchmark
  def future0: Int = Await.result(future(1000), 10 seconds)
  @Benchmark
  def future1: Int = Await.result(future(10000), 10 seconds)
  @Benchmark
  def future2: Int = Await.result(future(100000), 10 seconds)

  def future(n: Int): Future[Int] = n match {
    case 0 => Future.successful(0)
    case n => Future.successful(n - 1).flatMap(future)
  }

  def resolveZ[E](e: E): ZIO[Void, Int] = ZIO.now(1)
  def resolveC[E](e: Either[E, Int]): CIO[Int] = CIO(1)

  @Benchmark
  def countdownScalaz0: Int = Zed.unsafePerformIO(Zed.ioCountdown(1000).catchAll(resolveZ))
  @Benchmark
  def countdownScalaz1: Int = Zed.unsafePerformIO(Zed.ioCountdown(10000).catchAll(resolveZ))
  @Benchmark
  def countdownScalaz2: Int = Zed.unsafePerformIO(Zed.ioCountdown(100000).catchAll(resolveZ))

  @Benchmark
  def countdownCats0: Int = (Kitties.ioCountdown(1000).value >>= resolveC).unsafeRunSync
  @Benchmark
  def countdownCats1: Int = (Kitties.ioCountdown(10000).value >>= resolveC).unsafeRunSync
  @Benchmark
  def countdownCats2: Int = (Kitties.ioCountdown(100000).value >>= resolveC).unsafeRunSync

  @Benchmark
  def countdownEScalaz0: Int = Zed.unsafePerformIO(Zed.ioCountdownE(1000).catchAll(resolveZ))
  @Benchmark
  def countdownEScalaz1: Int = Zed.unsafePerformIO(Zed.ioCountdownE(10000).catchAll(resolveZ))
  @Benchmark
  def countdownEScalaz2: Int = Zed.unsafePerformIO(Zed.ioCountdownE(100000).catchAll(resolveZ))

  @Benchmark
  def countdownECats0: Int = (Kitties.ioCountdownE(1000).attempt >>= resolveC).unsafeRunSync
  @Benchmark
  def countdownECats1: Int = (Kitties.ioCountdownE(10000).attempt >>= resolveC).unsafeRunSync
  @Benchmark
  def countdownECats2: Int = (Kitties.ioCountdownE(100000).attempt >>= resolveC).unsafeRunSync

  def futCountdownE(n: Int): Future[Int] = n match {
    case 0 => Future.failed(new Exception)
    case n => Future.successful(n - 1).flatMap(futCountdownE)
  }

  @Benchmark
  def futureE0: Int = Await.result(futCountdownE(1000).recover { case _ => 1 }, 10 seconds)
  @Benchmark
  def futureE1: Int = Await.result(futCountdownE(10000).recover { case _ => 1 }, 10 seconds)
  @Benchmark
  def futureE2: Int = Await.result(futCountdownE(100000).recover { case _ => 1 }, 10 seconds)
}
