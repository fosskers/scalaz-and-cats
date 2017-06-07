package svc

import scaliper.{ Benchmark, Benchmarks, ConsoleReport }

// --- //

class SVCBench extends Benchmarks with ConsoleReport {
  benchmark("State") {

    /* A single call to `get`, which fetches the current "stateful" value */
    run("cats/one get") { new Benchmark { def run() = Kitties.oneGet } }
    run("scalaz/one get") { new Benchmark { def run() = Zed.oneGet } }

    /* Binding speed */
    run("cats/bind") { new Benchmark { def run() = Kitties.bind.run(1).value }}
    run("cats/flatmap") { new Benchmark { def run() = Kitties.flatmap.run(1).value }}
    run("scalaz/bind") { new Benchmark { def run() = Zed.bind.run(1) }}
    run("scalaz/flatmap") { new Benchmark { def run() = Zed.flatmap.run(1) }}

    /* Count down by assigning each `n` to the State */
    run("cats/countdown") { new Benchmark { def run() = Kitties.countdown.run(10000).value }}

    run("scalaz/trampoline-countdown") { new Benchmark {
      def run() = Zed.trampolineCountdown.run(10000).run
    }}

    /* Countdown with StateT/EitherT combination */
    run("cats/StateT[EitherT]") { new Benchmark {
      def run() = Kitties.countdownT.run(10000).value.value
    }}

    run("cats/applic-sum") { new Benchmark {
      def run() = Kitties.dumbSum(List.range(0, 1000))
    }}

    run("scalaz/applic-sum") { new Benchmark {
      def run() = Zed.dumbSum(List.range(0, 1000))
    }}

    run("scalaz/applic-sum2") { new Benchmark {
      def run() = Zed.dumbSum2(List.range(0, 1000))
    }}

    /*
    type EitherStr2[T] = scalaz.EitherT[scalaz.Id.Id, String, T]
    type Statey2[T] = scalaz.StateT[({ type λ[α] = scalaz.EitherT[scalaz.Id.Id, String, α] })#λ, Int, T]

    run("scalaz/StateT-EitherT") { new Benchmark {

      def go: Statey2[Int] = scalaz.State.get.lift[EitherStr2].flatMap({ n: Int =>
        if (n <= 0) ??? else scalaz.State.put(n - 1).lift[EitherStr2] >> go
      })

//      val res: Statey2[Int] = scalaz.State.get.lift[EitherStr2]
//      val foo: EitherStr2[Int] = scalaz.MonadError[EitherStr2, String].raiseError("crape")
//      val bar: Statey2[Int] = scalaz.MonadError[Statey2, String].raiseError("noo")

      def run() = ??? //go.run(100)
    }}
     */
  }
}
