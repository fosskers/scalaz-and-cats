package svc

import scalaz._
import scalaz.syntax.monad._ /* Brings in `>>` operator */
import scalaz.Free.Trampoline
import scalaz.syntax.std.option._  /* Brings in `.some` */
import scalaz.std.option._  /* Brings in `|@|` for Option */

// --- //

object Zed {

  /* --- STATE --- */

  /** No extra `value` call necessary like in cats.
    *
    * Note: (TODO: What is State an alias for?)
    */
  def oneGet: (Int, Int) = State.get.run(1)

  /** We can't get rid of the explicit type param here or in cats. */
  def bind: State[Int, Unit] = State.get[Int] >>= { n => State.put(n + 1) }

  def flatmap: State[Int, Unit] = State.get.flatMap(n => State.put(n + 1))

  /** While this works fine in cats, in Scalaz this code blows the JVM stack
    * even at low inital State values (1000).
    *
    * Scalaz opted for `state` instead of `pure`, but kept Haskell's `put`.
    */
  def badCountdown: State[Int, Int] = State.get.flatMap({ n =>
    if (n <= 0) State.state(n) else State.put(n - 1) >> badCountdown
  })

  /** The only way to get the countdown to work with State.
    *
    * WART: What *should* be an implementation detail (Trampoline) is exposed to
    * the user and made necessary.
    *
    * WART: Doing the "Statey" operations requires using the `State` object
    * (not StateT), then explicitely lifting through the stacked monad (Trampoline).
    * `State.get.lift[Trampoline].flatMap` is *very* verbose just for a simple
    * `get` call.
    */
  def trampolineCountdown: StateT[Trampoline, Int, Int] = State.get.lift[Trampoline].flatMap({ n =>
    if (n <= 0) StateT(_ => Trampoline.done((n,n)))
    else State.put(n - 1).lift[Trampoline] >> trampolineCountdown
  })

  /** An extra `run` is necessary here to escape the Trampoline. */
  def runTrampoline: (Int, Int) = trampolineCountdown.run(10000).run

  /* --- APPLICATIVE --- */

  /** We don't `map` into the Applicative like in cats. */
  def dumbSum(nums: List[Int]): Option[Int] = nums match {
    case Nil => Some(0)
    case n :: ns => (n.some |@| dumbSum(ns)) { _ + _ }
  }

  /** Argument order for `<*>` seems backward from Haskell.
    * Cats doesn't have a `<*>`, but they do have `ap`.
    * Using `|@|` is favoured, however.
    *
    * WART: Type handholding in the lambda.
    * WART: Operator precedence is borked, such that you have to use parens in
    * order to ensure the correct order of operations. Otherwise the code won't
    * compile.
    */
  def dumbSum2(nums: List[Int]): Option[Int] = nums match {
    case Nil => Some(0)
    case n :: ns => dumbSum2(ns) <*> (n.some <*> { (a: Int) => (b: Int) => a+b }.some)
  }
}
