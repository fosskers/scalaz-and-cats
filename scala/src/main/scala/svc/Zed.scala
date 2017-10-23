package svc

import scalaz._
import Scalaz._  /* This is easiest. Fighting with the "a la carte" import style is much harder */
import scalaz.effect._  /* Requires a separate dep, scalaz-effect */
import scalaz.Free.Trampoline

// --- //

object Zed {

  /* --- SHOW --- */

  /** The `Show` typeclass gives us a static guarantee that it makes sense
    * to convert a type to a String. This is not always true for any given type,
    * but the `toString` method that all types get from `Any` lets this happen.
    * For instance, should we be able to `toString` a `Future`? You can:

    * {{{
    * scala> Future(println("hi")).toString
    * res3: String = Future(<not completed>)hi
    * }}}
    *
    * How about a function?
    *
    * {{{
    * scala> val f: Int => Int = { n => n + 1 }
    * f: Int => Int = $$Lambda$1035/114275379@765e9352
    *
    * scala> f.toString
    * res0: String = $$Lambda$1035/114275379@765e9352
    * }}}
    *
    * Notice that the following also works:
    *
    * {{{
    * scala> val stuff: List[Any] = List(1, true, "Hi")
    * stuff: List[Any] = List(1, true, Hi)
    *
    * scala> stuff.map(_.toString)
    * res1: List[String] = List(1, true, Hi)
    * }}}
    *
    * How was it able to find the correct `.toString` for each element, even though
    * they've all been cast to `Any`? The answer is a dynamic method lookup at runtime
    * (i.e. runtime polymorphism), which would incur some overhead and is generally
    * antithetical to type-safety.
    *
    * `Show` binds "printability" to a typeclass, making the conversion call
    * (via `shows`) a static one instead of a runtime reflective one as with `toString`.
    * Only types that are strictly _data_ will ever have a `Show` instance.
    *
    * All the standard types have `Show` instances defined for them already.
    */
  def show[A: Show](a: A): String = a.shows

  def showOpt: String = some(5).shows

  /* --- EQUAL --- */

  /** Scala has "universal equality". We can compare any two objects as equal, even
    * if it doesn't make sense to. Even if that type has no notion of equality (like `Function`).
    *
    * In a similar vein to `Show`, the `Equal` typeclass brings type safety back
    * to equality testing. Its main operator `(===)` (or "triple equals" like in
    * Javascript) guarantees that the type on each side of the operator is the same.
    *
    * `Equal` is also useful as it paves the way for the `Order` typeclass,
    * i.e. anything that can be compared (for sorting, say, or binary searches).
    *
    * ScalaZ opted to call their typeclass `Equal` instead of `Eq` like in Haskell.
    *
    * WART: Array equality doesn't work out of the box. This doesn't compile:
    *     Array(1,2,3) === Array(1,2,3)
    */
  def equalOpt: Boolean = some(5) === some(6)  // false

  def equalAll[A: Equal](l0: List[A], l1: List[A]): Boolean = l0 === l1

  /* --- STATE --- */

  /** No extra `value` call necessary like in cats.
    *
    * Note: `State` is actually an alias for:
    *   type State[S, A] = IndexedStateT[scalaz.Id.Id, S, S, A]
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

  /* --- SIDE EFFECTS --- */

  /** Same usage as Cats, except that to run the IO we use the Haskell-inspired
    * method `.unsafePerformIO`
    */
  def greet(name: String): IO[Unit] = IO { println(s"Hi, ${name}!") }

  /** This doesn't appear to blow the stack, even without manual trampolining.
    * Unlike Cats, there is a bit of a slowdown if you use `(>>=)` instead of
    * `flatMap`, but luckily performance is linear with input size, like Cats.
    */
  def recurseIO(n: Int): IO[Int] = n match {
    case 0 => IO(0)
    case n => IO(n - 1).flatMap(recurseIO)
  }

  /** The `IO` type is aware of exceptions and can help you handle them. */
  def ioException: IO[Unit] =
    IO(println("Step 1")) >> IO { throw new Exception } >> IO(println("Step 2"))

  /** Various ways of handling Exceptions through IO. Since Exceptions are side-effects
    * and side-effects should only occur in IO, this is a good way to communicate
    * that exceptions are possible in an application.
    *
    * ScalaZ also has the `bracket` pattern from Haskell, which Cats did not emulate.
    */
  def catchingExceptions: Unit = {
    /* Prints "Step 1" then explodes */
    ioException.unsafePerformIO

    /* Prints "Step 1", then "crap", then explodes */
    ioException.onException(IO(println("crap"))).unsafePerformIO
    ioException.ensuring(IO(println("crap"))).unsafePerformIO

    /* Prints "Step 1", then "crap", then returns safely */
    ioException.except(_ => IO(println("crap"))).unsafePerformIO
  }

  /* --- ASYNC IO --- */

  // This is being overhauled in ScalaZ 8, so I'm waiting for that to be released
  // before I review it.

  /* --- TAGGED TYPES --- */

  /** This is similar to Haskell's `newtype` mechanism. It allows one to assign
    * multiple typeclass instances to a single type without incurring JVM boxing.
    */
  sealed trait Unsigned
  val Unsigned = Tag.of[Unsigned]
  type UInt = Int @@ Unsigned

  /* Value Classes */
  class Word(val unwrap: Int) extends AnyVal

  type Tile[A] = Array[A]

  def tileSumFoldUInt(ns: Tile[UInt]): UInt = Unsigned(ns.foldLeft(0)( (acc, n) => Unsigned.unwrap(n) + acc ))

  def tileSumFoldInt(ns: Tile[Int]): Int = ns.foldLeft(0)( (acc, n) => acc + n )

  def tileSumFoldWord(ns: Tile[Word]): Word = new Word(ns.foldLeft(0)( (acc, n) => acc + n.unwrap ))

  def tileSumWhileUInt(ns: Tile[UInt]): UInt = {
    var i: Int = 0
    var sum: Int = 0

    while (i < ns.length) { sum += Unsigned.unwrap(ns(i)); i += 1 }

    Unsigned(sum)
  }

  def tileSumWhileInt(ns: Tile[Int]): Int = {
    var i: Int = 0
    var sum: Int = 0

    while (i < ns.length) { sum += ns(i); i += 1 }

    sum
  }

  def tileSumWhileWord(ns: Tile[Word]): Word = {
    var i: Int = 0
    var sum: Int = 0

    while (i < ns.length) { sum += ns(i).unwrap; i += 1 }

    new Word(sum)
  }

}

/* --- WRITING TYPECLASS INSTANCES --- */

/** A simple Rose Tree. */
case class Tree[T](root: T, children: Seq[Tree[T]])

object Tree {

  /** Trees are mappable. */
  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      Tree(f(fa.root), fa.children.map(t => t.map(f)))
   }

  /** Trees are Applicative Functors. */
  implicit val treeApplicative: Applicative[Tree] = new Applicative[Tree] {
    def point[A](a: => A): Tree[A] = Tree(a, Seq.empty)

    def ap[A, B](fa: => Tree[A])(tf: => Tree[A => B]): Tree[B] = {
      val Tree(f, tfs) = tf

      Tree(f(fa.root), fa.children.map(t => t.map(f)) ++ tfs.map(t => fa <*> t))
     }
   }

  /** Trees are also Monads. */
  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    def point[A](a: => A): Tree[A] = treeApplicative.point(a)

    def bind[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = {
      val Tree(r2, k2) = f(fa.root)

      Tree(r2, k2 ++ fa.children.map(t => t >>= f))
    }
  }
}
