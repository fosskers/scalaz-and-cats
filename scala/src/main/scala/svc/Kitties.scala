package svc

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/* Straight-forward imports */
import cats._
import cats.data._
import cats.implicits._

// --- //

object Kitties {

  /* --- STATE --- */

  /** Set the initial `State` to 1, and immediately `get` it.
    * `value` is what actually runs the `Eval` type returned by `run`.
    *
    * Note: (TODO: What is `State` an alias for?)
    */
  def oneGet: (Int, Int) = State.get.run(1).value

  /** How much ceremony is necessary for a monadic bind?
    *
    * WART: Type hand-holding on `get` but not `set`.
    * WART: Can't leave off the `n =>`. No fun currying option, either.
    */
  def bind: State[Int, Unit] = State.get[Int] >>= { n => State.set(n + 1) }

  /** The less sugary variant. No hand-holding necessary here, for some reason. */
  def flatmap: State[Int, Unit] = State.get.flatMap(n => State.set(n + 1))

  /** A simple recursive call. The State value is maintained throughout the
    * operation, which frees us from having to pass a variable around or worse,
    * use a global mutable variable.
    *
    * `pure` lifts any value into a monad (a "context"). In this case, it's
    * the State monad. This does *not* update the State.
    *
    * cats went for `set` instead of Haskell's `put`.
    */
  def countdown: State[Int, Int] = State.get.flatMap({ n =>
    if (n <= 0) State.pure(n) else State.set(n - 1) >> countdown
  })

  /* WART: Type parameters don't curry in Scala, so in order to define Transformer stacks
   * we need to write these boilerplate aliases.
   */
  type EitherStr[T] = EitherT[Eval, String, T]
  type StEtr[T] = StateT[EitherStr, Int, T]

  /** The same as the above, except that we "throw an error" via `EitherT`
    * once we count down to 0. This stack respects the bind (>>=) semantics of
    * both `StateT` and `EitherT`, so the State value will be carried through each
    * operation, and any "Leftness" will fail the entire thing.
    *
    * WART: The compiler needs its hand held on `get`, `set`, and `MonadError`.
    */
  def countdownT: StEtr[Unit] = StateT.get[EitherStr, Int].flatMap({ n =>
    if (n <= 0) MonadError[StEtr, String].raiseError("crap")
    else StateT.set[EitherStr, Int](n - 1) >> countdownT
  })

  /** `.value` needs to be called twice to fully evaluate the transformer stack. */
  def runCountDownT: Either[String, (Int, Unit)] = countdownT.run(10000).value.value

  type EitherStr2[T] = Either[String, T]

  /** One may be tempted to forego `EitherT` and reach straight for
    * `Either`. This is logical, but won't actually work. Here's how you'd do the
    * countdown, using `Left` to cancel further recursion. This compiles, but WILL blow the JVM
    * stack, even with relatively low initial State values (1000!).
    *
    * WART: Much hand-holding necessary to get the `Left` business to type check.
    */
  def badCountdownT: StateT[EitherStr2, Int, Unit] = StateT.get[EitherStr2, Int].flatMap({ n =>
    if (n <= 0) StateT((_:Int) => Left("crap") : EitherStr2[(Int, Unit)])
    else StateT.set[EitherStr2, Int](n - 1) >> badCountdownT
  })

  /* --- APPLICATIVE --- */

  /** The dumbest way to sum a list. Notice the `n.some`; we can't use
    * `Some(n)` here because `Some` doesn't have the |@| operator injected into
    * it.
    *
    * Cats uses `|@|` instead of Haskell's `<*>`.
    */
  def dumbSum(nums: List[Int]): Option[Int] = nums match {
    case Nil => Some(0)
    case n :: ns => (n.some |@| dumbSum(ns)).map(_ + _)
  }

  /** Oh god, the horror. Scala functions don't curry, so we have to do it ourselves.
    *
    * `ap` is the same as Haskell's `ap` (or commonly `<*>`).
    */
  def dumbSum2(nums: List[Int]): Option[Int] = nums match {
    case Nil => Some(0)
    case n :: ns => { (a: Int) => (b: Int) => a + b }.some ap n.some ap dumbSum2(ns)
  }
}

/* --- WRITING TYPECLASS INSTANCES --- */

/** A simple Rose Tree. Baum is Tree in German, renamed to avoid a conflict
  * with the Tree defined in Zed.
  */
case class Baum[T](root: T, children: Seq[Baum[T]])

object Baum {

  /** Identical to the ScalaZ implementation. */
  implicit val baumFunctor: Functor[Baum] = new Functor[Baum] {
    def map[A, B](fa: Baum[A])(f: A => B): Baum[B] =
      Baum(f(fa.root), fa.children.map(t => t.map(f)))
  }

  implicit val baumApplicative: Applicative[Baum] = new Applicative[Baum] {
    /* The argument here is strict, where ScalaZ's is lazy (=> A) */
    def pure[A](a: A): Baum[A] = Baum(a, Seq.empty)

    /* Opposite argument order from ScalaZ */
    def ap[A, B](tf: Baum[A => B])(fa: Baum[A]): Baum[B] = {
      val Baum(f, tfs) = tf

      Baum(f(fa.root), fa.children.map(t => t.map(f)) ++ tfs.map(t => t ap fa))
    }
  }

  implicit val baumMonad: Monad[Baum] = new Monad[Baum] {
    def pure[A](x: A): Baum[A] = baumApplicative.pure(x)

    def flatMap[A, B](fa: Baum[A])(f: A => Baum[B]): Baum[B] = {
      val Baum(r2, k2) = f(fa.root)

      Baum(r2, k2 ++ fa.children.map(t => t >>= f))
    }

    /* A unique requirement of cats which prevents stack overflows during
     * Monadic recursion, which is commonplace.
     */
    @tailrec
    def tailRecM[A, B](a: A)(f: A => Baum[Either[A, B]]): Baum[B] = f(a) match {
      case Baum(Right(r), kids) => Baum(r, ???)
      case Baum(Left(l), kids) => tailRecM(l)(f)
    }
  }
}

object James {
  val zong: Option[Int] = Some(2)
  def zing: Future[Option[Option[Int]]] = Future.successful(Some(Some(1)))

  val foo: OptionT[Future, Option[Int]] = for{
    a <- OptionT.fromOption[Future](zong)
    b <- OptionT(zing)
  } yield b.map(a + _)
}
