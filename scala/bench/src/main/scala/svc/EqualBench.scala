package svc

import java.util.concurrent.TimeUnit

import cats.implicits._
import org.openjdk.jmh.annotations._
import scalaz.{IList, ImmutableArray}
import scalaz.Scalaz._

// --- //

case class Bar(age: Int, msg: String, truthy: Boolean)

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@State(Scope.Thread)
class EqualBench {
  var list0: List[Int] = _
  var list1: List[Int] = _
  var listK1: List[Kitties.Foo] = _
  var listK2: List[Kitties.Foo] = _
  var listz1: IList[Int] = _
  var listz2: IList[Int] = _
  var listZ1: IList[Zed.Foo] = _
  var listZ2: IList[Zed.Foo] = _
  var listV1: List[Bar] = _
  var listV2: List[Bar] = _
  var arr0:  Array[Int] = _
  var arrK:  Array[Kitties.Foo] = _
  var arrZ:  ImmutableArray[Zed.Foo] = _
  var arrV:  Array[Bar] = _

  @Setup
  def setup(): Unit = {
    arrK  = Array.range(1, 1000).map(_ => Kitties.Foo(1000, "hello therE", true))
    arrZ  = ImmutableArray.fromArray(Array.range(1, 1000).map(_ => Zed.Foo(1000, "hello therE", true)))
    arrV  = Array.range(1, 1000).map(_ => Bar(1000, "hello therE", true))
    arr0  = Array.range(1, 1000)
    list0 = List.range(1, 1000)
    list1 = List.range(1, 1000)
    listK1 = arrK.toList
    listK2 = arrK.toList
    listz1 = IList.fromList(List.range(1, 1000))
    listz2 = IList.fromList(List.range(1, 1000))
    listZ1 = IList.fromList(arrZ.toList)
    listZ2 = IList.fromList(arrZ.toList)
    listV1 = arrV.toList
    listV2 = arrV.toList
  }

  def whileInt(arr: Array[Int]): Boolean = {
    var res: Boolean = false
    var i: Int = 0

    while (i < arr.length) {
      res = arr(i) == 1000
      i += 1
    }

    res
  }

  def whileClass(arr: Array[Bar]): Boolean = {
    var res: Boolean = false
    var i: Int = 0
    val target: Bar = Bar(1000, "hello there", true)

    while (i < arr.length) {
      res = arr(i) == target
      i += 1
    }

    res
  }

  /*
  @Benchmark
  def equalSameIntVanilla: Boolean = list0 == list0
  @Benchmark
  def equalSameClassVanilla: Boolean = listV1 == listV1
  @Benchmark
  def equalDiffClassVanilla: Boolean = listV1 == listV2
  @Benchmark
  def equalDiffIntVanilla: Boolean = list0 == list1
  @Benchmark
  def equalWhileIntVanilla: Boolean = whileInt(arr0)
  @Benchmark
  def equalWhileClassVanilla: Boolean = whileClass(arrV)

  @Benchmark
  def equalSameIntCats: Boolean = Kitties.equalAll(list0, list0)
  @Benchmark
  def equalSameClassCats: Boolean = Kitties.equalAll(listK1, listK1)
  @Benchmark
  def equalDiffIntCats: Boolean = Kitties.equalAll(list0, list1)
  @Benchmark
  def equalDiffClassCats: Boolean = Kitties.equalAll(listK1, listK2)
  @Benchmark
  def equalWhileIntCats: Boolean = Kitties.equalWhileInt(arr0)
  @Benchmark
  def equalWhileClassCats: Boolean = Kitties.equalWhileClass(arrK)
   */

  @Benchmark
  def equalSameIntScalaz: Boolean = Zed.equalAll(listz1, listz1)
  @Benchmark
  def equalSameClassScalaz: Boolean = Zed.equalAll(listZ1, listZ1)
  @Benchmark
  def equalDiffIntScalaz: Boolean = Zed.equalAll(listz1, listz2)
  @Benchmark
  def equalDiffClassScalaz: Boolean = Zed.equalAll(listZ1, listZ2)
  @Benchmark
  def equalWhileIntScalaz: Boolean = Zed.equalWhileInt(arr0)
  @Benchmark
  def equalWhileClassScalaz: Boolean = Zed.equalWhileClass(arrZ)

}
