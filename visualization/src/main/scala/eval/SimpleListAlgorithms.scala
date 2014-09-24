/**
 * Copyright (C) 2013 Carnegie Mellon University
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package tbd.visualization.eval

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map

import tbd.visualization.TestAlgorithm
import tbd._
import tbd.list._
import tbd.TBD._
import SimpleListMod._
import scala.language.implicitConversions


abstract class SimpleListTestAlgoritm[TbdOutputType, NativeOutputType]()
    extends TestAlgorithm[SimpleListInput[Int, Int], TbdOutputType, NativeOutputType] {
  protected def createInput() = { new SimpleListInput[Int, Int]() }
}

class NaiveSimpleListMap()
    extends SimpleListTestAlgoritm[Mod[SimpleList[(Int, Int)]], Seq[Int]] {

  def run(implicit c: Context): Mod[SimpleList[(Int, Int)]] = {
    val head = input.getList()

    head.naiveMap(x => (x._1, x._2 * 2))
  }

  def getResult(output: Mod[SimpleList[(Int, Int)]]) = {
    output.readList().map(_._2).sortWith(_ < _)
  }

  def getExpectedResult(input: Map[Int, Int]) = {
    input.values.map(x => x * 2).toSeq.sortWith(_ < _)
  }
}

class MemoSimpleListMap()
    extends NaiveSimpleListMap {

  override def run(implicit c: Context): Mod[SimpleList[(Int, Int)]] = {
    val head = input.getList()

    head.memoMap(x => (x._1, x._2 * 2))
  }
}

class NaiveSimpleListReverse()
    extends SimpleListTestAlgoritm[Mod[SimpleList[(Int, Int)]], Seq[Int]] {

  def run(implicit c: Context): Mod[SimpleList[(Int, Int)]] = {
    val head = input.getList()

    head.naiveReverse(c)
  }

  def getResult(output: Mod[SimpleList[(Int, Int)]]) = {
    output.readList().map(_._2)
  }

  def getExpectedResult(input: Map[Int, Int]) = {
    input.values.toSeq.reverse
  }
}

class MemoSimpleListReverse()
    extends NaiveSimpleListReverse {

  override def run(implicit c: Context): Mod[SimpleList[(Int, Int)]] = {
    val head = input.getList()

    head.memoReverse(c)
  }
}

class NaiveSimpleListFilter()
    extends SimpleListTestAlgoritm[Mod[SimpleList[(Int, Int)]], Seq[Int]] {

  def run(implicit c: Context): Mod[SimpleList[(Int, Int)]] = {
    val head = input.getList()

    head.naiveFilter(x => (x._2 % 2 == 0))
  }

  def getResult(output: Mod[SimpleList[(Int, Int)]]) = {
    output.readList().map(_._2).sortWith(_ < _)
  }

  def getExpectedResult(input: Map[Int, Int]) = {
    input.values.filter(x => x % 2 == 0).toSeq.sortWith(_ < _)
  }
}

class MemoSimpleListFilter()
    extends NaiveSimpleListFilter {

  override def run(implicit c: Context): Mod[SimpleList[(Int, Int)]] = {
    val head = input.getList()

    head.memoFilter(x => (x._2 % 2 == 0))
  }
}

class NaiveSimpleListSplit()
    extends SimpleListTestAlgoritm[(Mod[SimpleList[(Int, Int)]], Mod[SimpleList[(Int, Int)]]), (Seq[Int], Seq[Int])] {

  def run(implicit c: Context): (Mod[SimpleList[(Int, Int)]], Mod[SimpleList[(Int, Int)]]) = {
    val head = input.getList()

    head.naiveSplit(x => (x._2 % 2 == 0))
  }

  def getResult(output: (Mod[SimpleList[(Int, Int)]], Mod[SimpleList[(Int, Int)]])) = {
    (output._1.readList().map(_._2).sortWith(_ < _),
     output._2.readList().map(_._2).sortWith(_ < _))
  }

  def getExpectedResult(input: Map[Int, Int]) = {
    (input.values.filter(x => x % 2 == 0).toSeq.sortWith(_ < _),
    input.values.filter(x => x % 2 != 0).toSeq.sortWith(_ < _))
  }
}

class MemoSimpleListSplit()
    extends NaiveSimpleListSplit {

  override def run(implicit c: Context): (Mod[SimpleList[(Int, Int)]], Mod[SimpleList[(Int, Int)]]) = {
    val head = input.getList()

    head.memoSplit(x => (x._2 % 2 == 0))
  }
}