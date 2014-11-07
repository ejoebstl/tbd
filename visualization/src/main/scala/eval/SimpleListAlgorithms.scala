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
    extends TestAlgorithm[SimpleListInput[Int], TbdOutputType, NativeOutputType] {
  protected def createInput() = { new SimpleListInput[Int]() }
}

class NaiveSimpleListMap()
    extends SimpleListTestAlgoritm[Mod[SimpleList[Int]], Seq[Int]] {

  def run(implicit c: Context): Mod[SimpleList[Int]] = {
    val head = input.getList()

    head.naiveMap(x => (x * 2))
  }

  def getResult(output: Mod[SimpleList[Int]]) = {
    output.readList().sortWith(_ < _)
  }

  def getExpectedResult(input: Map[Int, Int]) = {
    input.values.map(x => x * 2).toBuffer.sortWith(_ < _)
  }
}

class MemoSimpleListMap()
    extends NaiveSimpleListMap {

  override def run(implicit c: Context): Mod[SimpleList[Int]] = {
    val head = input.getList()

    head.memoMap(x => (x * 2))
  }
}

class NaiveSimpleListReverse()
    extends SimpleListTestAlgoritm[Mod[SimpleList[Int]], Seq[Int]] {

  def run(implicit c: Context): Mod[SimpleList[Int]] = {
    val head = input.getList()

    head.naiveReverse(c)
  }

  def getResult(output: Mod[SimpleList[Int]]) = {
    output.readList().toBuffer
  }

  def getExpectedResult(input: Map[Int, Int]) = {
    input.values.toBuffer.reverse
  }
}

class MemoSimpleListReverse()
    extends NaiveSimpleListReverse {

  override def run(implicit c: Context): Mod[SimpleList[Int]] = {
    val head = input.getList()

    head.memoReverse(c)
  }
}

class NaiveSimpleListFilter()
    extends SimpleListTestAlgoritm[Mod[SimpleList[Int]], Seq[Int]] {

  def run(implicit c: Context): Mod[SimpleList[Int]] = {
    val head = input.getList()

    head.naiveFilter(x => (x % 2 == 0))
  }

  def getResult(output: Mod[SimpleList[Int]]) = {
    output.readList().toBuffer.sortWith(_ < _)
  }

  def getExpectedResult(input: Map[Int, Int]) = {
    input.values.filter(x => x % 2 == 0).toBuffer.sortWith(_ < _)
  }
}

class MemoSimpleListFilter()
    extends NaiveSimpleListFilter {

  override def run(implicit c: Context): Mod[SimpleList[Int]] = {
    val head = input.getList()

    head.memoFilter(x => (x % 2 == 0))
  }
}

class NaiveSimpleListSplit()
    extends SimpleListTestAlgoritm[(Mod[SimpleList[Int]], Mod[SimpleList[Int]]), (Seq[Int], Seq[Int])] {

  def run(implicit c: Context): (Mod[SimpleList[Int]], Mod[SimpleList[Int]]) = {
    val head = input.getList()

    head.naiveSplit(x => (x % 2 == 0))
  }

  def getResult(output: (Mod[SimpleList[Int]], Mod[SimpleList[Int]])) = {
    (output._1.readList().sortWith(_ < _),
     output._2.readList().sortWith(_ < _))
  }

  def getExpectedResult(input: Map[Int, Int]) = {
    (input.values.filter(x => x % 2 == 0).toSeq.sortWith(_ < _),
    input.values.filter(x => x % 2 != 0).toSeq.sortWith(_ < _))
  }
}

class NaiveSimpleLinearReduce()
    extends SimpleListTestAlgoritm[Mod[Int], Int] {
  override def run(implicit c: Context): Mod[Int] = {
    val head = input.getList()
    val init = mod {
      write(0)
    }
    head.naiveLinearReduce((a, b) => (a + b), init)
  }

  def getResult(output: Mod[Int]) = {
    output.read()
  }

  def getExpectedResult(input: Map[Int, Int]) = {
    input.values.foldLeft(0)(_ + _)
  }
}

class NaiveTreeReduce()
    extends NaiveSimpleLinearReduce {

  override def run(implicit c: Context): Mod[Int] = {
    val head = input.getList()

    head.naiveTreeReduce((a, b) => (a + b))
  }
}

class MemoLinearReduce()
    extends NaiveSimpleLinearReduce {

  override def run(implicit c: Context): Mod[Int] = {
    val head = input.getList()

    val init = mod {
      write(0)
    }

    head.memoLinearReduce((a, b) => (a + b), init)
  }
}

class MemoTreeReduce()
    extends NaiveSimpleLinearReduce {

  override def run(implicit c: Context): Mod[Int] = {
    val head = input.getList()

    head.memoTreeReduce((a, b) => (a + b))
  }
}

class RandomTreeReduce()
    extends NaiveSimpleLinearReduce {

  override def run(implicit c: Context): Mod[Int] = {
    val head = input.getList()

    head.randomTreeReduce((a, b) => (a + b))
  }
}

class NaiveQuickSort()
   extends SimpleListTestAlgoritm[Mod[SimpleList[Int]], Seq[Int]] {
  override def run(implicit c: Context): Mod[SimpleList[Int]] = {
    val head = input.getList()

    head.simpleQuickSort(_ < _)
  }

  def getResult(output: Mod[SimpleList[Int]]) = {
    output.readList().toBuffer
  }

  def getExpectedResult(input: Map[Int, Int]) = {
    input.values.toBuffer.sortWith(_ < _)
  }
}



class MemoSimpleListSplit()
    extends NaiveSimpleListSplit {

  override def run(implicit c: Context): (Mod[SimpleList[Int]], Mod[SimpleList[Int]]) = {
    val head = input.getList()

    head.memoSplit(x => (x % 2 == 0))
  }
}