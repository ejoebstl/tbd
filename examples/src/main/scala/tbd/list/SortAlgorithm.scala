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
package tbd.examples.list

import scala.collection.{GenIterable, GenMap, Seq}
import scala.collection.mutable.Map

import tbd.{Adjustable, ListConf, Mutator, TBD}
import tbd.mod.{AdjustableList, Mod}

object SortAlgorithm {
  def predicate(a: (Int, String), b: (Int, String)): Boolean = {
    a._2 < b._2
  }
}

class SortAlgorithm(_conf: Map[String, _], _listConf: ListConf)
    extends Algorithm[String, AdjustableList[Int, String]](_conf, _listConf) {
  val input = mutator.createList[Int, String](listConf)

  data = new WCData(input, count, mutations)

  def runNaive(input: GenIterable[String]) = {
     input.toBuffer.sortWith(_ < _)
  }

  def checkOutput(
      input: Map[Int, String],
      output: AdjustableList[Int, String]) = {
    val sortedOutput = output.toBuffer

    val answer = runNaive(input.values)

    sortedOutput == answer.toBuffer
  }

  def run(tbd: TBD): AdjustableList[Int, String] = {
    val pages = input.getAdjustableList()

    pages.sort(tbd, (tbd: TBD, a:(Int, String), b:(Int, String)) =>
      SortAlgorithm.predicate(a, b), parallel, memoized)
  }
}
