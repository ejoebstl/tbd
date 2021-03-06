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

import tbd.{Adjustable, Mutator, TBD}
import tbd.mod.AdjustableList

object FilterAdjust {
  def predicate(pair: (Int, String)): Boolean = {
    var count = 0
    for (word <- pair._2.split("\\W+")) {
      count += 1
    }
    pair._2.hashCode() % 2 == 0
  }
}

class FilterAdjust(mutator: Mutator, partitions: Int, parallel: Boolean,
    memoized: Boolean) extends Algorithm(mutator, partitions, 1, true, parallel,
      memoized) {
  var output: AdjustableList[Int, String] = null

  var traditionalAnswer: GenIterable[String] = null

  def run(tbd: TBD): AdjustableList[Int, String] = {
    val pages = input.getAdjustableList()
    pages.filter(tbd, (pair:(Int, String)) => FilterAdjust.predicate(pair), parallel, memoized)
  }

  def traditionalRun(input: GenIterable[String]) {
     traditionalAnswer = input.filter(value => {
      FilterAdjust.predicate((0, value))
    })
  }

  def initialRun(mutator: Mutator) {
    output = mutator.run[AdjustableList[Int, String]](this)
  }

  def checkOutput(input: GenMap[Int, String]): Boolean = {
    val sortedOutput = output.toBuffer().sortWith(_ < _)
    traditionalRun(input.values)

    sortedOutput == traditionalAnswer.toBuffer.sortWith(_ < _)
  }
}
