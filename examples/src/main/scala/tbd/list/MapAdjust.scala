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

import tbd.{Adjustable, ChunkListInput, Mutator, TBD}
import tbd.mod.AdjustableList

object MapAdjust {
  def mapper(tbd: TBD, pair: (Int, String)): (Int, Int) = {
    var count = 0
    for (word <- pair._2.split("\\W+")) {
      count += 1
    }
    (pair._1, count)
  }
}

class MapAdjust(mutator: Mutator, partitions: Int, valueMod: Boolean,
    parallel: Boolean, memoized: Boolean) extends Algorithm(mutator, partitions,
      1, valueMod, parallel, memoized) {
  var output: AdjustableList[Int, Int] = null

  var traditionalAnswer: GenIterable[Int] = null

  def run(tbd: TBD) = {
    val pages = input.getAdjustableList()
    pages.map(tbd, MapAdjust.mapper, parallel = parallel, memoized = memoized)
  }

  def traditionalRun(input: GenIterable[String]) {
    traditionalAnswer = input.map(s => {
      MapAdjust.mapper(null, (0, s))._2
    })
  }

  def initialRun(mutator: Mutator) {
    output = mutator.run[AdjustableList[Int, Int]](this)
  }

  def checkOutput(input: GenMap[Int, String]): Boolean = {
    val sortedOutput = output.toBuffer().sortWith(_ < _)
    traditionalRun(input.values)
    sortedOutput == traditionalAnswer.toBuffer.sortWith(_ < _)
  }
}

class ChunkMapAdjust(mutator: Mutator, partitions: Int, chunkSize: Int,
    valueMod: Boolean, parallel: Boolean, memoized: Boolean)
      extends Algorithm(mutator, partitions, chunkSize, valueMod, parallel,
	memoized) {
  var output: AdjustableList[Int, Int] = null

  var traditionalAnswer: GenIterable[Int] = null

  def chunkMapper(tbd: TBD, chunk: Vector[(Int, String)]) = {
    mapCount += 1
    var count = 0

    for (page <- chunk) {
      for (word <- page._2.split("\\W+")) {
        count += 1
      }
    }

    (0, count)
  }

  def run(tbd: TBD) = {
    val pages = input.asInstanceOf[ChunkListInput[Int, String]].getChunkList()
    pages.chunkMap(tbd, chunkMapper, parallel = parallel, memoized = memoized)
  }

  def traditionalRun(input: GenIterable[String]) {
    traditionalAnswer = input.map(s => {
      MapAdjust.mapper(null, (0, s))._2
    })
  }

  def initialRun(mutator: Mutator) {
    output = mutator.run[AdjustableList[Int, Int]](this)
  }

  def checkOutput(input: GenMap[Int, String]): Boolean = {
    traditionalRun(input.values)
    output.toBuffer.reduce(_ + _) == traditionalAnswer.reduce(_ + _)
  }
}
