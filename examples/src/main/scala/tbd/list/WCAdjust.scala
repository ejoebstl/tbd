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

import scala.collection.{GenIterable, GenMap}
import scala.collection.mutable.Map
import scala.collection.immutable.HashMap

import tbd.{Adjustable, ChunkListInput, Mutator, TBD}
import tbd.mod.{AdjustableList, Mod}

class WCAdjust(mutator: Mutator, partitions: Int, valueMod: Boolean,
    parallel: Boolean) extends Algorithm(mutator, partitions, 1, valueMod,
      parallel, true) {
  var output: Mod[(Int, HashMap[String, Int])] = null

  var traditionalAnswer: Map[String, Int] = null

  def initialRun(mutator: Mutator) {
    output = mutator.run[Mod[(Int, HashMap[String, Int])]](this)
  }

  def checkOutput(chunks: GenMap[Int, String]): Boolean = {
    traditionalRun(chunks.values)
    output.read()._2 == traditionalAnswer
  }

  def traditionalRun(input: GenIterable[String]) {
    traditionalAnswer = input.aggregate(Map[String, Int]())((x, line) =>
      WC.countReduce(line, x), WC.mutableReduce)
  }

  def mapper(tbd: TBD, pair: (Int, String)) = {
    mapCount += 1
    (pair._1, WC.wordcount(pair._2))
  }

  def reducer(
      tbd: TBD,
      pair1: (Int, HashMap[String, Int]),
      pair2: (Int, HashMap[String, Int])) = {
    reduceCount += 1
    (pair1._1, WC.reduce(pair1._2, pair2._2))
   }

  def run(tbd: TBD): Mod[(Int, HashMap[String, Int])] = {
    val pages = input.getAdjustableList()
    val counts = pages.map(tbd, mapper, parallel = parallel)
    val initialValue = tbd.createMod((0, HashMap[String, Int]()))
    counts.reduce(tbd, initialValue, reducer, parallel = parallel)
  }
}

class ChunkWCAdjust(mutator: Mutator, partitions: Int, chunkSize: Int,
    valueMod: Boolean, parallel: Boolean) extends Algorithm(mutator, partitions,
      chunkSize, valueMod, parallel, true) {
  var output: Mod[(Int, Map[String, Int])] = null

  var traditionalAnswer: Map[String, Int] = null;

  def initialRun(mutator: Mutator) {
    output = mutator.run[Mod[(Int, Map[String, Int])]](this)
  }

  def checkOutput(chunks: GenMap[Int, String]): Boolean = {
    traditionalRun(chunks.values)
    output.read()._2 == traditionalAnswer
  }

  def traditionalRun(input: GenIterable[String]) {
    traditionalAnswer = input.aggregate(Map[String, Int]())((x, line) =>
      WC.countReduce(line, x), WC.mutableReduce)
  }

  def chunkMapper(tbd: TBD, chunk: Vector[(Int, String)]) = {
    mapCount += 1
    val counts = Map[String, Int]()

    for (page <- chunk) {
      for (word <- page._2.split("\\W+")) {
        if (counts.contains(word)) {
          counts(word) += 1
        } else {
          counts(word) = 1
        }
      }
    }

    (0, HashMap(counts.toSeq: _*))
  }

  def chunkReducer(
      tbd: TBD,
      pair1: (Int, HashMap[String, Int]),
      pair2: (Int, HashMap[String, Int])) = {
    reduceCount += 1
    (pair1._1, WC.reduce(pair1._2, pair2._2))
  }

  def run(tbd: TBD): Mod[(Int, HashMap[String, Int])] = {
    val pages = input.asInstanceOf[ChunkListInput[Int, String]].getChunkList()
    val counts = pages.chunkMap(tbd, chunkMapper, parallel = parallel)
    val initialValue = tbd.createMod((0, HashMap[String, Int]()))
    counts.reduce(tbd, initialValue, chunkReducer, parallel = parallel)
  }
}
