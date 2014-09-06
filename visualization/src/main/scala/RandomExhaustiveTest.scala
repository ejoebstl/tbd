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

package tbd.visualization

import scala.collection.mutable.ArrayBuffer
import collection.mutable.HashMap
import scala.util.Random
import scala.io.StdIn

import tbd._

/*
 * Test generator which performs random mutations on the input.
 */
class RandomExhaustiveTest[I <: Input[Int, Int], T, V](algorithm: TestAlgorithm[I, T, V])
  extends TestBase[I, T, V](algorithm) {

  var maxMutations = 2
  var minMutations = 0
  var count = 20

  def initialize() = { }

  def step() = {
    for(i <- 1 to rand.nextInt(maxMutations - minMutations) + minMutations) {
      randomMutation()
    }

    mutationCounter < count
  }

  def dispose() = { }
}
