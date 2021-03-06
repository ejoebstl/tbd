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

import scala.collection.mutable.{ArrayBuffer, Map}

import tbd.Input

class IntInput(maxKey: Int, mutations: Array[String]) {

  val rand = new scala.util.Random()
  def addValue(input: Input[Int, Int], table: Map[Int, Int]) {
    var key = rand.nextInt(maxKey)
    val value = rand.nextInt(Int.MaxValue)
    while (table.contains(key)) {
      key = rand.nextInt(maxKey)
    }
    input.put(key, value)

    table += (key -> value)
  }

  def removeValue(input: Input[Int, Int], table: Map[Int, Int]) {
    if (table.size > 1) {
      var key = rand.nextInt(maxKey)
      while (!table.contains(key)) {
        key = rand.nextInt(maxKey)
      }
      input.remove(key)
      table -= key
    } else {
      addValue(input, table)
    }
  }

  def updateValue(input: Input[Int, Int], table: Map[Int, Int]) {
    var key = rand.nextInt(maxKey)
    val value = rand.nextInt(Int.MaxValue)
    while (!table.contains(key)) {
      key = rand.nextInt(maxKey)
    }

    input.update(key, value)

    table(key) = value
  }

  def update(input: Input[Int, Int], table: Map[Int, Int]) {
    mutations(rand.nextInt(mutations.size)) match {
      case "insert" => addValue(input, table)
      case "remove" => removeValue(input, table)
      case "update" => updateValue(input, table)
    }
  }
}
