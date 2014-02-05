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
package tbd.ddg

import scala.collection.mutable.{Map, Set}

import tbd.mod.ModId

class DDG {
  var root = new RootNode
  val reads = Map[ModId, Set[ReadNode]]()

  def addRead(modId: ModId, parent: Node): Node = {
    println(parent)
    val readNode = new ReadNode(modId, parent)

    if (reads.contains(modId)) {
      reads(modId) += readNode
    } else {
      reads(modId) = Set(readNode)
    }

    if (parent != null) {
      parent.addChild(readNode)
    } else {
      root.addChild(readNode)
    }

    readNode
  }

  def addWrite(modId: ModId, parent: Node): Node = {
    val writeNode = new WriteNode(modId, parent)

    parent.addChild(writeNode)

    writeNode
  }

  def modUpdated(modId: ModId) {
    for (readNode <- reads(modId)) {
      readNode.updated = true
    }
  }

  override def toString = {
    root.toString("")
  }
}
