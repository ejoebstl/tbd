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

import akka.actor.ActorRef
import akka.event.LoggingAdapter
import scala.collection.mutable.{Map, MutableList, Set, TreeSet}

import tbd.{Changeable, Dest, Mod}
import tbd.Constants._
import tbd.master.Master
import tbd.worker.Worker

class DDG(log: LoggingAdapter, id: String, worker: Worker) {
  var root = new RootNode(id)
  val reads = Map[ModId, Set[ReadNode]]()
  val pars = Map[ActorRef, ParNode]()

  var updated = TreeSet[Node]()((new TimestampOrdering()).reverse)

  val ordering = new Ordering()

  def addRead(
      mod: Mod[Any],
      value: Any,
      parent: Node,
      reader: Any => Changeable[Any],
      funcTag: FunctionTag): ReadNode = {
    val timestamp = nextTimestamp(parent)

    val readNode = if(tbd.master.Main.debug) {
      new ReadNode(mod, parent, timestamp,
                   reader, Tag.Read(mod.id, funcTag)(value))
    } else {
      new ReadNode(mod, parent, timestamp,
                   reader, null)
    }

    parent.addChild(readNode)

    if (reads.contains(mod.id)) {
      reads(mod.id) += readNode.asInstanceOf[ReadNode]
    } else {
      reads(mod.id) = Set(readNode.asInstanceOf[ReadNode])
    }

    readNode
  }

  def addMod(mod: Mod[Any], mod2: Mod[Any], parent: Node, funcTag: FunctionTag): ModNode = {
    val timestamp = nextTimestamp(parent)

    val tag = if(tbd.master.Main.debug) {
      val reads = List(mod, mod2).filter(_ != null).map(x => {
        x.id
      }).toList
      Tag.Mod(reads, funcTag)
    } else {
      null
    }
    val modNode = new ModNode(parent, timestamp, tag)

    parent.addChild(modNode)

    modNode
  }

  def addWrite[T](mod: Mod[Any], mod2: Mod[Any], parent: Node): WriteNode = {
    val timestamp = nextTimestamp(parent)


    val tag = if(tbd.master.Main.debug) {
      val writes = List(mod, mod2).filter(_ != null).map((x: Mod[Any]) => {
        SingleWriteTag(x.id, x.read())
      }).toList
      Tag.Write(writes)
    } else {
      null
    }

    val writeNode = new WriteNode(mod, mod2, parent, timestamp, tag)

    parent.addChild(writeNode)

    writeNode
  }

  def addPar(workerRef1: ActorRef,
             workerRef2: ActorRef,
             parent: Node,
             fun1: FunctionTag,
             fun2: FunctionTag) {
    val timestamp = nextTimestamp(parent)

    val parNode = if(tbd.master.Main.debug) {
      new ParNode(workerRef1, workerRef2, parent, timestamp,
                              Tag.Par(fun1, fun2))
    } else {
      new ParNode(workerRef1, workerRef2, parent, timestamp, null)
    }

    parent.addChild(parNode)

    pars(workerRef1) = parNode
    pars(workerRef2) = parNode
  }

  def addMemo(
      parent: Node,
      signature: Seq[Any],
      funcTag: FunctionTag): MemoNode = {
    val timestamp = nextTimestamp(parent)

    val tag = if(tbd.master.Main.debug) {
      Tag.Memo(funcTag, signature)
    } else {
      null
    }

    val memoNode = new MemoNode(parent, timestamp, signature, tag)

    parent.addChild(memoNode)
    memoNode
  }

  def nextTimestamp(parent: Node): Timestamp = {
    if (parent.children.size == 0) {
      ordering.after(parent.timestamp)
    } else {
      ordering.after(parent.children.last.endTime)
    }
  }

  /**
   * Returns the timestamp for the next node in the graph after the execution
   * this node represents completes, which is the timestamp of the node's next
   * older sibling, unless it has no older siblings then its the node's parent's
   * next older sibling, recursively up the tree until an older sibling is found.
   */
  def getTimestampAfter(node: Node): Timestamp = {
    var previousChild: Node = null
    var ret: Timestamp = null

    if (node.parent == null) {
      Timestamp.MAX_TIMESTAMP
    } else {
      for (child <- node.parent.children) {
	if (previousChild == node) {
	  ret = child.timestamp
	}

	previousChild = child
      }

      if (ret == null) {
	getTimestampAfter(node.parent)
      } else {
	ret
      }
    }
  }

  def modUpdated(modId: ModId) {
    assert(reads.contains(modId))
    for (readNode <- reads(modId)) {
      if (!readNode.updated) {
        updated += readNode
      }

      readNode.updated = true
    }
  }

  // Pebbles a par node. Returns true iff the pebble did not already exist.
  def parUpdated(workerRef: ActorRef): Boolean = {
    val parNode = pars(workerRef)
    if (!parNode.pebble1 && !parNode.pebble2) {
      updated += parNode
      parNode.updated = true
    }

    if (parNode.workerRef1 == workerRef) {
      val ret = !parNode.pebble1
      parNode.pebble1 = true
      ret
    } else {
      val ret = !parNode.pebble2
      parNode.pebble2 = true
      ret
    }
  }

  /**
   * Called before a read is reexecuted, the descendents of this node are
   * cleaned up, up to the first memo nodes, which are returned so that
   * they can be reattached or cleaned up later.
   */
  def cleanupRead(subtree: Node): MutableList[Node] = {
    val ret = new MutableList[Node]()

    for (child <- subtree.children) {
      child.parent = null
    }

    ret ++= subtree.children

    subtree.children.clear()

    ret
  }

  def cleanupSubtree(subtree: Node) {
    for (child <- subtree.children) {
      cleanupSubtree(child)
    }

    cleanup(subtree)
  }

  private def cleanup(node: Node) {
    if (node.isInstanceOf[ReadNode]) {
      val readNode = node.asInstanceOf[ReadNode]
      reads(readNode.mod.id) -= readNode
    } else if (node.isInstanceOf[MemoNode]) {
      val signature = node.asInstanceOf[MemoNode].signature

      var toRemove: MemoNode = null
      for (memoNode <- worker.memoTable(signature)) {
        if (toRemove == null && memoNode.timestamp == node.timestamp) {
          toRemove = memoNode
        }
      }

      worker.memoTable(signature) -= toRemove
      if (worker.memoTable(signature).size == 0) {
	worker.memoTable -= signature
      }
    }

    node.updated = false
    ordering.remove(node.timestamp)

    node.children.clear()
  }

  def attachSubtree(parent: Node, subtree: Node) {
    if (subtree.parent != null) {
      subtree.parent.removeChild(subtree)

      var oldParent = subtree.parent
      while (oldParent != null) {
        oldParent.matchableInEpoch = Master.epoch + 1
        oldParent = oldParent.parent
      }
    }

    parent.addChild(subtree)
    subtree.parent = parent
  }

  /**
   * Replaces all of the dests equal to dest1 with dest2 and all mods equal to
   * dest1.mod with dest2.mod in the subtree rooted at node. This is called when
   * a memo match is made where the memo node has a dest that's different from
   * the currentDest.
   */
  def replaceDests(node: Node, dest1: Dest[Any], dest2: Dest[Any]) {
    if (node.isInstanceOf[MemoNode]) {
      val memoNode = node.asInstanceOf[MemoNode]
      if (memoNode.value.isInstanceOf[Changeable[_]]) {
	val changeable = memoNode.value.asInstanceOf[Changeable[Any]]
	if (changeable.mod == dest1.mod) {
	  changeable.mod = dest2.mod
	}
      }

      if (memoNode.value.isInstanceOf[Tuple2[_, _]]) {
        val tuple = memoNode.value.asInstanceOf[Tuple2[Any, Any]]

        if (tuple._1.isInstanceOf[Changeable[_]]) {
	  val changeable = tuple._1.asInstanceOf[Changeable[Any]]
	  if (changeable.mod == dest1.mod) {
	    changeable.mod = dest2.mod
	  }
        }

        if (tuple._2.isInstanceOf[Changeable[_]]) {
	  val changeable = tuple._2.asInstanceOf[Changeable[Any]]
	  if (changeable.mod == dest1.mod) {
	    changeable.mod = dest2.mod
	  }
        }
      }
    }

    if (node.currentDest == dest1) {
      node.currentDest = dest2

      // Because reads and memos must have as their currentDest the mod
      // that is closest in enclosing scope, a node that doesn't have dest1 as
      // its dest can't have any children that have dest1 either.
      for (child <- node.children) {
	replaceDests(child, dest1, dest2)
      }
    } else if(node.currentDest2 == dest1) {
      node.currentDest2 = dest2

      for (child <- node.children) {
	replaceDests(child, dest1, dest2)
      }
    }
  }

  override def toString = {
    root.toString("")
  }

  def toString(prefix: String): String = {
    root.toString(prefix)
  }
}
