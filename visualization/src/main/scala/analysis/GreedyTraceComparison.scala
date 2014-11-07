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

package tbd.visualization.analysis

import tbd.visualization.graph._
import tbd.ddg.{Tag, FunctionTag}
import scala.collection.mutable.{Buffer, HashSet}

/*
 * Greedy (intrinsic) trace distance computation.
 */
class GreedyTraceComparison(extractor: (Node => Any), val ignoreMods: Boolean = false)
  extends TraceComparison(extractor) {

  def compare(before: DDG, after: DDG):
      ComparisonResult = {

    if(ignoreMods) {
      tbd.ModSettings.IgnoreModsInComparison.set(true)
    }

    //Inserts nodes into two sets A, B and computes
    //unchanged = A intersect B
    //removed = A without B
    //added = B without A
    var set = after.nodes.map(x => new NodeWrapper(x, extractor))

    var removed = List[Node]()
    var added = List[Node]()
    var unchanged = List[Node]()

    before.nodes.map(x => new NodeWrapper(x, extractor)).foreach(x => {
      if(set.remove(x)) {
        unchanged = x.node :: unchanged
      } else {
        added = x.node :: added
      }
    })

    set.foreach(x => removed = x.node :: removed)

    if(ignoreMods) {
      tbd.ModSettings.IgnoreModsInComparison.set(false)
    }

    new ComparisonResult(removed, added, unchanged)
  }
}

object DistanceExtractors {
  def pure(node: Node) = {
    node.internalId
  }

  //Orders variables to ease comparison.
  private def cleanupFunctionTag(func: FunctionTag): FunctionTag = {
    FunctionTag(func.funcId, func.freeVars.sortWith((x, y) => x._1 > y._1))
  }

  //Orders variables and removes mods.
  private def purgeFunctionTag(func: FunctionTag): FunctionTag = {
    val cleanedFunc = cleanupFunctionTag(func)
    FunctionTag(cleanedFunc.funcId, cleanedFunc.freeVars.filter(x => isMod(x._2)))
  }

  def memoSensitive(node: Node) = {
    node.tag match {
      case Tag.Write(writes) => List("write", writes)
      case x:Tag.Read => List("read", x.mod, cleanupFunctionTag(x.reader), x.readValue)
      case Tag.Memo(function, args) => List("memo", cleanupFunctionTag(function), args)
      case Tag.Mod(dests, initializer) => List("mod", dests, cleanupFunctionTag(initializer))
      case Tag.Par(fun1, fun2) => List("par", cleanupFunctionTag(fun1), cleanupFunctionTag(fun2))
      case Tag.Root() => List("root")
    }
  }

  private def isMod(x: Any) = {
    x match {
      case x:tbd.Constants.ModId => false
      case x:tbd.Mod[Any] => false
      case _ => true
    }
  }

  def allocationSensitive(node: Node) = {

    if(tbd.ModSettings.IgnoreModsInComparison.get() == false) {
      throw new Exception("When using allocation sensitive trace distance, " +
      "set ModSettings.IgnoreModsInComparison to true.")
    }

    node.tag match {
      case x @ Tag.Write(writes) => List("write", writes.map(x => x.value))
      case x:Tag.Read => List("read", purgeFunctionTag(x.reader), x.readValue)
      case Tag.Memo(function, args) => {
        List("memo", purgeFunctionTag(function), args.filter(isMod(_)))
      }
      case Tag.Mod(dests, initializer) => List("mod", purgeFunctionTag(initializer))
      case Tag.Par(fun1, fun2) => List("par", purgeFunctionTag(fun1), purgeFunctionTag(fun2))
      case Tag.Root() => List("root")
    }
  }
}