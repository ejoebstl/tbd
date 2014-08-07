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

import scala.collection.mutable.{HashMap}
import tbd.ddg.Tag
import tbd.Constants.ModId
import tbd.visualization.graph._

object DependencyTracker {

  def findAndInsertReadWriteDependencies(ddg: DDG) {
    val writes = new HashMap[ModId, Node]

    ddg.nodes.foreach(x => x.tag match {
      case Tag.Write(ws) => for(w <- ws) {
        writes(w.mod) = x
      }
      case _ => null
    })

    ddg.nodes.foreach(x => x.tag match {
      case y:Tag.Read => if(writes.contains(y.mod)) {
        val dst = writes(y.mod)
        val src = x
        ddg.adj(src) += Edge.ReadWrite(src, dst, y.mod)
        ddg.adj(dst) += Edge.WriteRead(dst, src, y.mod)
      }
      case _ => null
    })
  }

  def findAndInsertModWriteDependencies(ddg: DDG) {
    val mods = new HashMap[ModId, Node]

    ddg.nodes.foreach(x => x.tag match {
      case Tag.Mod(dests, _) => for(dest <- dests) {
          mods(dest) = x
      }
      case _ => null
    })

    ddg.nodes.foreach(x => x.tag match {
      case Tag.Write(writes) =>
        for(write <- writes) {
          if(mods.contains(write.mod)) {
            val dst = mods(write.mod)
            val src = x
            ddg.adj(src) += Edge.ModWrite(src, dst, write.mod)
            ddg.adj(dst) += Edge.WriteMod(dst, src, write.mod)
          }
        }
      case _ => null
    })
  }
  def findAndInsertFreeVarDependencies(ddg: DDG) {

    //Inverse direction for faster lookup
    var invDeps = HashMap[Node, List[Edge.FreeVar]]()

    //Insert single edges for each node.
    ddg.nodes.foreach(node => {
      var dependencies = node.tag match {
        case Tag.Read(_, fun) => fun.freeVars
        case Tag.Par(fun1, fun2) => fun1.freeVars ::: fun2.freeVars
        case Tag.Mod(_, fun) => fun.freeVars
        case Tag.Memo(fun, _) => fun.freeVars
        case _ => List[(String, Any)]()
      }

      if(!dependencies.isEmpty) {
        val parent = ddg.getCallParent(node)
        if(!invDeps.contains(parent)) {
          invDeps(parent) = List()
        }

        invDeps(parent) = Edge.FreeVar(node, parent, dependencies) :: invDeps(parent)
      }
    })

    //Summarize paths with similar dependencies. 
    val iter = new TopoSortIterator(
                ddg.root,
                ddg,
                (e: Edge) => (e.isInstanceOf[Edge.Control])).toList
    iter.foreach(node => {
      val parent = ddg.getCallParent(node)

      if(parent != null && invDeps.contains(node)) {
        val depsToMe = invDeps(node)
        val depsToParent = invDeps(parent)

        for(depToMe <- depsToMe) {
          for(depToParent <- depsToParent) {
            if(depToParent.source == node) {
              val common = depToParent.dependencies.intersect(depToMe.dependencies)

              if(!common.isEmpty) {
                depToMe.dependencies = depToMe.dependencies.filter(x => {
                  !common.contains(x)
                })

                invDeps(parent) = Edge.FreeVar(depToMe.source, parent, common) :: invDeps(parent)
              }
            }
          }
        }
      }
    })

    //Remove deps to root
    invDeps(ddg.root) = List()

    //Add non-empty edges to graph
    for(dep <- invDeps) {
      val (_, edges) = dep
      for(edge <- edges) {
        if(!edge.dependencies.isEmpty) {
          ddg.adj(edge.source) += edge
        }
      }
    }
  }
}
