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

import graph._
import tbd.ddg.Tag

/*
 * Generates a latex representation of the control graph of a DDG, using
 * TIKZ.
 *
 * The following TIKS styles have to be included into the document:
 * \tikzstyle{root}=[draw,fill=gray,rectangle,minimum size=8pt,inner sep=0pt,label={0:root}]
 * \tikzstyle{read}=[draw,fill=blue,circle,minimum size=8pt,inner sep=0pt,label={0:read}]
 * \tikzstyle{mod}=[draw,fill=magenta,circle,minimum size=8pt,inner sep=0pt,label={0:mod}]
 * \tikzstyle{memo}=[draw,fill=green!50,shape=diamond,minimum size=8pt,inner sep=0pt,label={0:memo}]
 * \tikzstyle{write}=[draw,fill=orange,circle,minimum size=8pt,inner sep=0pt,label={0:write}]
 * \tikzstyle{par}=[draw,fill=yellow,shape=diamond,minimum size=8pt,inner sep=0pt,label={0:par}]
 */
class LatexExport[T] extends ExperimentSink[T] {

  def resultReceived(result: ExperimentResult[T],
                     sender: ExperimentSource[T]) {
    println(export(result.ddg))
  }

  def export(ddg: DDG): String = {
    val out = new StringBuilder()

    writeNode(out, ddg.root, ddg, "")

    out.toString
  }

  //Formates a node lable suitable for latex export.
  private def getNodeLabel(node: Node): String = {
    node.tag match {
      case Tag.Write(writes) => writes match {
          case List(tbd.ddg.SingleWriteTag(mod, value)) => s"(${mod}, ${value})"
          case writes => "(" + {
              writes.map(w => s"(${w.mod}, ${w.value})").mkString(", ")
          } + ")"
      }
      case x:Tag.Read => {
          s"(${x.mod}, ${x.readValue}, f.${x.reader.funcId})"
      }
      case Tag.Memo(func, sig) => {
          val fsig = sig.mkString(", ")
          s"(${fsig}, f.${func.funcId})"
      }
      case Tag.Par(f1, f2) => s"(f.${f1.funcId}, f.${f2.funcId})"
      case Tag.Mod(mods, initializer) => {
          val fmods = mods.mkString(", ")
          s"(${fmods}, f.${initializer.funcId})"
      }
      case x:Tag.Root => ""
    }
  }

  //Recursively writes DDG nodes to the supplied StringBuilder.
  private def writeNode(
      out: StringBuilder,
      node: Node,
      ddg: DDG,
      pre: String): Unit = {
    out.append(pre)
    out.append(s"node (${node.internalId})[${node.typeString}")
    val shortLabel = getNodeLabel(node)
    out.append(s", label={0:${node.internalId}.${node.typeString}}, label={350:${shortLabel}}")
    out.append("]{}\n")

    val childs = ddg.getCallChildren(node)

    childs.foreach(child => {
      out.append(pre + "child {\n")
      writeNode(out, child, ddg, pre + "  ")
      out.append(pre + "}\n")
    })
  }
}
