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

import tbd.visualization.graph._
import tbd.visualization.analysis._
import tbd.ddg.{Tag, FunctionTag}
import scala.collection.mutable.{HashMap, ListBuffer}
import swing._
import swing.event._
import java.awt.{Color, Graphics2D}

class TbdVisualizer extends Panel with Publisher {

  val vSpacing = 50
  val hSpacing = 50

  var translateX: Float = 0
  var translateY: Float = 0

  var scale: Float = 1

  var lx = -1
  var ly = -1

  val pos = new HashMap[Node, (Int, Int)]()
  val nodes = new ListBuffer[Node]()
  val edges = new ListBuffer[(Node, Node)]()

  listenTo(this.mouse.moves)
  listenTo(this.mouse.clicks)
  listenTo(this.mouse.wheel)
  listenTo(this.keys)
  focusable = true

  private def setPos(node: Node, x: Int, y: Int) {
    pos(node) = (x, y)
  }

  private def getPos(node: Node): (Int, Int) = {
    if(pos.contains(node)) {
      pos(node)
    } else {
      (0, 0)
    }
  }

  private def clear() {
    edges.clear()
    nodes.clear()
    pos.clear()
  }

  private def transform(pos: (Int, Int)): (Int, Int) = {
    val (x, y) = pos

    (((x + translateX) * scale).toInt, ((y + translateY) * scale).toInt)
  }

  private def inverseTransform(pos: (Int, Int)): (Int, Int) = {
    val (x, y) = pos

    ((x / scale - translateX).toInt, (y / scale - translateY).toInt)
  }

  override def paintComponent(g: Graphics2D) = {

    g.setColor(Color.WHITE)
    g.fillRect(0, 0, g.getClipBounds().width , g.getClipBounds().height)

    for(edge <- edges) {
      val (x1, y1) = transform(getPos(edge._1))
      val (x2, y2) = transform(getPos(edge._2))

      g.setColor(Color.BLACK)
      g.drawLine(x1, y1, x2, y2)
    }

    for(node <- nodes) {
      val (x, y) = transform(getPos(node))

      val bg = getNodeBackgroundColor(node)

      node.tag match {
        case q:Tag.Read => drawRead(x, y, g, bg)
        case q:Tag.Write => drawWrite(x, y, g, bg)
        case q:Tag.Memo => drawMemo(x, y, g, bg)
        case q:Tag.Par => drawPar(x, y, g, bg)
        case q:Tag.Root => drawRoot(x, y, g, bg)
        case q:Tag.Mod => drawMod(x, y, g, bg)
      }
    }
  }

  private def getNodeBackgroundColor(node: Node): Color = {
    if(comparison != null) {
      if(comparison.removed.contains(node)) {
        Color.RED
      } else if(comparison.added.contains(node)) {
        Color.YELLOW
      } else {
        null
      }
    } else {
      null
    }
  }

  val nodeSize = 5
  val backSize = 7

  private def drawRead(x: Int, y: Int, g: Graphics2D, bg: Color) {
    if(bg != null) {
      drawRoundNode(x, y, backSize, bg, g)
    }
    drawRoundNode(x, y, nodeSize, Color.BLUE, g)
  }

  private def drawWrite(x: Int, y: Int, g: Graphics2D, bg: Color) {
    if(bg != null) {
      drawRoundNode(x, y, backSize, bg, g)
    }
    drawRoundNode(x, y, nodeSize, Color.ORANGE, g)
  }

  private def drawMod(x: Int, y: Int, g: Graphics2D, bg: Color) {
    if(bg != null) {
      drawRoundNode(x, y, backSize, bg, g)
    }
    drawRoundNode(x, y, nodeSize, Color.MAGENTA, g)
  }

  private def drawRoot(x: Int, y: Int, g: Graphics2D, bg: Color) {
    if(bg != null) {
      drawRectNode(x, y, backSize, bg, g)
    }
    drawRectNode(x, y, nodeSize, Color.GRAY, g)
  }

  private def drawMemo(x: Int, y: Int, g: Graphics2D, bg: Color) {
    if(bg != null) {
      drawRectNode(x, y, backSize, bg, g)
    }
    drawRectNode(x, y, nodeSize, Color.GREEN, g)
  }

  private def drawPar(x: Int, y: Int, g: Graphics2D, bg: Color) {
    if(bg != null) {
      drawRectNode(x, y, backSize, bg, g)
    }
    drawRectNode(x, y, nodeSize, Color.YELLOW, g)
  }

  private def drawRoundNode(x: Int, y: Int, radius: Int, c: Color, g: Graphics2D) {
    g.setColor(c)
    g.fillOval(x - radius, y - radius, radius * 2, radius * 2)
  }

  private def drawRectNode(x: Int, y: Int, size: Int, c: Color, g: Graphics2D) {
    g.setColor(c)
    g.fillRect(x - size, y - size, size * 2, size * 2)
  }

  private def drawDIamondNode(x: Int, y: Int, size: Int, c: Color, g: Graphics2D) {
    val transform = g.getTransform()
    g.translate(x, y)
    g.rotate(Math.PI / 4)
    g.setColor(c)
    g.fillRect(-size, -size, size * 2, size * 2)
    g.setTransform(transform)
  }

  reactions += {
    case e: KeyPressed => println("Key pressed" + e)
    case MouseClicked(_, p, _, _, _) => {
      val (x, y) = inverseTransform((p.x, p.y))
      val selectedNode = nodes.map(n => {
        val (nx, ny) = pos(n)
        val dx = (nx - x)
        val dy = (ny - y)

        (dx * dx + dy * dy, n)
      }).sortWith((a, b) => a._1 < b._1).headOption

      if(!selectedNode.isEmpty) {
        publish(NodeClickedEvent(selectedNode.get._2))
      }
    }
    case MouseDragged(_, point, _) => {
        if(lx != -1 && ly != -1) {
          translateX -= (lx - point.x) / scale
          translateY -= (ly - point.y) / scale
        }
        lx = point.x
        ly = point.y

        invokePerspectiveChanged()
        repaint()
    }
    case e: MouseReleased => {
      lx = -1
      ly = -1
    }
    case MouseWheelMoved(_, _, _, dir) => {

      val scaleSpeed = 1.3f

      if(dir < 0) {
        scale *= scaleSpeed
      } else {
        scale /= scaleSpeed
      }

      invokePerspectiveChanged()
      repaint()
    }
    case _ => null
  }

  private def invokePerspectiveChanged() {
    publish(PerspectiveChangedEvent(translateX, translateY, scale))
  }

  def setNewPerspective(translateX: Float, translateY: Float, scale: Float) {
    if(this.translateX != translateX ||
       this.translateY != translateY ||
       this.scale != scale) {

      this.translateX = translateX
      this.translateY = translateY
      this.scale = scale

      repaint()
    }
  }

  private def addNode(node: Node) = {
    nodes += node
  }

  private def removeNode(node: Node) {
    nodes -= node
  }

  private def addEdge(a: Node, b: Node) = {
    edges += Tuple2(a, b)
  }

  private def removeEdge(a: Node, b: Node) {
    edges -= Tuple2(a, b)
  }

  private def getNodeType(node: Node): String = {
    node.tag match {
      case x:Tag.Write => "write"
      case x:Tag.Read => "read"
      case x:Tag.Memo => "memo"
      case x:Tag.Par => "par"
      case x:Tag.Root => "root"
    }
  }

  def createTree(node: Node, parent: Node) {

    addNode(node)

    if(parent != null) {
      addEdge(parent, node)
    }

    ddg.getCallChildren(node).foreach(x => {
      createTree(x, node)
    })
  }

  //Layouts the tree and returns the subtree width.
  def layoutTree(parent: Node, depth: Int): Int = {

    setPos(parent, 0, depth * vSpacing)

    if(ddg.getCallChildren(parent).length == 0) {
      return 0
    }
    else {
      var sum = 0;
      ddg.getCallChildren(parent).foreach(child => {
          val width = layoutTree(child, depth + 1)
          if(sum != 0)
            translateTree(child, sum)

          sum += (width + hSpacing)
      })

      sum -= hSpacing

      setPos(parent, sum / 2, depth * vSpacing)

      sum
    }
  }

  def translateTree(parent: Node, dx: Int) {

    val (x, y) = getPos(parent)
    setPos(parent, x + dx, y)

    if(((x + dx) / vSpacing) % 2 == 0) {
      //setStyle(parent, "text-offset: 0, 3;")
    } else {
      //setStyle(parent, "text-offset: 0, -27;")
    }

    ddg.getCallChildren(parent).foreach(child => {
      translateTree(child, dx)
    })
  }

  var ddg: DDG = null
  var comparison: ComparisonResult = null

  def showDDG(ddg: DDG, comparison: ComparisonResult = null) = {

    clear()

    println("show ddg " + ddg)

    val root = ddg.root
    this.ddg = ddg
    this.comparison = comparison

    createTree(root, null)
    layoutTree(root, 0)

    this.repaint()
  }

  private def extractMethodName(node: Node): String = {

    if(node.stacktrace == null) {
      return "<No stacktrace available. Set Main.debug = true to enable stacktraces>"
    }

    val methodNames = node.stacktrace.map(y => y.getMethodName())
    val fileNames = node.stacktrace.map(y => (y.getMethodName(), y.getFileName(), y.getLineNumber()))

    var (_, fileName, lineNumber) = fileNames.filter(y => (y._1.contains("apply")))(0)

    var currentMethod = methodNames.filter(y => (!y.startsWith("<init>")
                                            && !y.startsWith("()")
                                            && !y.startsWith("addRead")
                                            && !y.startsWith("addWrite")
                                            && !y.startsWith("addMemo")
                                            && !y.startsWith("createMod")
                                            && !y.startsWith("getStackTrace")
                                            && !y.startsWith("apply")
                                            && !y.startsWith("read")
                                            && !y.startsWith("memo")
                                            && !y.startsWith("par")
                                            && !y.startsWith("write")
                                            && !y.startsWith("mod")))(0)

    if(currentMethod.contains("$")) {
      currentMethod = currentMethod.substring(0, currentMethod.lastIndexOf("$"))
      currentMethod = currentMethod.substring(currentMethod.lastIndexOf("$") + 1)
    }

    if(methodNames.find(x => x == "createMod").isDefined) {
      currentMethod += " (createMod)"
    }

    currentMethod + " " + fileName + ":" + lineNumber.toString
  }
}

case class NodeClickedEvent(val node: Node) extends event.Event
case class PerspectiveChangedEvent(val translateX: Float, val translateY: Float, val zoom: Float) extends event.Event