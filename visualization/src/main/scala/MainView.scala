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
import analysis._
import scala.swing._
import scala.swing.event._
import tbd.ddg.{Tag, FunctionTag}

/*
 * Main Frame of the single visualizer application.
 */
class SingleView[T]() extends MainFrame with ExperimentSink[T] {
  def resultReceived(result: ExperimentResult[T],
                   sender: ExperimentSource[T]) {
    addResult(result)
  }

  def addResult(result: ExperimentResult[Any]) {
    visualizer.addResult(result)
  }

  val visualizer = new DdgVisualizer()

  listenTo(visualizer)

  contents = new GridBagPanel() {
    layout(visualizer) = new Constraints() {
      gridx = 0
      gridy = 0
      weighty = 1
      weightx = 1
      gridwidth = 2
      fill = GridBagPanel.Fill.Both
    }
  }
  pack()

  visible = true
}

/*
 * Main frame for the diff visualizer application.
 */
class DiffView[T]() extends MainFrame with ExperimentSink[T] {


  val label = new TextArea("To calculate trace distance, select two different DDGs.") {
    editable = false
    background = java.awt.Color.LIGHT_GRAY
  }

  def resultReceived(result: ExperimentResult[T],
                   sender: ExperimentSource[T]) {
    addResult(result)
  }

  def addResult(result: ExperimentResult[Any]) {
    visualizer1.addResult(result)
    visualizer2.addResult(result)
  }

  var adDistance = DistanceExtractors.allocationSensitive _
  var mDistance = DistanceExtractors.memoSensitive _
  var pDistance = DistanceExtractors.pure _

  var distances = List(("Pure", pDistance),
                       ("Memo-Sensitive", mDistance),
                       ("Allocation-Sensitive", adDistance))

  var comparison:(Node) => Any = DistanceExtractors.pure

  var diffSelector = new ComboBox(distances) {
    renderer = ListView.Renderer(_._1)
  }

  listenTo(diffSelector.selection)

  reactions += {
    case SelectedDDGChanged(ddg) => {
      updateDiff()
    }
    case x:SelectionChanged => {
      comparison = diffSelector.selection.item._2
      updateDiff()
    }
  }

  //Calculates and updates the diff information.
  private def updateDiff() {
    if(visualizer1.ddg != null && visualizer2.ddg != null) {

      val diff = new GreedyTraceComparison(comparison).compare(visualizer1.ddg.ddg,
                    visualizer2.ddg.ddg)
      visualizer1.setComparisonResult(diff) 
      visualizer2.setComparisonResult(diff)

      label.text = "Tree size left: " + visualizer1.ddg.ddg.nodes.size +
        ", right: " + visualizer2.ddg.ddg.nodes.size +
        "\nTrace Distance: " +
        (diff.added.length + diff.removed.length) +
        " (Added: " + diff.removed.length + ", removed: " + diff.added.length + ")"
    }
  }

  val visualizer1 = new DdgVisualizer()
  val visualizer2 = new DdgVisualizer()

  listenTo(visualizer1)
  listenTo(visualizer2)

  contents = new GridBagPanel() {
    layout(
      new SplitPane(Orientation.Vertical) {
        contents_$eq(visualizer1, visualizer2)
      }
    ) = new Constraints() {
      gridx = 0
      gridy = 0
      weighty = 1
      weightx = 1
      gridwidth = 2
      fill = GridBagPanel.Fill.Both
    }
    layout(diffSelector) = new Constraints() {
      gridx = 1
      gridy = 1
      fill = GridBagPanel.Fill.Horizontal
    }
    layout(label) = new Constraints() {
      gridx = 1
      gridy = 2
      fill = GridBagPanel.Fill.Horizontal
    }
  }
  pack()

  visible = true
}