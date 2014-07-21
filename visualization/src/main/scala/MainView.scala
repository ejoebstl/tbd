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

import scala.swing._

class MainView extends MainFrame {

  val visualizer = new TbdVisualizer()
  import ListView._
  val selector = new ListView(List[ExperimentResult[Any, Any]]()) {
    renderer = Renderer(x => x.runId + " - " + x.input)
    border = Swing.EmptyBorder(10)
    selection.reactions += {
      case _ =>
        if(selection.items.length > 0) {
          visualizer.showDDG(selection.items(0).ddg)
        }
    }
  }

  def addResult(result: ExperimentResult[Any, Any]) {
    selector.listData = result +: selector.listData
  }

  contents = new GridBagPanel(){
    layout(new ScrollPane(selector))  = new Constraints() {
      gridx = 0
      gridy = 0
      weighty = 1
      fill = GridBagPanel.Fill.Vertical
    }
    layout(visualizer)  = new Constraints() {
      gridx = 1
      gridy = 0
      weighty = 1
      weightx = 1
      fill = GridBagPanel.Fill.Both
    }
  }

  visible = true
}