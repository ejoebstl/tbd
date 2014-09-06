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

package tbd.visualization.eval

import tbd._
import tbd.TBD._
import SimpleListMod._
import scala.language.implicitConversions

object SimpleListMod {
  implicit def Convert[Element](mod: Mod[SimpleList[Element]]): SimpleListMod[Element] = {
    new SimpleListMod(mod)
  }
}

class SimpleList[ListElement](val value: ListElement,
                              val next: Mod[SimpleList[ListElement]]) {

  def naiveMap[NewListElement]
    (mapper: ListElement => NewListElement)
    (implicit c: Context):
      SimpleList[NewListElement] = {
    val newValue = mapper(value)
    val newNext = next.naiveMap(mapper)
    new SimpleList(newValue, newNext)
  }

  def memoMap[NewListElement]
    (mapper: ListElement => NewListElement, memo: Memoizer[Mod[SimpleList[NewListElement]]])
    (implicit c: Context):
      SimpleList[NewListElement] = {
    val newValue = mapper(value)
    val newNext = next.memoMap(mapper, memo)
    new SimpleList(newValue, newNext)
  }

  def readList(): List[ListElement] = {
    value :: next.readList()
  }
}

class SimpleListMod[Element](val elem: Mod[SimpleList[Element]]) {
  def naiveMap[NewListElement]
    (mapper: Element => NewListElement)
    (implicit c: Context):
      Mod[SimpleList[NewListElement]] = {
    mod {
      read(elem) {
        case null => write(null)
        case elem => write(elem.naiveMap(mapper))
      }
    }
  }

  def memoMap[NewListElement]
    (mapper: Element => NewListElement, memoizer: Memoizer[Mod[SimpleList[NewListElement]]] = null)
    (implicit c: Context):
      Mod[SimpleList[NewListElement]] = {

    val memo = if(memoizer == null) {
      makeMemoizer[Mod[SimpleList[NewListElement]]]()
    } else {
      memoizer
    }
    memo(elem) {
      mod {
        read(elem) {
          case null => write(null)
          case elem => write(elem.memoMap(mapper, memo))
        }
      }
    }
  }

  def readList(): List[Element] = {
    elem.read() match {
      case null => List()
      case n => n.readList()
    }
  }
}