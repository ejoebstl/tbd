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
                              val key: Int,
                              val next: Mod[SimpleList[ListElement]]) {
  override def toString = value.toString

  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[SimpleList[ListElement]]) {
      false
    } else {
      val that = obj.asInstanceOf[SimpleList[ListElement]]
      that.value == value && that.key == key
    }
  }

  override def hashCode() = value.hashCode()
}

class SimpleListMod[Element](val elem: Mod[SimpleList[Element]]) {
  def naiveMap[NewListElement]
    (mapper: Element => NewListElement)
    (implicit c: Context):
      Mod[SimpleList[NewListElement]] = {
    mod {
      read(elem) {
        case null => write(null)
        case elem => write({
          val newValue = mapper(elem.value)
          val newNext = elem.next.naiveMap(mapper)
          new SimpleList(newValue, elem.key, newNext)
        })
      }
    }
  }

  def naiveSplit
    (predicate: Element => Boolean)
    (implicit c: Context):
      (Mod[SimpleList[Element]], Mod[SimpleList[Element]]) = {
    (elem.naiveFilter(x => predicate(x)), elem.naiveFilter(x => !predicate(x)))
  }

  def naiveReverse(implicit c: Context): Mod[SimpleList[Element]] = {
      mod {
        var end = mod[SimpleList[Element]] { write[SimpleList[Element]](null) }
        elem.naiveReverseInternal(end)
      }
  }

  def naiveReverseInternal
    (akku: Mod[SimpleList[Element]])
    (implicit c: Context):
      Changeable[SimpleList[Element]] = {
    read(elem) {
      case null => {
        read(akku) {
          write(_)
        }
      }
      case elem => {
        val newAkku = mod {
          write(new SimpleList(elem.value, elem.key, akku))
        }
        elem.next.naiveReverseInternal(newAkku)
      }
    }
  }

  def memoReverse(implicit c: Context): Mod[SimpleList[Element]] = {
      mod {
        var end = mod[SimpleList[Element]] { write[SimpleList[Element]](null) }
        elem.memoReverseInternal(end, makeMemoizer[Changeable[SimpleList[Element]]]())
      }
  }

  def memoReverseInternal
    (akku: Mod[SimpleList[Element]],
    memo: Memoizer[Changeable[SimpleList[Element]]])
    (implicit c: Context):
      Changeable[SimpleList[Element]] = {
    read(elem) {
      case null => {
        read(akku) {
          write(_)
        }
      }
      case elem => {
        val newAkku = mod({
          write(new SimpleList(elem.value, elem.key, akku))
        }, elem.next)
        memo(newAkku, elem.next) {
          elem.next.memoReverseInternal(newAkku, memo)
        }
      }
    }
  }

  def naiveLinearReduce[OutputElement]
    (reducer: (Element, OutputElement) => OutputElement, initialValue: Mod[OutputElement])
    (implicit c: Context): Mod[OutputElement] = {
      mod {
        naiveLinearReduceInternal(reducer, initialValue)
      }
  }

  def naiveLinearReduceInternal[OutputElement]
    (reducer: (Element, OutputElement) => OutputElement, initialValue: Mod[OutputElement])
    (implicit c: Context): Changeable[OutputElement] = {
    read(elem) {
      case null => {
        read(initialValue) {
          write(_)
        }
      }
      case elem => {
        val akku = mod {
          read(initialValue) {
            case v => write(reducer(elem.value, v))
          }
        }

        elem.next.naiveLinearReduceInternal(reducer, akku)
      }
    }
  }

  def naiveTreeReduce
    (reducer: (Element, Element) => Element)
    (implicit c: Context): Mod[Element] = {
      mod {
        naiveTreeReduceInternal(reducer)
      }
  }
  def naiveTreeReduceInternal
    (reducer: (Element, Element) => Element)
    (implicit c: Context): Changeable[Element] = {
      read(elem) {
        case null => {
          throw new Exception("Tried to Tree-Reduce an empty list.")
        }
        case e => {
          read(e.next) {
            case null => write(e.value)
            case next => {
              val newList = mod {
                elem.reducePairs(reducer)
              }
              newList.naiveTreeReduceInternal(reducer)
            }
          }
        }
      }
  }

  def reducePairs
    (reducer: (Element, Element) => Element)
    (implicit c: Context): Changeable[SimpleList[Element]] = {
      read(elem) {
        case null => write(null)
        case elem => {
          read(elem.next) {
            case null => write(elem)
            case next => {
              val newNext = mod {
                next.next.reducePairs(reducer)
              }
              write(new SimpleList[Element](reducer(elem.value, next.value), elem.key, newNext))
            }
          }
        }
      }
  }

  def randomTreeReduce
    (reducer: (Element, Element) => Element)
    (implicit c: Context): Mod[Element] = {
      mod {
        randomTreeReduceInternal(reducer, 0)
      }
  }

  def randomTreeReduceInternal
    (reducer: (Element, Element) => Element, round: Integer)
    (implicit c: Context): Changeable[Element] = {
      read(elem) {
        case null => {
          throw new Exception("Tried to Tree-Reduce an empty list.")
        }
        case e => {
          read(e.next) {
            case null => write(e.value)
            case next => {
              val newList = mod {
                elem.randomReducePairs(reducer, round)
              }
              newList.randomTreeReduceInternal(reducer, round + 1)
            }
          }
        }
      }
  }

  def randomReducePairs
    (reducer: (Element, Element) => Element, round: Int)
    (implicit c: Context): Changeable[SimpleList[Element]] = {
      read(elem) {
        case null => write(null)
        case elem => {
          if(bhash(elem.key, round)) {
            val newNext = mod {
              elem.next.randomReducePairs(reducer, round)
            }
            write(new SimpleList(elem.value, elem.key, newNext))
          } else {
            read(elem.next) {
              case null => write(elem)
              case next => {
                val newNext = mod {
                  next.next.randomReducePairs(reducer, round)
                }
                write(new SimpleList(reducer(elem.value, next.value), elem.key, newNext))
              }
            }
          }
        }
      }
  }

  private def bhash(value: Int, round: Int): Boolean = {
    ((value * 16) + (round * 17)) % 5 == 0
  }

  def memoSplit
    (predicate: Element => Boolean)
    (implicit c: Context):
      (Mod[SimpleList[Element]], Mod[SimpleList[Element]]) = {
    (elem.memoFilter(x => predicate(x)), elem.memoFilter(x => !predicate(x)))
  }


  def naiveFilter
    (predicate: Element => Boolean)
    (implicit c: Context):
      Mod[SimpleList[Element]] = {

    mod {
      elem.naiveFilterInternal(predicate)
    }
  }

  def naiveFilterInternal
    (predicate: Element => Boolean)
    (implicit c: Context):
      Changeable[SimpleList[Element]] = {

    read(elem) {
      case null => {
        write(null)
      }
      case elem =>

        if(predicate(elem.value)) {
          val newNext = mod {
            elem.next.naiveFilterInternal(predicate)
          }
          write(new SimpleList(elem.value, elem.key, newNext))
        } else {
          elem.next.naiveFilterInternal(predicate)
        }
    }
  }

  def memoFilter
    (predicate: Element => Boolean)
    (implicit c: Context):
      Mod[SimpleList[Element]] = {

    val memo = makeMemoizer[Mod[SimpleList[Element]]]()

    mod {
      elem.memoFilterInternal(predicate, memo)
    }
  }

  def memoFilterInternal
    (predicate: Element => Boolean, memo: Memoizer[Mod[SimpleList[Element]]])
    (implicit c: Context):
      Changeable[SimpleList[Element]] = {

    read(elem) {
      case null => {
        write(null)
      }
      case elem =>

        if(predicate(elem.value)) {
          val newNext = memo(elem) {
            mod {
              elem.next.memoFilterInternal(predicate, memo)
            }
          }
          write(new SimpleList(elem.value, elem.key, newNext))
        } else {
          elem.next.memoFilterInternal(predicate, memo)
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
          case elem => write({
            val newValue = mapper(elem.value)
            val newNext = elem.next.memoMap(mapper, memo)
            new SimpleList(newValue, elem.key, newNext)
          })
        }
      }
    }
  }

  def readList(): List[Element] = {
    elem.read() match {
      case null => List()
      case n => n.value :: n.next.readList()
    }
  }
}