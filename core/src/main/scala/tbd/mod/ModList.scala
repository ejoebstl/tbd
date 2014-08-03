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
package tbd.mod

import scala.collection.mutable.{ArrayBuffer, Buffer, Map}

import tbd.{Changeable, Context, Memoizer}
import tbd.Constants.ModId
import tbd.TBD._

class ModList[T, U](
    val head: Mod[ModListNode[T, U]]
  ) extends AdjustableList[T, U] {

  def filter(
      pred: ((T, U)) => Boolean)
     (implicit c: Context): ModList[T, U] = {
    val memo = makeMemoizer[Mod[ModListNode[T, U]]]()

    new ModList(
      mod {
        read(head) {
	  case null => write[ModListNode[T, U]](null)
	  case node => node.filter(pred, memo)
        }
      }
    )
  }

  def map[V, W](
      f: ((T, U)) => (V, W))
     (implicit c: Context): ModList[V, W] = {
    val memo = makeMemoizer[Changeable[ModListNode[V, W]]]()

    new ModList(
      mod({
        read(head) {
          case null => write[ModListNode[V, W]](null)
          case node => node.map(f, memo)
        }
      }, head.id)
    )
  }

  override def merge(
      that: ModList[T, U],
      comparator: ((T, U), (T, U)) => Boolean)
     (implicit c: Context): ModList[T, U] = {
    new ModList(
      mod {
	read(head) {
	  case null => read(that.head) { write(_) }
	  case node =>
	    read(that.head) {
	      case null => write(node)
	      case thatNode => node.merge(thatNode, comparator)
	    }
	}
      }
    )
  }

  def reduce(
      f: ((T, U), (T, U)) => (T, U))
     (implicit c: Context): Mod[(T, U)] = {

    // Each round we need a hasher and a memo, and we need to guarantee that the
    // same hasher and memo are used for a given round during change propagation,
    // even if the first mod of the list is deleted.
    class RoundMemoizer {
      val memo = makeMemoizer[(Hasher,
                               Memoizer[Mod[ModListNode[T, U]]],
                               RoundMemoizer)]()

      def getTuple() =
        memo() {
          (new Hasher(2, 4),
           makeMemoizer[Mod[ModListNode[T, U]]](),
           new RoundMemoizer())
	}
    }

    def randomReduceList(
        head: ModListNode[T, U],
        next: ModListNode[T, U],
        round: Int,
        roundMemoizer: RoundMemoizer): Changeable[(T, U)] = {
      val tuple = roundMemoizer.getTuple()

      val halfListMod = mod {
	halfList(head.value, next, round, tuple._1, tuple._2)
      }

      read(halfListMod) {
        case halfList =>
          read(halfList.next) {
            case null => write(halfList.value)
            case next => randomReduceList(halfList, next, round + 1, tuple._3)
          }
      }
    }

    def binaryHash(id: ModId, round: Int, hasher: Hasher) = {
      hasher.hash(id.hashCode() ^ round) == 0
    }

    def halfList(
        acc: (T, U),
        node: ModListNode[T, U],
        round: Int,
        hasher: Hasher,
        memo: Memoizer[Mod[ModListNode[T, U]]]
      ): Changeable[ModListNode[T, U]] = {
      val newAcc = f(acc, node.value)

      if(binaryHash(node.next.id, round, hasher)) {
        val newNext = memo(node.next) {
	  mod {
	    read(node.next) {
              case null => write[ModListNode[T, U]](null)
              case next =>
                read(next.next) {
                  case null =>
                    val tail = mod { write[ModListNode[T, U]](null) }
                    write(new ModListNode(next.value, tail))
                  case nextNext =>
	            halfList(next.value, nextNext, round, hasher, memo)
                }
            }
	  }
	}
        write(new ModListNode(newAcc, newNext))
      } else {
        read(node.next) {
          case null =>
	    val tail = createMod[ModListNode[T, U]](null)
            write(new ModListNode(newAcc, tail))
          case next =>
	    halfList(newAcc, next, round, hasher, memo)
        }
      }
    }

    val roundMemoizer = new RoundMemoizer()
    mod {
      read(head) {
        case null => write(null)
        case head =>
          read(head.next) {
            case null => write(head.value)
            case next => randomReduceList(head, next, 0, roundMemoizer)
          }
      }
    }
  }

  def sort(
      comparator: ((T, U), (T, U)) => Boolean)
     (implicit c: Context): AdjustableList[T, U] = {
    val memo = makeMemoizer[Mod[ModListNode[T, U]]]()
    val memo2 = makeMemoizer[Changeable[ModListNode[T, U]]]()
    val memoizers = Map[(T, U), Memoizer[ModListNode.ChangeableTuple[T, U]]]()

    val sorted = mod {
      read(head) {
        case null => write[ModListNode[T, U]](null)
        case node =>
	  node.sort(createMod(null), comparator, memoizers, memo, memo2)
      }
    }

    new ModList(sorted)
  }

  def split(
      pred: ((T, U)) => Boolean)
     (implicit c: Context): (AdjustableList[T, U], AdjustableList[T, U]) = {
    val memo = makeMemoizer[ModListNode.ChangeableTuple[T, U]]()

    val result = mod2({
      read_2(head) {
	case null =>
	  write2[ModListNode[T, U], ModListNode[T, U]](null, null)
	case node => 
	  memo(node) {
	    node.split(memo, pred)
	  }
      }
    }, head.id)

    (new ModList(result._1), new ModList(result._2))
  }

  def toBuffer(): Buffer[U] = {
    val buf = ArrayBuffer[U]()
    var node = head.read()
    while (node != null) {
      buf += node.value._2
      node = node.next.read()
    }

    buf
  }

  override def toString: String = {
    head.toString
  }
}
