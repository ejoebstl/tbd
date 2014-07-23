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

import java.io.Serializable

import tbd.{Changeable, Changeable2, Memoizer, TBD}

class DoubleModListNode[T, V] (
    var value: Mod[(T, V)],
    val next: Mod[DoubleModListNode[T, V]]
  ) extends Iterator[T, V, DoubleModListNode[T, V]] with Serializable {

  override def equals(obj: Any): Boolean = {
    if (!obj.isInstanceOf[DoubleModListNode[T, V]]) {
      false
    } else {
      val that = obj.asInstanceOf[DoubleModListNode[T, V]]
      that.value == value && that.next == next
    }
  }

  def map[U, Q](
      tbd: TBD,
      f: (TBD, (T, V)) => (U, Q),
      memo: Memoizer[(Mod[(U, Q)], Mod[DoubleModListNode[U, Q]])])
        : Changeable[DoubleModListNode[U, Q]] = {
    val pair = memo(List(next)) {
      (tbd.modNoDest(() =>
	tbd.read(value)(value => tbd.writeNoDest(f(tbd, value)))),
       tbd.modNoDest(() =>
         tbd.read(next)(next => {
           if (next != null) {
               next.map(tbd, f, memo)
           } else {
             tbd.writeNoDest[DoubleModListNode[U, Q]](null)
           }
         })
      ))
    }

    tbd.writeNoDest(new DoubleModListNode[U, Q](pair._1, pair._2))
  }

  def parMap[U, Q](
      tbd: TBD,
      dest: Dest[DoubleModListNode[U, Q]],
      f: (TBD, (T, V)) => (U, Q)): Changeable[DoubleModListNode[U, Q]] = {
    val modTuple =
      tbd.par((tbd: TBD) => {
	      tbd.mod((valueDest: Dest[(U, Q)]) => {
          tbd.read(value)(value => {
            tbd.write(valueDest, f(tbd, value))
          })
        })
      }, (tbd: TBD) => {
        tbd.mod((nextDest: Dest[DoubleModListNode[U, Q]]) => {
	        tbd.read(next)(next => {
            if (next != null) {
              next.parMap(tbd, nextDest, f)
            } else {
              tbd.write(nextDest, null)
            }
          })
        })
      })
    tbd.write(dest, new DoubleModListNode[U, Q](modTuple._1, modTuple._2))
  }

  def split(
      tbd: TBD,
      destMatch: Dest[DoubleModListNode[T, V]],
      destNoMatch: Dest[DoubleModListNode[T, V]],
      memo: Memoizer[(Mod[DoubleModListNode[T, V]], Mod[DoubleModListNode[T, V]])],
      pred: (TBD, (T, V)) => Boolean,
      parallel: Boolean = false,
      memoized: Boolean = false):
        Changeable[DoubleModListNode[T, V]] = {

    val (matchNext, diffNext) = memo(List(next)) {
      tbd.mod2((newDestMatch: Dest[DoubleModListNode[T, V]], newDestNoMatch: Dest[DoubleModListNode[T, V]]) => {
        tbd.read(next)(next => {
          if(next != null) {
            next.split(tbd, newDestMatch, newDestNoMatch, memo, pred)
          } else {
            tbd.write(newDestMatch, null)
            tbd.write(newDestNoMatch, null)
          }
        })
      })
    }

    tbd.read(value)((v) => {
      if(pred(tbd, (v._1, v._2))) {
        tbd.write(destMatch, new DoubleModListNode(tbd.createMod(v), matchNext))
        tbd.read(diffNext)(diffNext => {
          tbd.write(destNoMatch, diffNext)
        })
      } else {
        tbd.write(destNoMatch, new DoubleModListNode(tbd.createMod(v), diffNext))
        tbd.read(matchNext)(matchNext => {
          tbd.write(destMatch, matchNext)
        })
      }
    })
  }

  def splitNoDest(
      tbd: TBD,
      memo: Memoizer[Changeable2[DoubleModListNode[T, V], DoubleModListNode[T, V]]],
      pred: (TBD, (T, V)) => Boolean,
      parallel: Boolean = false,
      memoized: Boolean = false):
        Changeable2[DoubleModListNode[T, V], DoubleModListNode[T, V]] = {
    tbd.read(value)((v) => {
      if(pred(tbd, v)) {
	val (matchNext, diffNext) =
	  tbd.modNoDestLeft(() => {
	    memo(List(next)) {
	      tbd.read(next)(next => {
		if(next != null) {
		  next.splitNoDest(tbd, memo, pred)
		} else {
		  tbd.writeNoDest2(null.asInstanceOf[DoubleModListNode[T, V]],
				   null.asInstanceOf[DoubleModListNode[T, V]])
		}
              })
	    }
	  })

	tbd.writeNoDestLeft(new DoubleModListNode(value, matchNext), diffNext)
      } else {
	val (matchNext, diffNext) =
	  tbd.modNoDestRight(() => {
	    memo(List(next)) {
	      tbd.read(next)(next => {
		if(next != null) {
		  next.splitNoDest(tbd, memo, pred)
		} else {
		  tbd.writeNoDest2(null.asInstanceOf[DoubleModListNode[T, V]],
				   null.asInstanceOf[DoubleModListNode[T, V]])
		}
	      })
	    }
	  })

	tbd.writeNoDestRight(matchNext, new DoubleModListNode(value, diffNext))
      }
    })
  }

  def quicksort(
        tbd: TBD,
        dest: Dest[DoubleModListNode[T, V]],
        toAppend: Mod[DoubleModListNode[T, V]],
        comperator: (TBD, (T, V), (T, V)) => Boolean,
        memo: Memoizer[Mod[DoubleModListNode[T, V]]],
        parallel: Boolean = false,
        memoized: Boolean = false):
          Changeable[DoubleModListNode[T, V]] = {
    tbd.read(next)(next => {
      if(next != null) {
        tbd.read(value)(v => {
          val (smaller, greater) = tbd.mod2((destSmaller: Dest[DoubleModListNode[T, V]],
                                      destGreater: Dest[DoubleModListNode[T, V]]) => {

            val memo = tbd.makeMemoizer[(Mod[DoubleModListNode[T, V]],
                                     Mod[DoubleModListNode[T, V]])](!memoized)

            next.split(tbd, destSmaller, destGreater, memo,
              (tbd, cv) => { comperator(tbd, cv, v) },
              parallel, memoized)
          })

          val greaterSorted = memo(List(greater)) {
            tbd.mod((dest: Dest[DoubleModListNode[T, V]]) => {
              tbd.read(greater)(greater => {
                if(greater != null) {
                  greater.quicksort(tbd, dest, toAppend,
                                    comperator, memo, parallel, memoized)
                } else {
                  tbd.read(toAppend)(toAppend => {
                    tbd.write(dest, toAppend)
                  })
                }
              })
            })
          }

          val mid = new DoubleModListNode(value, greaterSorted)

          tbd.read(smaller)(smaller => {
            if(smaller != null) {
              smaller.quicksort(tbd, dest, tbd.createMod(mid),
                                comperator, memo, parallel, memoized)
            } else {
              tbd.write(dest, mid)
            }
          })
        })
      } else {
        tbd.write(dest, new DoubleModListNode(value, toAppend))
      }
    })
  }

  def binaryHash(id: T, round: Int, hasher: Hasher) = {
    hasher.hash(id.hashCode() ^ round) == 0
  }

  def filter(
      tbd: TBD,
      dest: Dest[DoubleModListNode[T, V]],
      pred: ((T, V)) => Boolean,
      memo: Memoizer[Mod[DoubleModListNode[T, V]]])
        : Changeable[DoubleModListNode[T, V]] = {
    tbd.read(value)(value => {
      if (pred(value)) {
        val newNext = memo(List(next)) {
          tbd.mod((nextDest: Dest[DoubleModListNode[T, V]]) => {
            tbd.read(next)(nextValue => {
                if (nextValue == null) {
                  tbd.write(nextDest, null)
                } else {
                  nextValue.filter(tbd, nextDest, pred, memo)
                }
              })
            })
        }
        tbd.write(dest, new DoubleModListNode(tbd.createMod(value), newNext))
      } else {
        tbd.read(next)(nextValue => {
          if (nextValue == null) {
            tbd.write(dest, null)
          } else {
            nextValue.filter(tbd, dest, pred, memo)
          }
        })
      }
    })
  }
}
