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

import akka.actor.ActorRef
import scala.collection.mutable.{ArrayBuffer, Buffer, Set}

import tbd.{Changeable, TBD}
import tbd.datastore.Datastore

class PartitionedModList[T, V](
    aPartitions: ArrayBuffer[ModList[T, V]])
    extends AdjustableList[T, V] {
  val partitions = aPartitions

  def map[U, Q](
      tbd: TBD,
      f: (TBD, (T, V)) => (U, Q),
      parallel: Boolean = true,
      memoized: Boolean = true): PartitionedModList[U, Q] = {
    if (parallel) {
      def innerMemoParMap(tbd: TBD, i: Int): ArrayBuffer[ModList[U, Q]] = {
        if (i < partitions.size) {
          val parTup = tbd.par((tbd: TBD) => {
            partitions(i).map(tbd, f, memoized = memoized)
          }, (tbd: TBD) => {
            innerMemoParMap(tbd, i + 1)
          })

          parTup._2 += parTup._1
        } else {
          ArrayBuffer[ModList[U, Q]]()
        }
      }

      new PartitionedModList(innerMemoParMap(tbd, 0))
    } else {
      new PartitionedModList(
        partitions.map((partition: ModList[T, V]) => {
          partition.map(tbd, f, memoized = memoized)
        })
      )
    }
  }

  def reduce(
      tbd: TBD,
      initialValueMod: Mod[(T, V)],
      f: (TBD, (T, V), (T, V)) => (T, V),
      parallel: Boolean = true,
      memoized: Boolean = true) : Mod[(T, V)] = {


    def parReduce(tbd: TBD, i: Int): Mod[(T, V)] = {
      if (i < partitions.size) {
        val parTup = tbd.par((tbd: TBD) => {
          partitions(i).reduce(tbd, initialValueMod, f, parallel, memoized)
        }, (tbd: TBD) => {
          parReduce(tbd, i + 1)
        })


        tbd.mod((dest: Dest[(T, V)]) => {
          tbd.read2(parTup._1, parTup._2)((a, b) => {
            tbd.write(dest, f(tbd, a, b))
          })
        })
      } else {
        initialValueMod
      }
    }

    if(parallel) {
      parReduce(tbd, 0)
    } else {
      partitions.map((partition: ModList[T, V]) => {
        partition.reduce(tbd, initialValueMod, f, parallel, memoized)
      }).reduce((a, b) => {
        tbd.mod((dest: Dest[(T, V)]) => {
          tbd.read2(a, b)((a, b) => {
            tbd.write(dest, f(tbd, a, b))
          })
        })
      })
    }
  }

  def filter(
      tbd: TBD,
      pred: ((T, V)) => Boolean,
      parallel: Boolean = true,
      memoized: Boolean = true): PartitionedModList[T, V] = {
    def parFilter(tbd: TBD, i: Int): ArrayBuffer[ModList[T, V]] = {
      if (i < partitions.size) {
        val parTup = tbd.par((tbd: TBD) => {
          partitions(i).filter(tbd, pred, parallel, memoized)
        }, (tbd: TBD) => {
          parFilter(tbd, i + 1)
        })

        parTup._2 += parTup._1
      } else {
        ArrayBuffer[ModList[T, V]]()
      }
    }

    if (parallel) {
      new PartitionedModList(parFilter(tbd, 0))
    } else {
      new PartitionedModList(
        partitions.map((partition: ModList[T, V]) => {
          partition.filter(tbd, pred, parallel, memoized)
        })
      )
    }
  }

  /* Meta Operations */
  def toBuffer(): Buffer[V] = {
    val buf = ArrayBuffer[V]()

    for (partition <- partitions) {
      var innerNode = partition.head.read()
      while (innerNode != null) {
        buf += innerNode.value._2
        innerNode = innerNode.next.read()
      }
    }

    buf
  }
}
