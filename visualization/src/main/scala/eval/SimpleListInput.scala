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

import scala.collection.mutable.Map
import scala.concurrent.{Await, ExecutionContext, Future}

import tbd.{Mod, Input}
import tbd.Constants._
import tbd.datastore.Datastore

class SimpleListInput[ListElement] extends Input[Int, ListElement] {
    import scala.concurrent.ExecutionContext.Implicits.global

  private var head: Mod[SimpleList[ListElement]] = null
  private var tail = head
  private val table = Map[Int, Mod[SimpleList[ListElement]]]()

  def put(key: Int, value: ListElement) = {

    //We could place this global, but then, we get repeating ID's
    //from the datastore - no idea why.
    if(head == null) {
      head = Datastore.createMod(null)
      tail = head
    }

    val oldTail = tail
    tail = Datastore.createMod(null)
    val newNode = new SimpleList(value, key, tail)
    val future = Datastore.updateMod(oldTail.id, newNode)

    table(key) = oldTail

    Await.result(Future.sequence(future), DURATION)
  }

  def update(key: Int, value: ListElement) = {
    val mod = table(key)
    val item = Datastore.getMod(mod.id).asInstanceOf[SimpleList[ListElement]]
    val newNode = new SimpleList(value, key, item.next)

    var future = Datastore.updateMod(mod.id, newNode)

    Await.result(Future.sequence(future), DURATION)
  }

  def remove(key: Int) = {
    val mod = table(key)
    val item = Datastore.getMod(mod.id).asInstanceOf[SimpleList[ListElement]]
    val nextMod = item.next
    val nextItem = Datastore.getMod(item.next.id).asInstanceOf[SimpleList[ListElement]]

    val future = Datastore.updateMod(mod.id, nextItem)
    Datastore.removeMod(nextMod.id)

    table -= key

    if(nextItem == null) {
      tail = mod
    } else {
      table(nextItem.key) = mod
    }

    Await.result(Future.sequence(future), DURATION)
  }

import SimpleListMod._
import scala.language.implicitConversions

  def getList() : Mod[SimpleList[ListElement]] = {
    head
  }
}
