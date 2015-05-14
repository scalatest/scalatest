/*
 * Copyright 2001-2015 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalactic

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.higherKinds
import scala.language.implicitConversions

trait RecursiveMapEquality {

  implicit def recursiveMapEquality[K, V, MAP[k, v] <: collection.GenMap[k, v]](implicit eqK: Equality[K], eqV: Equality[V]): Equality[MAP[K, V]] =
    new Equality[MAP[K, V]] {
      def areEqual(mapA: MAP[K, V], b: Any): Boolean = {

        @tailrec
        def nextElement(mapA: mutable.Buffer[(K,V)], mapB: mutable.Buffer[(_,_)]): Boolean = (mapA, mapB) match {
          case (ma, mb) if ma.length == 0 && mb.length == 0 => true
          case (ma, mb) if ma.length == 0 || mb.length == 0 => false
          case (ma, mb) =>
            val elemA = ma.head
            val index = mb.indexWhere(kv => eqK.areEqual(elemA._1, kv._1) && eqV.areEqual(elemA._2, kv._2))
            if (index < 0) {
              false
            } else {
              mb.remove(index)
              nextElement(ma.tail, mb)
            }
        }

        b match {
          case mapB: collection.GenMap[_, _] => nextElement(mapA.toBuffer, mapB.toBuffer)
          case _ => false
        }
      }
    }
}

object RecursiveMapEquality extends RecursiveMapEquality
