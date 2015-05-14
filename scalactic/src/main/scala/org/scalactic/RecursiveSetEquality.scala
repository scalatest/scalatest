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

trait RecursiveSetEquality {

  implicit def recursiveSetEquality[E, SET[e] <: collection.GenSet[e]](implicit equalityOfE: Equality[E]): Equality[SET[E]] =
    new Equality[SET[E]] {
      def areEqual(setA: SET[E], b: Any): Boolean = {

        @tailrec
        def nextElement(bufA: mutable.Buffer[E], bufB: mutable.Buffer[_]): Boolean = (bufA, bufB) match {
          case (ba, bb) if ba.length == 0 && bb.length == 0 => true
          case (ba, bb) if ba.length == 0 || bb.length == 0 => false
          case (ba, bb) =>
            val index = bb.indexWhere(equalityOfE.areEqual(ba.head, _))
            if (index < 0) {
              false
            } else {
              bb.remove(index)
              nextElement(ba.tail, bb)
            }
        }

        b match {
          case setB: collection.GenSet[_] => nextElement(setA.toBuffer, setB.toBuffer)
          case _ => false
        }
      }
    }
}

object RecursiveSetEquality extends RecursiveSetEquality
