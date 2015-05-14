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
import scala.collection.{GenSeq, mutable}
import scala.language.higherKinds
import scala.language.implicitConversions

trait RecursiveSetEquality {

  implicit def recursiveSetEquality[E, SET[e] <: collection.GenSet[e]](implicit equalityOfE: Equality[E]): Equality[SET[E]] =
    new Equality[SET[E]] {
      def areEqual(setA: SET[E], b: Any): Boolean = {

        @tailrec
        def nextElement(seqA: GenSeq[E], bufB: mutable.Buffer[_]): Boolean = (seqA, bufB) match {
          case (a, b) if a.length == 0 && b.length == 0 => true
          case (a, b) if a.length == 0 || b.length == 0 => false
          case (a, b) =>
            val index = b.indexWhere(equalityOfE.areEqual(a.head, _))
            if (index < 0) {
              false
            } else {
              b.remove(index)
              nextElement(a.tail, b)
            }
        }

        b match {
          case setB: collection.GenSet[_] => nextElement(setA.toSeq, setB.toBuffer)
          case _ => false
        }
      }
    }
}

object RecursiveSetEquality extends RecursiveSetEquality
