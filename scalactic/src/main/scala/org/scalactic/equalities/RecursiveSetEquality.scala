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
package org.scalactic.equalities

import org.scalactic.Equality

import scala.annotation.tailrec
import scala.collection.{GenSeq, mutable}
import scala.language.{higherKinds, implicitConversions}

/**
 * An [[Equality]] that allows the comparison of values nested in [[Set]]s using whatever Equality is
 * in scope for the contained type.
 */
trait RecursiveSetEquality {
  implicit def recursiveSetEquality[E, SET[e] <: collection.GenSet[e]](implicit eqE: Equality[E]): Equality[SET[E]] =
    new Equality[SET[E]] {
      def areEqual(setA: SET[E], b: Any): Boolean = {

        @tailrec
        def nextElement(seqA: GenSeq[E], bufB: mutable.Buffer[_]): Boolean = (seqA, bufB) match {
          case (a, b) if a.isEmpty && b.isEmpty => true
          case (a, b) if a.isEmpty || b.isEmpty => false
          case (a, b) =>
            val index = b.indexWhere(eqE.areEqual(a.head, _))
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
