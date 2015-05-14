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

import scala.language.higherKinds
import scala.language.implicitConversions

trait RecursiveSeqEquality {

  implicit def recursiveSeqEquality[E, SEQ[e] <: collection.GenSeq[e]](implicit equalityOfE: Equality[E]): Equality[SEQ[E]] =
    new Equality[SEQ[E]] {
      def areEqual(seqA: SEQ[E], b: Any): Boolean = {
        b match {
          case seqB: collection.GenSeq[_] =>
            seqA.length == seqB.length && (seqA zip seqB).forall {
              case (eleA, eleB) => equalityOfE.areEqual(eleA, eleB)
            }
          case _ => false
        }
      }
    }
}

object RecursiveSeqEquality extends RecursiveSeqEquality
