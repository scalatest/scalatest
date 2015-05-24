/*
 * Copyright 2001-2014 Artima, Inc.
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

import org.scalactic.{EnabledEqualityBetween, Equality}

import scala.language.higherKinds

/**
 * An [[Equality]] that allows the comparison of values nested in [[Option]]s using whatever Equality is
 * in scope for the contained type.
 */
trait RecursiveOptionEquality {

  // The following does not work in 2.10, it breaks implicit lookup somehow and breaks matchers tests compile.
  // We can use the following when we no longer need to support Scala 2.11.
  /*implicit def recursiveOptionEquality[E, OPT[e] <: Option[e]](implicit equalityOfE: Equality[E]): Equality[OPT[E]] =
    new Equality[OPT[E]] {
      def areEqual(a: OPT[E], b: Any): Boolean = {
        (a, b) match {
          case (Some(aEle), Some(bEle)) => equalityOfE.areEqual(aEle, bEle)
          case _ => a == b // false should work here, because None on the left won't match this implicit because of element type Nothing, but just in case, will ask ==
        }
      }
    }*/

  implicit def recursiveOptionEquality[E](implicit equalityOfE: Equality[E]): Equality[Option[E]] =
    new Equality[Option[E]] {
      def areEqual(a: Option[E], b: Any): Boolean = {
        (a, b) match {
          case (Some(aEle), Some(bEle)) => equalityOfE.areEqual(aEle, bEle)
          case _ => a == b // false should work here, because None on the left won't match this implicit because of element type Nothing, but just in case, will ask ==
        }
      }
    }

  implicit def recursiveSomeEquality[E](implicit equalityOfE: Equality[E]): Equality[Some[E]] =
    new Equality[Some[E]] {
      def areEqual(a: Some[E], b: Any): Boolean = {
        (a, b) match {
          case (Some(aEle), Some(bEle)) => equalityOfE.areEqual(aEle, bEle)
          case _ => a == b // false should work here, because None on the left won't match this implicit because of element type Nothing, but just in case, will ask ==
        }
      }
    }
}

object RecursiveOptionEquality extends RecursiveOptionEquality
