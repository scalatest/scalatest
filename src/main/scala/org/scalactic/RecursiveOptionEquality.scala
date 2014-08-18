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
package org.scalactic

import scala.language.higherKinds

trait RecursiveOptionEquality {

  implicit def recursiveOptionEquality[E, OPT[e] <: Option[e]](implicit equalityOfE: Equality[E]): Equality[OPT[E]] =
    new Equality[OPT[E]] {
      def areEqual(a: OPT[E], b: Any): Boolean = {
        (a, b) match {
          case (Some(aEle), Some(bEle)) => equalityOfE.areEqual(aEle, bEle)
          case _ => a == b // false should work here, because None on the left won't match this implicit because of element type Nothing, but just in case, will ask ==
        }
      }
    }

  implicit def someToSomeRecursiveEnabledEqualityConverting[A, B](implicit cnv: EnabledEqualityConverting[A, B]): EnabledEqualityConverting[Some[A], Some[B]] =
    EnabledEqualityConverting(
      (someA: Some[A]) => Some(cnv(someA.get))
    )

  implicit def someToOptionRecursiveEnabledEqualityConverting[A, B](implicit cnv: EnabledEqualityConverting[A, B]): EnabledEqualityConverting[Some[A], Option[B]] =
    EnabledEqualityConverting(
      (someA: Some[A]) => Option(cnv(someA.get))
    )

  implicit def optionToOptionRecursiveEnabledEqualityConverting[A, B](implicit cnv: EnabledEqualityConverting[A, B]): EnabledEqualityConverting[Option[A], Option[B]] =
    EnabledEqualityConverting(
      (optionA: Option[A]) =>
        optionA match {
          case Some(a) => Some(cnv(a))
          case None => None
        }
    )

  implicit def optionToOptionRecursiveEnabledEqualityBetween[A, OPTA[a] <: Option[a], B, OPTB[b] <: Option[b]](implicit ev: EnabledEqualityBetween[A, B]): EnabledEqualityBetween[OPTA[A], OPTB[B]] =
      EnabledEqualityBetween[OPTA[A], OPTB[B]]

}

object RecursiveOptionEquality extends RecursiveOptionEquality
