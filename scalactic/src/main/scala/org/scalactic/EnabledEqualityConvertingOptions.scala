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

trait EnabledEqualityConvertingOptions extends RecursiveOptionEquality {

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
}

object EnabledEqualityConvertingOptions extends EnabledEqualityConvertingOptions
