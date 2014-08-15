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

trait RecursiveOptionEquality {
  implicit def recursiveSomeEquality[E](implicit equalityOfE: Equality[E]): Equality[Some[E]] =
    new Equality[Some[E]] {
      def areEqual(a: Some[E], b: Any): Boolean =
        b match {
          case Some(bEle) => equalityOfE.areEqual(a.get, bEle)
          case _ => false
        }
    }

  implicit def recursiveOptionEquality[E](implicit equalityOfE: Equality[E]): Equality[Option[E]] =
    new Equality[Option[E]] {
      def areEqual(a: Option[E], b: Any): Boolean =
        (a, b) match {
          case (Some(aEle), Some(bEle)) => equalityOfE.areEqual(aEle, bEle)
          case (None, None) => true
          case _ => false
        }
    }
}

object RecursiveOptionEquality extends RecursiveOptionEquality
