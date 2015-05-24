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

import scala.language.{higherKinds, implicitConversions}
import scala.util.{Failure, Success, Try}

/**
 * An [[Equality]] that allows the comparison of values nested in [[Try]]s using whatever Equality is
 * in scope for the contained type.
 */
trait RecursiveTryEquality {
  implicit def recursiveTryEquality[A, TRY[a] <: Try[a]](implicit eqA: Equality[A]): Equality[Try[A]] =
    new Equality[Try[A]] {
      override def areEqual(t: Try[A], other: Any): Boolean = (t, other) match {
        case (Success(s), Success(otherS)) => eqA.areEqual(s, otherS)
        case (Failure(ex), Failure(otherEx)) => ex == otherEx
        case _ => false
      }
    }
}

object RecursiveTryEquality extends RecursiveTryEquality
