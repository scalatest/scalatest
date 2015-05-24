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

/**
 * An [[Equality]] that allows the comparison of values nested in [[Left]]s using whatever Equality is
 * in scope for the contained type.
 */

trait RecursiveLeftEquality {
  implicit def recursiveLeftEquality[L, R, LEFT[l] <: Left[l, R]](implicit eqL: Equality[L]): Equality[Left[L, R]] =
    new Equality[Left[L, R]] {
      override def areEqual(left: Left[L, R], other: Any): Boolean = (left, other) match {
        case (Left(l), Left(otherL)) => eqL.areEqual(l, otherL)
        case _ => false
      }
    }
}

/**
 * An [[Equality]] that allows the comparison of values nested in [[Right]]s using whatever Equality is
 * in scope for the contained type.
 */

trait RecursiveRightEquality {
  implicit def recursiveRightEquality[L, R, LEFT[l] <: Right[l, R]](implicit eqR: Equality[R]): Equality[Right[L, R]] =
    new Equality[Right[L, R]] {
      override def areEqual(right: Right[L, R], other: Any): Boolean = (right, other) match {
        case (Right(r), Right(otherR)) => eqR.areEqual(r, otherR)
        case _ => false
      }
    }
}


object RecursiveEitherEquality extends RecursiveLeftEquality with RecursiveRightEquality
