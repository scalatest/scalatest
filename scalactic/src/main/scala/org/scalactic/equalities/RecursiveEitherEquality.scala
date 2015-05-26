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
 * An [[Equality]] that allows the comparison of values nested in [[Either]]s using whatever Equality is
 * in scope for the [[Left]] and/or [[Right]] contained type.
 */
trait RecursiveEitherEquality {
  implicit def recursiveEitherEquality[L, R, EITHER[l, r] <: Either[l, r]](implicit eqL: Equality[L], eqR: Equality[R]): Equality[EITHER[L, R]] =
    new Equality[EITHER[L, R]] {
      override def areEqual(either: EITHER[L, R], other: Any): Boolean = (either, other) match {
        case (Left(l), Left(otherL)) => eqL.areEqual(l, otherL)
        case (Right(r), Right(otherR)) => eqR.areEqual(r, otherR)
        case _ => false
      }
    }

  // This is a Left
  implicit def recursiveRightNothingEquality[L, EITHER[l, r] <: Either[l, r]](implicit eqL: Equality[L]): Equality[EITHER[L, Nothing]] =
    new Equality[EITHER[L, Nothing]] {
      override def areEqual(either: EITHER[L, Nothing], other: Any): Boolean = (either, other) match {
        case (Left(l), Left(otherL)) => eqL.areEqual(l, otherL)
        case _ => false
      }
    }

  // This is a Right
  implicit def recursiveLeftNothingEquality[R, EITHER[l, r] <: Either[l, r]](implicit eqR: Equality[R]): Equality[EITHER[Nothing, R]] =
    new Equality[EITHER[Nothing, R]] {
      override def areEqual(either: EITHER[Nothing, R], other: Any): Boolean = (either, other) match {
        case (Right(r), Right(otherR)) => eqR.areEqual(r, otherR)
        case _ => false
      }
    }
}

object RecursiveEitherEquality extends RecursiveEitherEquality
