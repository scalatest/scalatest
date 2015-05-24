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
 * An [[Equality]] that allows the comparison of values nested in [[java.util.Set]]s using whatever Equality is
 * in scope for the contained type.
 */
trait RecursiveJavaSetEquality {
  implicit def recursiveJavaSetEquality[E, JSET[e] <: java.util.Set[e]](implicit eqE: Equality[E]): Equality[JSET[E]] =
    new Equality[JSET[E]] {
      import collection.JavaConverters._

      val scalaEq = RecursiveSetEquality.recursiveSetEquality[E, collection.GenSet]
      def areEqual(jset: JSET[E], other: Any): Boolean = (jset, other) match {
        case (setA: java.util.Set[E], setB: java.util.Set[_]) => scalaEq.areEqual(
          collection.immutable.Set[E](setA.asScala.toSeq:_*), setB.asScala)
        case _ => false
      }
    }
}

object RecursiveJavaSetEquality extends RecursiveJavaSetEquality
