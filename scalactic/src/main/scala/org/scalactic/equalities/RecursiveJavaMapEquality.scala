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
 * An [[Equality]] that allows the comparison of values nested in [[java.util.Map]]s using whatever Equality is
 * in scope for the contained key and value types.
 */
trait RecursiveJavaMapEquality {
  implicit def recursiveJavaMapEquality[K, V, JMAP[k, v] <: java.util.Map[k, v]](implicit eqK: Equality[K], eqV: Equality[V]): Equality[JMAP[K, V]] =
    new Equality[JMAP[K, V]] {
      import collection.JavaConverters._

      val scalaEq = RecursiveMapEquality.recursiveMapEquality[K, V, collection.GenMap]
      def areEqual(jmap: JMAP[K, V], other: Any): Boolean = (jmap, other) match {
        case (mapA: java.util.Map[K,V], mapB: java.util.Map[_,_]) => scalaEq.areEqual(mapA.asScala, mapB.asScala)
      }
    }
}

object RecursiveJavaMapEquality extends RecursiveJavaMapEquality
