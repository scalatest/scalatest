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
 * An [[Equality]] that allows the comparison of values nested in [[java.util.List]]s using whatever Equality is
 * in scope for the contained type.
 */
trait RecursiveJavaListEquality {
  implicit def recursiveJavaListEquality[E, JLIST[e] <: java.util.List[e]](implicit eqE: Equality[E]): Equality[JLIST[E]] =
    new Equality[JLIST[E]] {
      import collection.JavaConverters._

      val scalaEq = RecursiveSeqEquality.recursiveSeqEquality[E, Seq](eqE)
      def areEqual(seqA: JLIST[E], other: Any): Boolean = (seqA, other) match {
        case (jlistA: java.util.List[E], jlistB: java.util.List[_]) => scalaEq.areEqual(jlistA.asScala.toSeq, jlistB.asScala.toSeq)
        case _ => false
      }
    }

}

object RecursiveJavaListEquality extends RecursiveJavaListEquality
