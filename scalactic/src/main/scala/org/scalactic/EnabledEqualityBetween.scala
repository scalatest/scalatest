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

class EnabledEqualityBetween[A, B]

object EnabledEqualityBetween {
  def apply[A, B]: EnabledEqualityBetween[A, B] = new EnabledEqualityBetween[A, B]
  // THis is needed to allow under EnabledEquality an implicit like
  // enablers.ContainingConstraint.containingNatureOfJavaMap, which needs an implicit
  // EqualityConstraint[java.util.Map.Entry[K, V], R]. Well R could be org.scalatest.Entry,
  // and that's a subtype. So with this implicit, will always allow under EnabledEquality
  // any subtype of java.util.Map.Entry with any other subtype of java.util.Map.Entry.
  implicit def enabledEqualityBetweenJavaMapEntries[A, B, ENTRYA[a, b] <: java.util.Map.Entry[a, b], ENTRYB[a, b] <: java.util.Map.Entry[a, b]]: EnabledEqualityBetween[ENTRYA[A, B], ENTRYB[A, B]] = EnabledEqualityBetween[ENTRYA[A, B], ENTRYB[A, B]]
}

