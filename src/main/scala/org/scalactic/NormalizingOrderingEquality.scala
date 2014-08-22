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

// Todo: This can't extend NormalizingEquality[A] too because its areEqual is
// final, which makes me think I should make NormalizingEquality a trait and
// use an implementation class for NormalizingEquality that has the final
// areEqual. Reason is that we were at one point using NormalizingEquality as
// a performance optimization somewhere. So it seems like this guy should also
// be a NormalizingEquality.
abstract class NormalizingOrderingEquality[A](orderingEquality: OrderingEquality[A]) extends NormalizingEquality[A] with OrderingEquality[A] { thisNormEq =>

  // Inherit the final areEqual implementation from NormalizingEquality, adn provide this consistent
  // final compare method to go along with it.
  final def compare(a: A, b: A): Int = orderingEquality.compare(normalized(a), normalized(b))
}
