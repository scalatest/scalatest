/*
 * Copyright 2001-2013 Artima, Inc.
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
package org.scalautils

/**
 * Provides an implicit conversion that will be applied only if a higher-priority implicit conversion declared a subtrait
 * is not applicable.
 *
 * <p>
 * The purpose of this trait is to make the <code>===</code> operator symetric. In other words, a <code>===</code> invocation
 * will be allowed if subtype relationship exists in either direction. For example, in the following expression, the left hand
 * side is a subtype of the right hand side:
 * </p>
 * 
 * <pre class="stHighlight">
 * List(1, 2, 3) === Seq(1, 2, 3)
 * </pre>
 *
 * <p>
 * But in the next expression, it the right hand side is a subtype of the left hand side
 * </p>
 * 
 * <pre class="stHighlight">
 * Seq(1, 2, 3) === List(1, 2, 3)
 * </pre>
 *
 * <p>
 * The first expression above is enabled by the implicit conversion <code>typeCheckedEqualityConstraint</code> in traits
 * <a href="TypeCheckedTripleEquals.html"><code>TypeCheckedTripleEquals</code></a> and
 * <a href="TypeCheckedLegacyTripleEquals.html"><code>TypeCheckedLegacyTripleEquals</code></a>. The second expression above is
 * enabled by the implicit conversion <code>lowPriorityTypeCheckedEqualityConstraint</code> in this trait.
 * </p>
 *
 * <p>
 * The reason these two implicit methods aren't both declared in the subtraits is
 * that if the subtype relationship existed in both directions, they would conflict. This can happen when the exact same type is on both
 * the left and right hand sides, because a type is a subtype of itself. By placing one of them in this supertrait, the higher
 * priority conversion will be selected.
 * </p>
 */
trait LowPriorityTypeCheckedConstraint extends EqualityConstraints {
  // Inherit the scaladoc for this method
  implicit override def lowPriorityTypeCheckedEqualityConstraint[A, B](implicit equalityOfA: Equality[A], ev: A <:< B): EqualityConstraint[A, B] = new BasicEqualityConstraint[A, B](equalityOfA)
}

