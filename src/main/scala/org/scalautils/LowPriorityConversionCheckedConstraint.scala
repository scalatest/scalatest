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
 * will be allowed if an implicit conversion exists in either direction. For example, the implicit widening conversion from
 * <code>Int</code> to <code>Long</code> will be applied on the left hand side in this expression:
 * </p>
 * 
 * <pre class="stHighlight">
 * 1 === 1L
 * </pre>
 *
 * <p>
 * But in the next expression, it will be applied on the right hand side:
 * </p>
 * 
 * <pre class="stHighlight">
 * 1L === 1
 * </pre>
 *
 * <p>
 * The first expression above is enabled by the implicit conversion <code>conversionCheckedEqualityConstraint</code> in traits
 * <a href="ConversionCheckedTripleEquals.html"><code>ConversionCheckedTripleEquals</code></a> and
 * <a href="ConversionCheckedLegacyTripleEquals.html"><code>ConversionCheckedLegacyTripleEquals</code></a>. The second expression above is
 * enabled by the implicit conversion <code>lowPriorityConversionCheckedEqualityConstraint</code> in this trait.
 * </p>
 *
 * <p>
 * The reason these two implicit methods aren't both declared in the subtraits is
 * that if implicit conversions were available in both directions, they would conflict. By placing one of them in this supertrait, the higher
 * priority conversion will be selected.
 * </p>
 */
trait LowPriorityConversionCheckedConstraint extends EqualityConstraints {

  // Inherit the Scaladoc for this method

  implicit override def lowPriorityConversionCheckedEqualityConstraint[A, B](implicit equalityOfB: Equality[B], cnv: A => B): EqualityConstraint[A, B] = new AToBEqualityConstraint[A, B](equalityOfB, cnv)
}

