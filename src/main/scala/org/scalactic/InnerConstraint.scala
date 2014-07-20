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
package org.scalactic

import annotation.implicitNotFound
import scala.language.higherKinds

/**
 * Abstract class used to enforce type constraints for equality checks.
 *
 * <p>
 * For more information on how this class is used, see the documentation of <a href="TripleEqualsSupport.html"><code>TripleEqualsSupport</code></a>.
 * </p>
 */
class InnerConstraint[A, B]

trait EvenLowerPriorityInnerConstraints {
  import scala.language.implicitConversions
  implicit def numericEqualityInnerConstraint[A, B](implicit numA: CooperatingNumeric[A], numB: CooperatingNumeric[B]): InnerConstraint[A, B] = new InnerConstraint[A, B]
}

trait LowPriorityInnerConstraints extends EvenLowerPriorityInnerConstraints {
  import scala.language.implicitConversions
  implicit def lowPriorityTypeCheckedInnerConstraint[A, B](implicit ev: A <:< B): InnerConstraint[A, B] = new InnerConstraint[A, B]
}

object InnerConstraint extends LowPriorityInnerConstraints {
  import scala.language.implicitConversions
  implicit def typeCheckedInnerConstraint[A, B](implicit ev: B <:< A): InnerConstraint[A, B] = new InnerConstraint[A, B]
}

