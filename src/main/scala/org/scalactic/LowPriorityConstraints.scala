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

import annotation.implicitNotFound
import scala.language.higherKinds

trait LowPriorityConstraints {

  import TripleEqualsSupport.EqualityConstraint

  // ELG Element Left Good
  // ELB Element Left Bad
  // ORL Or Left
  // ERG Element Right Good
  // ERB Element Right Bad
  // ORR Or Right
  // This one will provide an equality constraint if the Bad types have an inner constraint. It doesn't matter
  // in this case what the Good type does. If there was a constraint available ofr the Good types, then it would
  // use the higher priority implicit Constraint.orEqualityConstraint and never get here. 
/*
  implicit def lowPriorityOrEqualityConstraint[ELG, ELB, ORL[elg, erb] <: Or[elg, erb], ERG, ERB, ORR[erg, erb] <: Or[erg, erb]](implicit equalityOfA: Equality[ORL[ELG, ELB]], ev: InnerConstraint[ELB, ERB]): Constraint[ORL[ELG, ELB], ORR[ERG, ERB]] = new EqualityConstraint[ORL[ELG, ELB], ORR[ERG, ERB]](equalityOfA)
*/
  implicit def lowPriorityOrEqualityConstraint[ELG, ELB, ERG, ERB](implicit equalityOfL: Equality[Or[ELG, ELB]], ev: InnerConstraint[ELB, ERB]): Constraint[Or[ELG, ELB], Or[ERG, ERB]] = new EqualityConstraint[Or[ELG, ELB], Or[ERG, ERB]](equalityOfL)
}
