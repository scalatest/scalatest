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
import scala.util.{Try,Success,Failure}

trait LowPriorityEqualityConstraints {

  import EqualityPolicy.BasicEqualityConstraint

  // ELG Element Left Good
  // ELB Element Left Bad
  // ERG Element Right Good
  // ERB Element Right Bad
  // This one will provide an equality constraint if the Bad types have an inner constraint. It doesn't matter
  // in this case what the Good type does. If there was a constraint available for the Good types, then it would
  // use the higher priority implicit Constraint.orEqualityConstraint and never get here. 
  implicit def lowPriorityOrEqualityConstraint[ELG, ELB, ERG, ERB](implicit equalityOfL: Equality[Or[ELG, ELB]], ev: EqualityConstraint[ELB, ERB] with Cooperative): EqualityConstraint[Or[ELG, ELB], Or[ERG, ERB]] with Cooperative = new BasicEqualityConstraint[Or[ELG, ELB], Or[ERG, ERB]](equalityOfL)

  implicit def lowPriorityOrOnBothSidesWithGoodNothingConstraint[ELB, ERB](implicit equalityOfL: Equality[Or[Nothing, ELB]], ev: EqualityConstraint[ELB, ERB] with Cooperative): EqualityConstraint[Or[Nothing, ELB], Or[Nothing, ERB]] with Cooperative = new BasicEqualityConstraint[Or[Nothing, ELB], Or[Nothing, ERB]](equalityOfL)

  // This must be low priority to allow Every on both sides
  implicit def everyOnRightEqualityConstraint[EA, CA[ea] <: Every[ea], EB](implicit equalityOfA: Equality[CA[EA]], ev: EqualityConstraint[EA, EB] with Cooperative): EqualityConstraint[CA[EA], Every[EB]] with Cooperative = new BasicEqualityConstraint[CA[EA], Every[EB]](equalityOfA)

  // Either (in x === y, x is the "target" of the === invocation, y is the "parameter")
  // ETL Element Target Left
  // ETR Element Target Right
  // EPL Element Parameter Left
  // EPR Element Parameter Right
  // This one will provide an equality constraint if the Bad types have an inner constraint. It doesn't matter
  // in this case what the Good type does. If there was a constraint available for the Good types, then it would
  // use the higher priority implicit Constraint.orEqualityConstraint and never get here. 
  implicit def lowPriorityEitherEqualityConstraint[ETL, ETR, EPL, EPR](implicit equalityOfT: Equality[Either[ETL, ETR]], ev: EqualityConstraint[ETR, EPR] with Cooperative): EqualityConstraint[Either[ETL, ETR], Either[EPL, EPR]] with Cooperative = new BasicEqualityConstraint[Either[ETL, ETR], Either[EPL, EPR]](equalityOfT)
  implicit def lowPriorityEitherNothingConstraint[ETR, EPR](implicit equalityOfT: Equality[Either[Nothing, ETR]], ev: EqualityConstraint[ETR, EPR] with Cooperative): EqualityConstraint[Either[Nothing, ETR], Either[Nothing, EPR]] with Cooperative = new BasicEqualityConstraint[Either[Nothing, ETR], Either[Nothing, EPR]](equalityOfT)

  // This must be low priority to allow Option on both sides
  implicit def optionOnRightEqualityConstraint[EA, CA[ea] <: Option[ea], EB](implicit equalityOfA: Equality[CA[EA]], ev: EqualityConstraint[EA, EB] with Cooperative): EqualityConstraint[CA[EA], Option[EB]] with Cooperative = new BasicEqualityConstraint[CA[EA], Option[EB]](equalityOfA)

  // This must be low priority to allow Try on both sides
  implicit def tryOnRightEqualityConstraint[EA, CA[ea] <: Try[ea], EB](implicit equalityOfA: Equality[CA[EA]], ev: EqualityConstraint[EA, EB] with Cooperative): EqualityConstraint[CA[EA], Try[EB]] with Cooperative = new BasicEqualityConstraint[CA[EA], Try[EB]](equalityOfA)

}
