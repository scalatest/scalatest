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
trait EvenLowerPriorityInnerConstraints extends ExtremelyLowPriorityInnerConstraints {
  import scala.language.implicitConversions
  implicit def numericEqualityInnerConstraint[A, B](implicit numA: CooperatingNumeric[A], numB: CooperatingNumeric[B]): InnerConstraint[A, B] = new InnerConstraint[A, B]
  implicit def seqEqualityConstraint[EA, CA[ea] <: collection.GenSeq[ea], EB, CB[eb] <: collection.GenSeq[eb]](implicit ev: InnerConstraint[EA, EB]): InnerConstraint[CA[EA], CB[EB]] = new InnerConstraint[CA[EA], CB[EB]]

  implicit def setEqualityConstraint[EA, CA[ea] <: collection.GenSet[ea], EB, CB[eb] <: collection.GenSet[eb]](implicit ev: InnerConstraint[EA, EB]): InnerConstraint[CA[EA], CB[EB]] = new InnerConstraint[CA[EA], CB[EB]]

  implicit def mapEqualityConstraint[KA, VA, CA[ka, kb] <: collection.GenMap[ka, kb], KB, VB, CB[kb, vb] <: collection.GenMap[kb, vb]](implicit evKey: InnerConstraint[KA, KB], evValue: InnerConstraint[VA, VB]): InnerConstraint[CA[KA, VA], CB[KB, VB]] = new InnerConstraint[CA[KA, VA], CB[KB, VB]]

  // 1. Every on left, can by subclass of Every on right
  // 2. Every on right, can be subclass of Every on left
  // 3. One on left, can be One or Every on right, but the latter will be provided by number 2
  // 4. One on right, can be One or Every on left, but the latter will be provided by number 1
  // 5. Many on left, can be Many or Every on right, but the latter will be provided by number 2
  // 6. Many on right, can be Many or Every on left, but the latter will be provided by number 1
  implicit def everyOnLeftEqualityConstraint[EA, EB, CB[eb] <: Every[eb]](implicit ev: InnerConstraint[EA, EB]): InnerConstraint[Every[EA], CB[EB]] = new InnerConstraint[Every[EA], CB[EB]]

  implicit def oneOnBothSidesqualityConstraint[EA, EB](implicit ev: InnerConstraint[EA, EB]): InnerConstraint[One[EA], One[EB]] = new InnerConstraint[One[EA], One[EB]]

  implicit def manyOnBothSidesEqualityConstraint[EA, EB](implicit ev: InnerConstraint[EA, EB]): InnerConstraint[Many[EA], Many[EB]] = new InnerConstraint[Many[EA], Many[EB]]

  // ELG Element Left Good
  // ELB Element Left Bad
  // ERG Element Right Good
  // ERB Element Right Bad
  // This one will provide an equality constraint if the Good types have an inner constraint. It doesn't matter
  // in this case what the Bad type does. If there isn't one for the Good type, the lower priority implicit method
  // LowPriorityConstraints.lowPriorityOrEqualityConstraint will be checked will see
  // If there's an InnerConstraint for the Bad types.
  implicit def orEqualityConstraint[ELG, ELB, ERG, ERB](implicit ev: InnerConstraint[ELG, ERG]): InnerConstraint[Or[ELG, ELB], Or[ERG, ERB]] = new InnerConstraint[Or[ELG, ELB], Or[ERG, ERB]]

  implicit def goodOnLeftOrOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit ev: InnerConstraint[ELG, ERG]): InnerConstraint[Good[ELG, ELB], Or[ERG, ERB]] = new InnerConstraint[Good[ELG, ELB], Or[ERG, ERB]]

  implicit def orOnLeftGoodOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit ev: InnerConstraint[ELG, ERG]): InnerConstraint[Or[ELG, ELB], Good[ERG, ERB]] = new InnerConstraint[Or[ELG, ELB], Good[ERG, ERB]]

  implicit def goodOnLeftGoodOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit ev: InnerConstraint[ELG, ERG]): InnerConstraint[Good[ELG, ELB], Good[ERG, ERB]] = new InnerConstraint[Good[ELG, ELB], Good[ERG, ERB]]

  implicit def badOnLeftOrOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit ev: InnerConstraint[ELB, ERB]): InnerConstraint[Bad[ELG, ELB], Or[ERG, ERB]] = new InnerConstraint[Bad[ELG, ELB], Or[ERG, ERB]]

  implicit def orOnLeftBadOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit ev: InnerConstraint[ELB, ERB]): InnerConstraint[Or[ELG, ELB], Bad[ERG, ERB]] = new InnerConstraint[Or[ELG, ELB], Bad[ERG, ERB]]

  implicit def badOnLeftBadOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit ev: InnerConstraint[ELB, ERB]): InnerConstraint[Bad[ELG, ELB], Bad[ERG, ERB]] = new InnerConstraint[Bad[ELG, ELB], Bad[ERG, ERB]]

  // Either (in x === y, x is the "target" of the === invocation, y is the "parameter")
  // ETL Element Target Left
  // ETR Element Target Right
  // EPL Element Parameter Left
  // EPR Element Parameter Right
  // This one will provide an equality constraint if the Left types have an inner constraint. It doesn't matter
  // in this case what the Right type does. If there isn't one for the Left type, the lower priority implicit method
  // LowPriorityConstraints.lowPriorityEitherEqualityConstraint will be checked will see
  // If there's an InnerConstraint for the Bad types.
  implicit def eitherEqualityConstraint[ETL, ETR, EPL, EPR](implicit ev: InnerConstraint[ETL, EPL]): InnerConstraint[Either[ETL, ETR], Either[EPL, EPR]] = new InnerConstraint[Either[ETL, ETR], Either[EPL, EPR]]

  implicit def leftOnParamSideEitherOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit ev: InnerConstraint[ETL, EPL]): InnerConstraint[Left[ETL, ETR], Either[EPL, EPR]] = new InnerConstraint[Left[ETL, ETR], Either[EPL, EPR]]

  implicit def eitherOnParamSideLeftOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit ev: InnerConstraint[ETL, EPL]): InnerConstraint[Either[ETL, ETR], Left[EPL, EPR]] = new InnerConstraint[Either[ETL, ETR], Left[EPL, EPR]]

  implicit def leftOnParamSideLeftOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit ev: InnerConstraint[ETL, EPL]): InnerConstraint[Left[ETL, ETR], Left[EPL, EPR]] = new InnerConstraint[Left[ETL, ETR], Left[EPL, EPR]]

  implicit def rightOnParamSideEitherOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit ev: InnerConstraint[ETR, EPR]): InnerConstraint[Right[ETL, ETR], Either[EPL, EPR]] = new InnerConstraint[Right[ETL, ETR], Either[EPL, EPR]]

  implicit def eitherOnParamSideRightOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit ev: InnerConstraint[ETR, EPR]): InnerConstraint[Either[ETL, ETR], Right[EPL, EPR]] = new InnerConstraint[Either[ETL, ETR], Right[EPL, EPR]]

  implicit def rightOnParamSideRightOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit ev: InnerConstraint[ETR, EPR]): InnerConstraint[Right[ETL, ETR], Right[EPL, EPR]] = new InnerConstraint[Right[ETL, ETR], Right[EPL, EPR]]
}
