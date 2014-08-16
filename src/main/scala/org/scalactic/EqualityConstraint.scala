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
import scala.util.{Try,Success,Failure}

/**
 * Abstract class used to enforce type constraints for equality checks.
 *
 * <p>
 * For more information on how this class is used, see the documentation of <a href="EqualityPolicy.html"><code>EqualityPolicy</code></a>.
 * </p>
 */
@implicitNotFound(msg = "types ${A} and ${B} do not adhere to the type constraint selected for the === and !== operators; the missing implicit parameter is of type org.scalactic.EqualityConstraint[${A},${B}]")
abstract class EqualityConstraint[A, B] { thisConstraint =>

  /**
   * Indicates whether the objects passed as <code>a</code> and <code>b</code> are equal.
   *
   * @param a a left-hand-side object being compared with another (right-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
   * @param b a right-hand-side object being compared with another (left-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
   */
  def areEqual(a: A, b: B): Boolean
}

object EqualityConstraint extends LowPriorityEqualityConstraints {

  import EqualityPolicy.BasicEqualityConstraint

  implicit def seqEqualityConstraint[EA, CA[ea] <: collection.GenSeq[ea], EB, CB[eb] <: collection.GenSeq[eb]](implicit equalityOfA: Equality[CA[EA]], ev: EqualityConstraint[EA, EB] with Cooperative): EqualityConstraint[CA[EA], CB[EB]] with Cooperative = new BasicEqualityConstraint[CA[EA], CB[EB]](equalityOfA)

  implicit def arrayOnLeftEqualityConstraint[EA, CA[ea] <: Array[ea], EB, CB[eb] <: collection.GenSeq[eb]](implicit equalityOfA: Equality[CA[EA]], ev: EqualityConstraint[EA, EB] with Cooperative): EqualityConstraint[CA[EA], CB[EB]] with Cooperative = new BasicEqualityConstraint[CA[EA], CB[EB]](equalityOfA)

  implicit def arrayOnRightEqualityConstraint[EA, CA[ea] <: collection.GenSeq[ea], EB, CB[eb] <: Array[eb]](implicit equalityOfA: Equality[CA[EA]], ev: EqualityConstraint[EA, EB] with Cooperative): EqualityConstraint[CA[EA], CB[EB]] with Cooperative = new BasicEqualityConstraint[CA[EA], CB[EB]](equalityOfA)

  implicit def arrayOnBothSidesConstraint[EA, EB](implicit equalityOfA: Equality[Array[EA]], ev: EqualityConstraint[EA, EB] with Cooperative): EqualityConstraint[Array[EA], Array[EB]] with Cooperative = new BasicEqualityConstraint[Array[EA], Array[EB]](equalityOfA)

  implicit def setEqualityConstraint[EA, CA[ea] <: collection.GenSet[ea], EB, CB[eb] <: collection.GenSet[eb]](implicit equalityOfA: Equality[CA[EA]], ev: EqualityConstraint[EA, EB] with Cooperative): EqualityConstraint[CA[EA], CB[EB]] with Cooperative = new BasicEqualityConstraint[CA[EA], CB[EB]](equalityOfA)

  implicit def mapEqualityConstraint[KA, VA, CA[ka, kb] <: collection.GenMap[ka, kb], KB, VB, CB[kb, vb] <: collection.GenMap[kb, vb]](implicit equalityOfA: Equality[CA[KA, VA]], evKey: EqualityConstraint[KA, KB] with Cooperative, evValue: EqualityConstraint[VA, VB] with Cooperative): EqualityConstraint[CA[KA, VA], CB[KB, VB]] with Cooperative = new BasicEqualityConstraint[CA[KA, VA], CB[KB, VB]](equalityOfA)

  implicit def numericEqualityConstraint[A, B](implicit equalityOfA: Equality[A], numA: CooperatingNumeric[A], numB: CooperatingNumeric[B]): EqualityConstraint[A, B] with Cooperative = new BasicEqualityConstraint[A, B](equalityOfA)

  // 1. Every on left, can by subclass of Every on right
  // 2. Every on right, can be subclass of Every on left
  // 3. One on left, can be One or Every on right, but the latter will be provided by number 2
  // 4. One on right, can be One or Every on left, but the latter will be provided by number 1
  // 5. Many on left, can be Many or Every on right, but the latter will be provided by number 2
  // 6. Many on right, can be Many or Every on left, but the latter will be provided by number 1
  implicit def everyOnLeftEqualityConstraint[EA, EB, CB[eb] <: Every[eb]](implicit equalityOfA: Equality[Every[EA]], ev: EqualityConstraint[EA, EB] with Cooperative): EqualityConstraint[Every[EA], CB[EB]] with Cooperative = new BasicEqualityConstraint[Every[EA], CB[EB]](equalityOfA)

  implicit def oneOnBothSidesEqualityConstraint[EA, EB](implicit equalityOfA: Equality[One[EA]], ev: EqualityConstraint[EA, EB] with Cooperative): EqualityConstraint[One[EA], One[EB]] with Cooperative = new BasicEqualityConstraint[One[EA], One[EB]](equalityOfA)

  implicit def manyOnBothSidesEqualityConstraint[EA, EB](implicit equalityOfA: Equality[Many[EA]], ev: EqualityConstraint[EA, EB] with Cooperative): EqualityConstraint[Many[EA], Many[EB]] with Cooperative = new BasicEqualityConstraint[Many[EA], Many[EB]](equalityOfA)

  // ELG Element Left Good
  // ELB Element Left Bad
  // ERG Element Right Good
  // ERB Element Right Bad
  // This one will provide an equality constraint if the Good types have an inner constraint. It doesn't matter
  // in this case what the Bad type does. If there isn't one for the Good type, the lower priority implicit method
  // LowPriorityConstraints.lowPriorityOrEqualityConstraint will be checked will see
  // If there's an EqualityConstraint for the Bad types.
  implicit def orEqualityConstraint[ELG, ELB, ERG, ERB](implicit equalityOfL: Equality[Or[ELG, ELB]], ev: EqualityConstraint[ELG, ERG] with Cooperative): EqualityConstraint[Or[ELG, ELB], Or[ERG, ERB]] with Cooperative = new BasicEqualityConstraint[Or[ELG, ELB], Or[ERG, ERB]](equalityOfL)

  implicit def orOnBothSidesWithBadNothingConstraint[ELG, ERG](implicit equalityOfL: Equality[Or[ELG, Nothing]], ev: EqualityConstraint[ELG, ERG] with Cooperative): EqualityConstraint[Or[ELG, Nothing], Or[ERG, Nothing]] with Cooperative = new BasicEqualityConstraint[Or[ELG, Nothing], Or[ERG, Nothing]](equalityOfL)

  implicit def goodOnLeftOrOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit equalityOfL: Equality[Good[ELG, ELB]], ev: EqualityConstraint[ELG, ERG] with Cooperative): EqualityConstraint[Good[ELG, ELB], Or[ERG, ERB]] with Cooperative = new BasicEqualityConstraint[Good[ELG, ELB], Or[ERG, ERB]](equalityOfL)

  implicit def goodOnLeftOrOnRightNothingConstraint[ELG, ERG](implicit equalityOfL: Equality[Good[ELG, Nothing]], ev: EqualityConstraint[ELG, ERG] with Cooperative): EqualityConstraint[Good[ELG, Nothing], Or[ERG, Nothing]] with Cooperative = new BasicEqualityConstraint[Good[ELG, Nothing], Or[ERG, Nothing]](equalityOfL)

  implicit def orOnLeftGoodOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit equalityOfL: Equality[Or[ELG, ELB]], ev: EqualityConstraint[ELG, ERG] with Cooperative): EqualityConstraint[Or[ELG, ELB], Good[ERG, ERB]] with Cooperative = new BasicEqualityConstraint[Or[ELG, ELB], Good[ERG, ERB]](equalityOfL)

  implicit def orOnLeftGoodOnRightNothingConstraint[ELG, ERG](implicit equalityOfL: Equality[Or[ELG, Nothing]], ev: EqualityConstraint[ELG, ERG] with Cooperative): EqualityConstraint[Or[ELG, Nothing], Good[ERG, Nothing]] with Cooperative = new BasicEqualityConstraint[Or[ELG, Nothing], Good[ERG, Nothing]](equalityOfL)

  implicit def goodOnLeftGoodOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit equalityOfL: Equality[Good[ELG, ELB]], ev: EqualityConstraint[ELG, ERG] with Cooperative): EqualityConstraint[Good[ELG, ELB], Good[ERG, ERB]] with Cooperative = new BasicEqualityConstraint[Good[ELG, ELB], Good[ERG, ERB]](equalityOfL)

  implicit def goodOnLeftGoodOnRightNothingConstraint[ELG, ERG](implicit equalityOfL: Equality[Good[ELG, Nothing]], ev: EqualityConstraint[ELG, ERG] with Cooperative): EqualityConstraint[Good[ELG, Nothing], Good[ERG, Nothing]] with Cooperative = new BasicEqualityConstraint[Good[ELG, Nothing], Good[ERG, Nothing]](equalityOfL)

  implicit def badOnLeftOrOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit equalityOfL: Equality[Bad[ELG, ELB]], ev: EqualityConstraint[ELB, ERB] with Cooperative): EqualityConstraint[Bad[ELG, ELB], Or[ERG, ERB]] with Cooperative = new BasicEqualityConstraint[Bad[ELG, ELB], Or[ERG, ERB]](equalityOfL)

  implicit def badOnLeftOrOnRightNothingConstraint[ELB, ERB](implicit equalityOfL: Equality[Bad[Nothing, ELB]], ev: EqualityConstraint[ELB, ERB] with Cooperative): EqualityConstraint[Bad[Nothing, ELB], Or[Nothing, ERB]] with Cooperative = new BasicEqualityConstraint[Bad[Nothing, ELB], Or[Nothing, ERB]](equalityOfL)

  implicit def orOnLeftBadOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit equalityOfL: Equality[Or[ELG, ELB]], ev: EqualityConstraint[ELB, ERB] with Cooperative): EqualityConstraint[Or[ELG, ELB], Bad[ERG, ERB]] with Cooperative = new BasicEqualityConstraint[Or[ELG, ELB], Bad[ERG, ERB]](equalityOfL)

  implicit def orOnLeftBadOnRightNothingConstraint[ELB, ERB](implicit equalityOfL: Equality[Or[Nothing, ELB]], ev: EqualityConstraint[ELB, ERB] with Cooperative): EqualityConstraint[Or[Nothing, ELB], Bad[Nothing, ERB]] with Cooperative = new BasicEqualityConstraint[Or[Nothing, ELB], Bad[Nothing, ERB]](equalityOfL)

  implicit def badOnLeftBadOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit equalityOfL: Equality[Bad[ELG, ELB]], ev: EqualityConstraint[ELB, ERB] with Cooperative): EqualityConstraint[Bad[ELG, ELB], Bad[ERG, ERB]] with Cooperative = new BasicEqualityConstraint[Bad[ELG, ELB], Bad[ERG, ERB]](equalityOfL)

  implicit def badOnLeftBadOnRightNothingConstraint[ELB, ERB](implicit equalityOfL: Equality[Bad[Nothing, ELB]], ev: EqualityConstraint[ELB, ERB] with Cooperative): EqualityConstraint[Bad[Nothing, ELB], Bad[Nothing, ERB]] with Cooperative = new BasicEqualityConstraint[Bad[Nothing, ELB], Bad[Nothing, ERB]](equalityOfL)

  // Either (in x === y, x is the "target" of the === invocation, y is the "parameter")
  // ETL Element Target Left
  // ETR Element Target Right
  // EPL Element Parameter Left
  // EPR Element Parameter Right
  // This one will provide an equality constraint if the Left types have an inner constraint. It doesn't matter
  // in this case what the Right type does. If there isn't one for the Left type, the lower priority implicit method
  // LowPriorityConstraints.lowPriorityEitherEqualityConstraint will be checked will see
  // If there's an EqualityConstraint for the Bad types.
  implicit def eitherEqualityConstraint[ETL, ETR, EPL, EPR](implicit equalityOfT: Equality[Either[ETL, ETR]], ev: EqualityConstraint[ETL, EPL] with Cooperative): EqualityConstraint[Either[ETL, ETR], Either[EPL, EPR]] with Cooperative = new BasicEqualityConstraint[Either[ETL, ETR], Either[EPL, EPR]](equalityOfT)
  implicit def eitherNothingConstraint[ETL, EPL](implicit equalityOfT: Equality[Either[ETL, Nothing]], ev: EqualityConstraint[ETL, EPL] with Cooperative): EqualityConstraint[Either[ETL, Nothing], Either[EPL, Nothing]] with Cooperative = new BasicEqualityConstraint[Either[ETL, Nothing], Either[EPL, Nothing]](equalityOfT)

  implicit def leftOnParamSideEitherOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit equalityOfT: Equality[Left[ETL, ETR]], ev: EqualityConstraint[ETL, EPL] with Cooperative): EqualityConstraint[Left[ETL, ETR], Either[EPL, EPR]] with Cooperative = new BasicEqualityConstraint[Left[ETL, ETR], Either[EPL, EPR]](equalityOfT)
  implicit def leftOnParamSideEitherOnTargetSideNothingConstraint[ETL, EPL](implicit equalityOfT: Equality[Left[ETL, Nothing]], ev: EqualityConstraint[ETL, EPL] with Cooperative): EqualityConstraint[Left[ETL, Nothing], Either[EPL, Nothing]] with Cooperative = new BasicEqualityConstraint[Left[ETL, Nothing], Either[EPL, Nothing]](equalityOfT)

  implicit def eitherOnParamSideLeftOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit equalityOfT: Equality[Either[ETL, ETR]], ev: EqualityConstraint[ETL, EPL] with Cooperative): EqualityConstraint[Either[ETL, ETR], Left[EPL, EPR]] with Cooperative = new BasicEqualityConstraint[Either[ETL, ETR], Left[EPL, EPR]](equalityOfT)
  implicit def eitherOnParamSideLeftOnTargetSideNothingConstraint[ETL, EPL](implicit equalityOfT: Equality[Either[ETL, Nothing]], ev: EqualityConstraint[ETL, EPL] with Cooperative): EqualityConstraint[Either[ETL, Nothing], Left[EPL, Nothing]] with Cooperative = new BasicEqualityConstraint[Either[ETL, Nothing], Left[EPL, Nothing]](equalityOfT)

  implicit def leftOnParamSideLeftOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit equalityOfT: Equality[Left[ETL, ETR]], ev: EqualityConstraint[ETL, EPL] with Cooperative): EqualityConstraint[Left[ETL, ETR], Left[EPL, EPR]] with Cooperative = new BasicEqualityConstraint[Left[ETL, ETR], Left[EPL, EPR]](equalityOfT)
  implicit def leftOnParamSideLeftOnTargetSideNothingConstraint[ETL, EPL](implicit equalityOfT: Equality[Left[ETL, Nothing]], ev: EqualityConstraint[ETL, EPL] with Cooperative): EqualityConstraint[Left[ETL, Nothing], Left[EPL, Nothing]] with Cooperative = new BasicEqualityConstraint[Left[ETL, Nothing], Left[EPL, Nothing]](equalityOfT)

  implicit def rightOnParamSideEitherOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit equalityOfT: Equality[Right[ETL, ETR]], ev: EqualityConstraint[ETR, EPR] with Cooperative): EqualityConstraint[Right[ETL, ETR], Either[EPL, EPR]] with Cooperative = new BasicEqualityConstraint[Right[ETL, ETR], Either[EPL, EPR]](equalityOfT)
  implicit def rightOnParamSideEitherOnTargetSideNothingConstraint[ETR, EPR](implicit equalityOfT: Equality[Right[Nothing, ETR]], ev: EqualityConstraint[ETR, EPR] with Cooperative): EqualityConstraint[Right[Nothing, ETR], Either[Nothing, EPR]] with Cooperative = new BasicEqualityConstraint[Right[Nothing, ETR], Either[Nothing, EPR]](equalityOfT)

  implicit def eitherOnParamSideRightOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit equalityOfT: Equality[Either[ETL, ETR]], ev: EqualityConstraint[ETR, EPR] with Cooperative): EqualityConstraint[Either[ETL, ETR], Right[EPL, EPR]] with Cooperative = new BasicEqualityConstraint[Either[ETL, ETR], Right[EPL, EPR]](equalityOfT)
  implicit def eitherOnParamSideRightOnTargetSideNothingConstraint[ETR, EPR](implicit equalityOfT: Equality[Either[Nothing, ETR]], ev: EqualityConstraint[ETR, EPR] with Cooperative): EqualityConstraint[Either[Nothing, ETR], Right[Nothing, EPR]] with Cooperative = new BasicEqualityConstraint[Either[Nothing, ETR], Right[Nothing, EPR]](equalityOfT)

  implicit def rightOnParamSideRightOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit equalityOfT: Equality[Right[ETL, ETR]], ev: EqualityConstraint[ETR, EPR] with Cooperative): EqualityConstraint[Right[ETL, ETR], Right[EPL, EPR]] with Cooperative = new BasicEqualityConstraint[Right[ETL, ETR], Right[EPL, EPR]](equalityOfT)
  implicit def rightOnParamSideRightOnTargetSideNothingConstraint[ETR, EPR](implicit equalityOfT: Equality[Right[Nothing, ETR]], ev: EqualityConstraint[ETR, EPR] with Cooperative): EqualityConstraint[Right[Nothing, ETR], Right[Nothing, EPR]] with Cooperative = new BasicEqualityConstraint[Right[Nothing, ETR], Right[Nothing, EPR]](equalityOfT)

  // 1. Option on left, can by subclass of Option on right
  // 2. Option on right, can be subclass of Option on left
  // 3. None on left, can be None or Option on right, but the latter will be provided by number 2
  // 4. None on right, can be None or Option on left, but the latter will be provided by number 1
  // 5. Some on left, can be Some or Option on right, but the latter will be provided by number 2
  // 6. Some on right, can be Some or Option on left, but the latter will be provided by number 1
  implicit def optionOnLeftEqualityConstraint[EA, EB, CB[eb] <: Option[eb]](implicit equalityOfA: Equality[Option[EA]], ev: EqualityConstraint[EA, EB] with Cooperative): EqualityConstraint[Option[EA], CB[EB]] with Cooperative = new BasicEqualityConstraint[Option[EA], CB[EB]](equalityOfA)

  implicit def someOnBothSidesEqualityConstraint[EA, EB](implicit equalityOfA: Equality[Some[EA]], ev: EqualityConstraint[EA, EB] with Cooperative): EqualityConstraint[Some[EA], Some[EB]] with Cooperative = new BasicEqualityConstraint[Some[EA], Some[EB]](equalityOfA)
  implicit def optionOnBothSidesEqualityConstraint[EA, EB](implicit equalityOfA: Equality[Option[EA]], ev: EqualityConstraint[EA, EB] with Cooperative): EqualityConstraint[Option[EA], Option[EB]] with Cooperative = new BasicEqualityConstraint[Option[EA], Option[EB]](equalityOfA)

  // 1. Try on left, can by subclass of Try on right
  // 2. Try on right, can be subclass of Try on left
  // 3. Success on left, can be Success or Try on right, but the latter will be provided by number 2
  // 4. Success on right, can be Success or Try on left, but the latter will be provided by number 1
  // 5. Failure on left, can be Failure or Try on right, but the latter will be provided by number 2
  // 6. Failure on right, can be Failure or Try on left, but the latter will be provided by number 1
  implicit def tryOnLeftEqualityConstraint[EA, EB, CB[eb] <: Try[eb]](implicit equalityOfA: Equality[Try[EA]], ev: EqualityConstraint[EA, EB] with Cooperative): EqualityConstraint[Try[EA], CB[EB]] with Cooperative = new BasicEqualityConstraint[Try[EA], CB[EB]](equalityOfA)

  implicit def successOnBothSidesEqualityConstraint[EA, EB](implicit equalityOfA: Equality[Success[EA]], ev: EqualityConstraint[EA, EB] with Cooperative): EqualityConstraint[Success[EA], Success[EB]] with Cooperative = new BasicEqualityConstraint[Success[EA], Success[EB]](equalityOfA)

  implicit def failureOnBothSidesEqualityConstraint[EA, EB](implicit equalityOfA: Equality[Failure[EA]], ev: EqualityConstraint[EA, EB] with Cooperative): EqualityConstraint[Failure[EA], Failure[EB]] with Cooperative = new BasicEqualityConstraint[Failure[EA], Failure[EB]](equalityOfA)

}
