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
import scala.language.implicitConversions
import InnerConstraint.Open

class InnerConstraint[A, B]

// Placing the numeric and the subtype/supertype in separate traits because
// they all work off of A, B, the same level of specificity. So my theory is
// that they will clash if in the same level (like 1 and 1 would work for all three). 
// Putting these as the lowest priority because they are the least specific. If
// put them at the bottom, their points for being higher priority may even out
// points another more specific one has for being more specific.
trait NumericInnerConstraint {
  implicit def numericEqualityInnerConstraint[A, B](implicit open: Open[A], numA: CooperatingNumeric[A], numB: CooperatingNumeric[B]): InnerConstraint[A, B] = new InnerConstraint[A, B]
}
trait LeftTypeIsSubtypeOfRightTypeInnerConstraint extends NumericInnerConstraint {
  implicit def lowPriorityTypeCheckedInnerConstraint[A, B](implicit open: Open[B], ev: A <:< B): InnerConstraint[A, B] = new InnerConstraint[A, B]
}
trait RightTypeIsSubtypeOfLeftTypeInnerConstraint extends LeftTypeIsSubtypeOfRightTypeInnerConstraint {
  implicit def typeCheckedInnerConstraint[A, B](implicit open: Open[A], ev: B <:< A): InnerConstraint[A, B] = new InnerConstraint[A, B]
}
//
// Going to need to deal with Array more specially at the nested level. Would need to take the Array
// Equality for the nested one. I think I could do this in general: have special implicits when the
// contained type is Array, for any and all containers. I think that would fix List[Array[T]] too.
//
trait LowPriorityInnerConstraints1 extends RightTypeIsSubtypeOfLeftTypeInnerConstraint {

  // ELG Element Left Good
  // ELB Element Left Bad
  // ORL Or Left
  // ERG Element Right Good
  // ERB Element Right Bad
  // ORR Or Right
  // This one will provide an equality constraint if the Bad types have an inner constraint. It doesn't matter
  // in this case what the Good type does. If there was a constraint available for the Good types, then it would
  // use the higher priority implicit Constraint.orEqualityConstraint and never get here. 
  implicit def lowPriorityOrEqualityConstraint[ELG, ELB, ERG, ERB](implicit open: Open[Or[ELG, ELB]], ev: InnerConstraint[ELB, ERB]): InnerConstraint[Or[ELG, ELB], Or[ERG, ERB]] = new InnerConstraint[Or[ELG, ELB], Or[ERG, ERB]]

  implicit def lowPriorityOrOnBothSidesWithGoodNothingConstraint[ELB, ERB](implicit open: Open[Or[Nothing, ELB]], ev: InnerConstraint[ELB, ERB]): InnerConstraint[Or[Nothing, ELB], Or[Nothing, ERB]] = new InnerConstraint[Or[Nothing, ELB], Or[Nothing, ERB]]

  // This must be low priority to allow Every on both sides
  implicit def everyOnRightEqualityConstraint[EA, CA[ea] <: Every[ea], EB](implicit open: Open[CA[EA]], ev: InnerConstraint[EA, EB]): InnerConstraint[CA[EA], Every[EB]] = new InnerConstraint[CA[EA], Every[EB]]

  // Either (in x === y, x is the "target" of the === invocation, y is the "parameter")
  // ETL Element Target Left
  // ETR Element Target Right
  // EPL Element Parameter Left
  // EPR Element Parameter Right
  // This one will provide an equality constraint if the Bad types have an inner constraint. It doesn't matter
  // in this case what the Good type does. If there was a constraint available for the Good types, then it would
  // use the higher priority implicit Constraint.orEqualityConstraint and never get here. 
  implicit def lowPriorityEitherEqualityConstraint[ETL, ETR, EPL, EPR](implicit open: Open[Either[ETL, ETR]], ev: InnerConstraint[ETR, EPR]): InnerConstraint[Either[ETL, ETR], Either[EPL, EPR]] = new InnerConstraint[Either[ETL, ETR], Either[EPL, EPR]]
}
object InnerConstraint extends LowPriorityInnerConstraints1 {
  implicit def seqEqualityConstraint[EA, CA[ea] <: collection.GenSeq[ea], EB, CB[eb] <: collection.GenSeq[eb]](implicit open: Open[CA[EA]], ev: InnerConstraint[EA, EB]): InnerConstraint[CA[EA], CB[EB]] = new InnerConstraint[CA[EA], CB[EB]]

  implicit def setEqualityConstraint[EA, CA[ea] <: collection.GenSet[ea], EB, CB[eb] <: collection.GenSet[eb]](implicit open: Open[CA[EA]], ev: InnerConstraint[EA, EB]): InnerConstraint[CA[EA], CB[EB]] = new InnerConstraint[CA[EA], CB[EB]]

  implicit def mapEqualityConstraint[KA, VA, CA[ka, kb] <: collection.GenMap[ka, kb], KB, VB, CB[kb, vb] <: collection.GenMap[kb, vb]](implicit open: Open[CA[KA, VA]], evKey: InnerConstraint[KA, KB], evValue: InnerConstraint[VA, VB]): InnerConstraint[CA[KA, VA], CB[KB, VB]] = new InnerConstraint[CA[KA, VA], CB[KB, VB]]

  // 1. Every on left, can by subclass of Every on right
  // 2. Every on right, can be subclass of Every on left
  // 3. One on left, can be One or Every on right, but the latter will be provided by number 2
  // 4. One on right, can be One or Every on left, but the latter will be provided by number 1
  // 5. Many on left, can be Many or Every on right, but the latter will be provided by number 2
  // 6. Many on right, can be Many or Every on left, but the latter will be provided by number 1
  implicit def everyOnLeftEqualityConstraint[EA, EB, CB[eb] <: Every[eb]](implicit open: Open[Every[EA]], ev: InnerConstraint[EA, EB]): InnerConstraint[Every[EA], CB[EB]] = new InnerConstraint[Every[EA], CB[EB]]

  implicit def oneOnBothSidesqualityConstraint[EA, EB](implicit open: Open[One[EA]], ev: InnerConstraint[EA, EB]): InnerConstraint[One[EA], One[EB]] = new InnerConstraint[One[EA], One[EB]]

  implicit def manyOnBothSidesEqualityConstraint[EA, EB](implicit open: Open[Many[EA]], ev: InnerConstraint[EA, EB]): InnerConstraint[Many[EA], Many[EB]] = new InnerConstraint[Many[EA], Many[EB]]

  // ELG Element Left Good
  // ELB Element Left Bad
  // ERG Element Right Good
  // ERB Element Right Bad
  // This one will provide an equality constraint if the Good types have an inner constraint. It doesn't matter
  // in this case what the Bad type does. If there isn't one for the Good type, the lower priority implicit method
  // LowPriorityConstraints.lowPriorityOrEqualityConstraint will be checked will see
  // If there's an InnerConstraint for the Bad types.
  implicit def orEqualityConstraint[ELG, ELB, ERG, ERB](implicit open: Open[Or[ELG, ELB]], ev: InnerConstraint[ELG, ERG]): InnerConstraint[Or[ELG, ELB], Or[ERG, ERB]] = new InnerConstraint[Or[ELG, ELB], Or[ERG, ERB]]
  implicit def orOnBothSidesWithBadNothingConstraint[ELG, ERG](implicit open: Open[Or[ELG, Nothing]], ev: InnerConstraint[ELG, ERG]): InnerConstraint[Or[ELG, Nothing], Or[ERG, Nothing]] = new InnerConstraint[Or[ELG, Nothing], Or[ERG, Nothing]]

  implicit def goodOnLeftOrOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit open: Open[Good[ELG, ELB]], ev: InnerConstraint[ELG, ERG]): InnerConstraint[Good[ELG, ELB], Or[ERG, ERB]] = new InnerConstraint[Good[ELG, ELB], Or[ERG, ERB]]
  implicit def goodOnLeftOrOnRightNothingConstraint[ELG, ERG](implicit open: Open[Good[ELG, Nothing]], ev: InnerConstraint[ELG, ERG]): InnerConstraint[Good[ELG, Nothing], Or[ERG, Nothing]] = new InnerConstraint[Good[ELG, Nothing], Or[ERG, Nothing]]

  implicit def orOnLeftGoodOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit open: Open[Or[ELG, ELB]], ev: InnerConstraint[ELG, ERG]): InnerConstraint[Or[ELG, ELB], Good[ERG, ERB]] = new InnerConstraint[Or[ELG, ELB], Good[ERG, ERB]]
  implicit def orOnLeftGoodOnRightNothingConstraint[ELG, ERG](implicit open: Open[Or[ELG, Nothing]], ev: InnerConstraint[ELG, ERG]): InnerConstraint[Or[ELG, Nothing], Good[ERG, Nothing]] = new InnerConstraint[Or[ELG, Nothing], Good[ERG, Nothing]]

  implicit def goodOnLeftGoodOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit open: Open[Good[ELG, ELB]], ev: InnerConstraint[ELG, ERG]): InnerConstraint[Good[ELG, ELB], Good[ERG, ERB]] = new InnerConstraint[Good[ELG, ELB], Good[ERG, ERB]]
  implicit def goodOnLeftGoodOnRightNothingConstraint[ELG, ERG](implicit open: Open[Good[ELG, Nothing]], ev: InnerConstraint[ELG, ERG]): InnerConstraint[Good[ELG, Nothing], Good[ERG, Nothing]] = new InnerConstraint[Good[ELG, Nothing], Good[ERG, Nothing]]

  implicit def badOnLeftOrOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit open: Open[Bad[ELG, ELB]], ev: InnerConstraint[ELB, ERB]): InnerConstraint[Bad[ELG, ELB], Or[ERG, ERB]] = new InnerConstraint[Bad[ELG, ELB], Or[ERG, ERB]]
  implicit def badOnLeftOrOnRightNothingConstraint[ELB, ERB](implicit open: Open[Bad[Nothing, ELB]], ev: InnerConstraint[ELB, ERB]): InnerConstraint[Bad[Nothing, ELB], Or[Nothing, ERB]] = new InnerConstraint[Bad[Nothing, ELB], Or[Nothing, ERB]]

  implicit def orOnLeftBadOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit open: Open[Or[ELG, ELB]], ev: InnerConstraint[ELB, ERB]): InnerConstraint[Or[ELG, ELB], Bad[ERG, ERB]] = new InnerConstraint[Or[ELG, ELB], Bad[ERG, ERB]]
  implicit def orOnLeftBadOnRightNothingConstraint[ELB, ERB](implicit open: Open[Or[Nothing, ELB]], ev: InnerConstraint[ELB, ERB]): InnerConstraint[Or[Nothing, ELB], Bad[Nothing, ERB]] = new InnerConstraint[Or[Nothing, ELB], Bad[Nothing, ERB]]

  implicit def badOnLeftBadOnRightEqualityConstraint[ELG, ELB, ERG, ERB](implicit open: Open[Bad[ELG, ELB]], ev: InnerConstraint[ELB, ERB]): InnerConstraint[Bad[ELG, ELB], Bad[ERG, ERB]] = new InnerConstraint[Bad[ELG, ELB], Bad[ERG, ERB]]
  implicit def badOnLeftBadOnRightNothingConstraint[ELB, ERB](implicit open: Open[Bad[Nothing, ELB]], ev: InnerConstraint[ELB, ERB]): InnerConstraint[Bad[Nothing, ELB], Bad[Nothing, ERB]] = new InnerConstraint[Bad[Nothing, ELB], Bad[Nothing, ERB]]

  // Either (in x === y, x is the "target" of the === invocation, y is the "parameter")
  // ETL Element Target Left
  // ETR Element Target Right
  // EPL Element Parameter Left
  // EPR Element Parameter Right
  // This one will provide an equality constraint if the Left types have an inner constraint. It doesn't matter
  // in this case what the Right type does. If there isn't one for the Left type, the lower priority implicit method
  // LowPriorityConstraints.lowPriorityEitherEqualityConstraint will be checked will see
  // If there's an InnerConstraint for the Bad types.
  implicit def eitherEqualityConstraint[ETL, ETR, EPL, EPR](implicit open: Open[Either[ETL, ETR]], ev: InnerConstraint[ETL, EPL]): InnerConstraint[Either[ETL, ETR], Either[EPL, EPR]] = new InnerConstraint[Either[ETL, ETR], Either[EPL, EPR]]

  implicit def leftOnParamSideEitherOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit open: Open[Left[ETL, ETR]], ev: InnerConstraint[ETL, EPL]): InnerConstraint[Left[ETL, ETR], Either[EPL, EPR]] = new InnerConstraint[Left[ETL, ETR], Either[EPL, EPR]]

  implicit def eitherOnParamSideLeftOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit open: Open[Either[ETL, ETR]], ev: InnerConstraint[ETL, EPL]): InnerConstraint[Either[ETL, ETR], Left[EPL, EPR]] = new InnerConstraint[Either[ETL, ETR], Left[EPL, EPR]]

  implicit def leftOnParamSideLeftOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit open: Open[Left[ETL, ETR]], ev: InnerConstraint[ETL, EPL]): InnerConstraint[Left[ETL, ETR], Left[EPL, EPR]] = new InnerConstraint[Left[ETL, ETR], Left[EPL, EPR]]

  implicit def rightOnParamSideEitherOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit open: Open[Right[ETL, ETR]], ev: InnerConstraint[ETR, EPR]): InnerConstraint[Right[ETL, ETR], Either[EPL, EPR]] = new InnerConstraint[Right[ETL, ETR], Either[EPL, EPR]]

  implicit def eitherOnParamSideRightOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit open: Open[Either[ETL, ETR]], ev: InnerConstraint[ETR, EPR]): InnerConstraint[Either[ETL, ETR], Right[EPL, EPR]] = new InnerConstraint[Either[ETL, ETR], Right[EPL, EPR]]

  implicit def rightOnParamSideRightOnTargetSideEqualityConstraint[ETL, ETR, EPL, EPR](implicit open: Open[Right[ETL, ETR]], ev: InnerConstraint[ETR, EPR]): InnerConstraint[Right[ETL, ETR], Right[EPL, EPR]] = new InnerConstraint[Right[ETL, ETR], Right[EPL, EPR]]

  sealed abstract class Open[T]
  object Open {
    class OpenImpl[T] extends Open[T] {
      override def toString = "Open instance"
    }
    implicit def open[T]: Open[T] = new OpenImpl[T]
  }
}

