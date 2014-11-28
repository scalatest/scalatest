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

import org.scalatest._

/*
// trait SeqConstraints
// Need a low priority one
implicit def seqEqualityConstraint[EA, CA[_] <: collection.GenSeq[_], EB, CB[_] <: collection.GenSeq[_]](implicit equalityOfA: Equality[CA[EA]], ev: EA <:< EB): EqualityConstraint[CA[EA], CB[EB]] = new BasicEqualityConstraint[CA[EA], CB[EB]](equalityOfA)

// trait SetConstraints
// Need a low priority one
implicit def setEqualityConstraint[EA, CA[_] <: collection.GenSet[_], EB, CB[_] <: collection.GenSet[_]](implicit equalityOfA: Equality[CA[EA]], ev: EA <:< EB): EqualityConstraint[CA[EA], CB[EB]] = new BasicEqualityConstraint[CA[EA], CB[EB]](equalityOfA)

// trait MapConstraints
// Need a low priority one for the value going in VB <:< VA direction
implicit def mapEqualityConstraint[KA, VA, CA[_, _] <: collection.GenMap[_, _], KB, VB, CB[_, _] <: collection.GenMap[_, _]](implicit equalityOfA: Equality[CA[KA, VA]], kev: KA <:< KB, vev: VA <:< VB): EqualityConstraint[CA[KA, VA], CB[KB, VB]] = new BasicEqualityConstraint[CA[KA, VA], CB[KB, VB]](equalityOfA)

// trait ArrayConstraints
// Need a low priority one for the value going in VB <:< VA direction
implicit def seqArrayEqualityConstraint[EA, CA[_] <: collection.GenSeq[_], EB, CB[_] <: Array[_]](implicit equalityOfA: Equality[CA[EA]], ev: EA <:< EB): EqualityConstraint[CA[EA], CB[EB]] = new BasicEqualityConstraint[CA[EA], CB[EB]](equalityOfA)
// Need a low priority one for the value going in VB <:< VA direction
implicit def arraySeqEqualityConstraint[EA, CA[_] <: Array[_], EB, CB[_] <: collection.GenSeq[_]](implicit equalityOfA: Equality[CA[EA]], ev: EA <:< EB): EqualityConstraint[CA[EA], CB[EB]] = new BasicEqualityConstraint[CA[EA], CB[EB]](equalityOfA)
// Need a low priority one for the value going in VB <:< VA direction
implicit def arrayEqualityConstraint[EA, CA[_] <: Array[_], EB, CB[_] <: Array[_]](implicit equalityOfA: Equality[CA[EA]], ev: EA <:< EB): EqualityConstraint[CA[EA], CB[EB]] = new BasicEqualityConstraint[CA[EA], CB[EB]](equalityOfA)

// These are not in TypeCheckedTripleEquals or ConversionCheckedTripleEquals. Just available in addition.

trait SeqConstraints
trait SetConstraints
trait MapConstraints
trait TraversableConstraints extends SeqConstraints with SetConstraints with MapConstraints
trait ArrayConstraints
trait CollectionConstraints extends TraversableConstraints with ArrayConstraints

// They are all "equality" constraints, so maybe EqualityConstraints should be called something else
// like GeneralContraints.

*/
class TraversableConstraintsSpec extends Spec with NonImplicitAssertions with TypeCheckedTripleEquals {
}
