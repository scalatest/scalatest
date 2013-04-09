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

trait MapEqualityConstraints {
  implicit def mapEqualityConstraint[EAK, EAV, CA[_, _] <: collection.GenMap[_, _], EBK, EBV, CB[_, _] <: collection.GenMap[_, _]](implicit equalityOfA: Equality[CA[EAK, EAV]], evKey: EqualityConstraint[EAK, EBK], evValue: EqualityConstraint[EAV, EBV]): EqualityConstraint[CA[EAK, EAV], CB[EBK, EBV]] = new BasicEqualityConstraint[CA[EAK, EAV], CB[EBK, EBV]](equalityOfA)
}

object MapEqualityConstraints extends MapEqualityConstraints
