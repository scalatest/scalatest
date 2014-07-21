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

//
// Going to need to deal with Array more specially at the nested level. Would need to take the Array
// Equality for the nested one. I think I could do this in general: have special implicits when the
// contained type is Array, for any and all containers. I think that would fix List[Array[T]] too.
//

object InnerConstraint extends LowPriorityInnerConstraints {
  import scala.language.implicitConversions
  implicit def typeCheckedInnerConstraint[A, B](implicit ev: B <:< A): InnerConstraint[A, B] = new InnerConstraint[A, B]
}

