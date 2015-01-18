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
package org.scalactic.algebra

import scala.language.higherKinds

/**
 * Trait represents a monoid: a category of one type with an associative binary operation 
 * and a zero element, i.e. an identity element.
 *
 */
trait Monoid[A] extends Associative[A] {
    
    /**
     * The zero element is the identity element of the op and obeys left and right identity:
     *
     * <pre>
     *  op(z, monoidA) == monoidA
     *  op(monoidA, z) == monoidA
     * </pre>
     */
    def z: A

}
