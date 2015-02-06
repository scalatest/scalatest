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

/**
 * Abstract class used to enforce type constraints for equality checks.
 *
 * <p>
 * For more information on how this class is used, see the documentation of <a href="TripleEqualsSupport.html"><code>TripleEqualsSupport</code></a>.
 * </p>
 */
@implicitNotFound(msg = "types ${A} and ${B} do not adhere to the type constraint selected for the === and !== operators; the missing implicit parameter is of type org.scalactic.Constraint[${A},${B}]")
abstract class Constraint[A, B] {

  /**
   * Indicates whether the objects passed as <code>a</code> and <code>b</code> are equal.
   *
   * @param a a left-hand-side object being compared with another (right-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
   * @param b a right-hand-side object being compared with another (left-hand-side one) for equality (<em>e.g.</em>, <code>a == b</code>)
   */
  def areEqual(a: A, b: B): Boolean
}

