/*
 * Copyright 2001-2009 Artima, Inc.
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
package org.scalatest.words

import org.scalatest._

// Used to make an implicit conversion more specific. If there were ever another
// implicit (String, String, () => Unit) lying around in scope, it would clash with
// if the implicit was just based on that more general type. This is used in
// ShouldVerb, MustVerb, and CanVerb.
/**
 * Class that provides a role-specific type for an implicit conversion used to support
 * the registration of subject descriptions in <code>WordSpec</code> and <code>fixture.WordSpec</code>.
 *
 * <p>
 * For example, this class enables syntax such as the following in <code>WordSpec</code>
 * and <code>fixture.WordSpec</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * "A Stack (when empty)" should { ...
 *                        ^
 * </pre>
 *
 * This <code>should</code> method, which is provided in <code>ShouldVerb</code>, needs an implicit parameter
 * of type <code>(String, String, () => Unit) => Unit</code>. Because the required type has no ScalaTest-specific
 * types in it, it is possible that another implicit parameter of that same type could be in scope, which would
 * cause a compile failure. Requiring an implicit parameter of the more specific <code>StringVerbBlockRegistration</code>,
 * which simply extends the needed type, <code>(String, String, () => Unit) => Unit</code>, avoids this potential conflict.
 *
 * @author Bill Venners
 */
abstract class StringVerbBlockRegistration extends ((String, String, () => Unit) => Unit) {

  /**
   * Registers a subject description in <code>WordSpec</code> and <code>fixture.WordSpec</code>.
   *
   * <p>
   * For example, this class enables syntax such as the following in <code>WordSpec</code>
   * and <code>fixture.WordSpec</code>:
   * </p>
   *
   * <pre class="stHighlight">
   * "A Stack (when empty)" should { ...
   *                        ^
   * </pre>
   *
   */
  def apply(string: String, verb: String, block: () => Unit)
}

