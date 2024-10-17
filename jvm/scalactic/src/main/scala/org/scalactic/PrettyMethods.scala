/*
 * Copyright 2001-2024 Artima, Inc.
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

/**
 * Provides an implicit conversion that enables <code>pretty</code> to be invoked on any
 * object, to transform that object into a <code>String</code> representation.
 */
trait PrettyMethods {

  /**
   * Implicit class that adds a <code>pretty</code> method to any object.
   *
   * <p>
   * The constructor of this class, besides taking an object <code>o</code> to prettify,
   * also takes an implicit <code>Prettifier</code> that the <code>pretty</code> method will use to prettify the
   * object.
   * </p>
   *
   * @param o the object to prettify
   * @param prettifier an implicit <code>Prettifier</code> that will be used
   *     to prettify the passed object <code>o</code>
   */
  implicit class Prettyizer(o: Any)(implicit prettifier: Prettifier) {

    /**
     * Returns a pretty <code>String</code> representation of the object <code>o</code>
     */
    def pretty: String = prettifier(o)
  }
}

/**
 * Companion object for trait <code>PrettyMethods</code> enabling its members to be imported as an
 * alternative to mixing them in.
 */
object PrettyMethods extends PrettyMethods

