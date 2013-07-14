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

/**
 * Convenience base trait for string <a href="Uniformity.html"><code>Uniformity</code></a>s.
 *
 * <p>
 * This trait defines a <code>normalizedCanHandle</code> method that returns true if the passed
 * <code>Any</code> is a <code>String</code> and a <code>normalizedOrSame</code> method that
 * normalizes any passed <code>String</code>s via the <code>normalized</code> method, which is
 * left abstract for subclasses to fill in.
 * </p>
 */
trait AbstractStringUniformity extends Uniformity[String] {

  /**
   * Returns true if the passed <code>Any</code> is a <code>String</code>.
   *
   * @return true if the passed <code>Any</code> is a <code>String</code>.
   */
  final def normalizedCanHandle(b: Any): Boolean = b.isInstanceOf[String]

  /**
   * Normalizes the passed object if it is a <code>String</code>.
   *
   * <p>
   * This method returns either:
   * </p>
   *
   * <ul>
   * <li>if the passed object is a <code>String</code>, the result of passing that string to <code>normalized</code></li>
   * <li>else, the same exact object that was passed
   * </p>
   *
   * @return a normalized form of any passed <code>String</code>, or the same object if not a <code>String</code>.
   */
  final def normalizedOrSame(b: Any) =
    b match {
      case s: String => normalized(s)
      case _ => b
   }
}

