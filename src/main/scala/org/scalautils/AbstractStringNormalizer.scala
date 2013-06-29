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

abstract class AbstractStringNormalizer extends Normalizer[String] {

  // TODO: Scaladoc
  def canNormalize(b: Any): Boolean = b.isInstanceOf[String]

  /**
   * Indicates whether the passed object is an instance of <code>String</code>.
   *
   * @return true if the passed object is a <code>String</code>.
   */
  def normalizedOrSame(b: Any) =
    b match {
      case s: String => normalized(s)
      case _ => b
   }
}

