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
package org.scalatest

package object matchers {

  /**
   * Convenience type alias allowing <code>MustMatchers</code> to be used in <code>matchers</code> without qualification or another import
   * after a wildcard import of <code>org.scalatest</code>.
   */

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.matchers.MustMatchers</code> to <code>org.scalatest.MustMatchers</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.MustMatchers instead.")
  type MustMatchers = org.scalatest.MustMatchers

}