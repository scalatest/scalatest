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
package org.scalatest.words

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class ResultOfLessThanOrEqualToComparison[T <% Ordered[T]](val right: T) {

  /**
   * This method is invoked by <code>be</code> methods to which instances of this class are passed, which
   * enables syntax such as:
   *
   * <pre class="stHighlight">
   * result should not be &lt;= (7)
   *                   ^  ... invoked by this be method
   * </pre>
   *
   * <p>
   * or
   * </p>
   *
   * <pre class="stHighlight">
   * num should (not be &lt;= (10) and not be &gt; (17))
   *                 ^  ... invoked by this be method
   * </pre>
   */
  def apply(left: T): Boolean = left <= right
}

