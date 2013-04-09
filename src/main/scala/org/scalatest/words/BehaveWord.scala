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

/**
 * Class that supports shared test registration via instances referenced from the <code>behave</code> field of <code>FunSpec</code>s,
 * <code>FlatSpec</code>s, and <code>WordSpec</code>s as well as instance of their sister traits, 
 * <code>fixture.FunSpec</code>, <code>fixture.FlatSpec</code>, and <code>fixture.WordSpec</code>.
 *
 * <p>
 * This class, via the <code>behave</code> field, enables syntax such as the following in <code>FunSpec</code>s, <code>FlatSpec</code>s,
 * <code>fixture.FunSpec</code>s, and <code>fixture.FlatSpec</code>s:
 * </p>
 *
 * <pre class="stHighlight">
 * it should behave like nonFullStack(stackWithOneItem)
 *           ^
 * </pre>
 *
 * <p>
 * It also enables syntax such as the following syntax in <code>WordSpec</code>s and <code>fixture.WordSpec</code>s:
 * </p>
 *
 * <pre class="stHighlight">
 * behave like nonEmptyStack(lastValuePushed)
 * ^
 * </pre>
 *
 * <p>
 * For more information and examples of the use of <cod>behave</code>, see the Shared tests section
 * in the main documentation for trait <a href="../FunSpec.html#SharedTests"><code>FunSpec</code></a>,
 * <a href="../FlatSpec.html#SharedTests"><code>FlatSpec</code></a>, or <a href="../WordSpec.html#SharedTests"><code>WordSpec</code></a>.
 * </p>
 */
final class BehaveWord {

  /**
   * Supports the registration of shared tests.
   *
   * <p>
   * This method enables syntax such as the following in <code>FunSpec</code>s, <code>FlatSpec</code>s,
   * <code>fixture.FunSpec</code>s, and <code>fixture.FlatSpec</code>s:
   * </p>
   *
   * <pre class="stHighlight">
   * it should behave like nonFullStack(stackWithOneItem)
   *                  ^
   * </pre>
   *
   * <p>
   * It also enables syntax such as the following syntax in <code>WordSpec</code>s and <code>fixture.WordSpec</code>s:
   * </p>
   *
   * <pre class="stHighlight">
   * behave like nonEmptyStack(lastValuePushed)
   * ^
   * </pre>
   *
   * <p>
   * This method just provides syntax sugar intended to make the intent of the code clearer.
   * Because the parameter passed to it is
   * type <code>Unit</code>, the expression will be evaluated before being passed, which
   * is sufficient to register the shared tests.
   * For more information and examples of the use of <cod>behave</code>, see the Shared tests section
   * in the main documentation for trait <a href="../FunSpec.html#SharedTests"><code>FunSpec</code></a>,
   * <a href="../FlatSpec.html#SharedTests"><code>FlatSpec</code></a>, or <a href="../WordSpec.html#SharedTests"><code>WordSpec</code></a>.
   * </p>
   */
  def like(unit: Unit) = ()
}
