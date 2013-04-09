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
 * Abstract class that supports test registration in <code>FlatSpec</code>
 * and <code>fixture.FlatSpec</code>.
 *
 * <p>
 * For example, this class enables syntax such as the following pending test registration
 * in <code>FlatSpec</code> and <code>fixture.FlatSpec</code>:
 * </p>
 *
 * <pre>
 * "A Stack (when empty)" should "be empty" is (pending)
 *                                          ^
 * </pre>
 *
 *
 * <p>
 * For example, this class enables syntax such as the following tagged test registration
 * in <code>FlatSpec</code> and <code>fixture.FlatSpec</code>:
 * </p>
 *
 * <pre>
 * "A Stack (when empty)" should "be empty" taggedAs(SlowTet) in { ... }
 *                                          ^
 * </pre>
 *
 * <p>
 * This class also indirectly enables syntax such as the following regular test registration
 * in <code>FlatSpec</code> and <code>fixture.FlatSpec</code>:
 * </p>
 *
 * <pre>
 * "A Stack (when empty)" should "be empty" in { ... }
 *                                          ^
 * </pre>
 *
 * <p>
 * However, this class does not declare any methods named <code>in</code>, because the
 * type passed to <code>in</code> differs in a <code>FlatSpec</code> and a <code>fixture.FlatSpec</code>.
 * A <code>fixture.FlatSpec</code> needs two <code>in</code> methods, one that takes a no-arg
 * test function and another that takes a one-arg test function (a test that takes a
 * <code>Fixture</code> as its parameter). By constrast, a <code>FlatSpec</code> needs
 * only one <code>in</code> method that takes a by-name parameter. As a result,
 * <code>FlatSpec</code> and <code>fixture.FlatSpec</code> each provide an implicit conversion
 * from <code>ResultOfStringPassedToVerb</code> to a type that provides the appropriate
 * <code>in</code> methods. 
 * </p>
 *
 * @author Bill Venners
 */
abstract class ResultOfStringPassedToVerb(val verb: String, val rest: String) {

  /**
   * Supports the registration of pending tests in a
   * <code>FlatSpec</code> and <code>fixture.FlatSpec</code>.
   *
   * <p>
   * This method supports syntax such as the following:
   * </p>
   *
   * <pre>
   * "A Stack" must "pop values in last-in-first-out order" is (pending)
   *                                                        ^
   * </pre>
   *
   * <p>
   * For examples of pending test registration, see the <a href="../FlatSpec.html#PendingTests">Pending tests section</a> in the main documentation
   * for trait <code>FlatSpec</code>.
   * </p>
   */
  def is(fun: => PendingNothing)

  /**
   * Supports the registration of tagged tests in <code>FlatSpec</code> and <code>fixture.FlatSpec</code>.
   *
   * <p>
   * This method supports syntax such as the following:
   * </p>
   *
   * <pre>
   * "A Stack" must "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
   *                                                        ^
   * </pre>
   *
   * <p>
   * For examples of tagged test registration, see the <a href="../FlatSpec.html#TaggingTests">Tagging tests section</a> in the main documentation
   * for trait <code>FlatSpec</code>.
   * </p>
   */
  def taggedAs(firstTestTag: Tag, otherTestTags: Tag*): ResultOfTaggedAsInvocation
}
