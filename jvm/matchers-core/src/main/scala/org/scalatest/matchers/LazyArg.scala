
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
package org.scalatest.matchers

/**
 * Transforms a given object's <code>toString</code> with a given function lazily.
 *
 * <p>
 * This class is intended to be used with the <code>mapResult</code> method of
 * <a href="MatcherProducers.html"><code>MatcherProducers</code></a>, which you can use to 
 * modify error messages when composing matchers. This class exists to enable those error messages
 * to be modified <em>lazily</em>, so that <code>toString</code> is invoked on the given
 * <code>arg</code>, and its result transformed by the given function <code>f</code>, <em>only when and if</em>
 * the <code>toString</code> method is invoked on the <code>LazyArg</code>. As a performance optimization, ScalaTest's
 * <a href="MatchResult.html"><code>MatchResult</code></a> avoids invoking <code>toString</code> on objects
 * until and unless an error message is actually needed, to minimize unecessary creation and concatenation
 * of strings. The <code>LazyArg</code> class enables this same performance optimization when composing
 * matchers.
 * </p>
 *
 * <p>
 * The other design goal of <code>LazyArg</code> is to make the internal <code>arg</code> available for inspection
 * in an IDE. In a future version of ScalaTest, the <code>args</code> of <code>MatchResult</code> that were used
 * to create the error message will be included in the <code>TestFailedException</code>, so they can be inspected
 * in IDEs. This is why the <code>arg</code> field of <code>LazyArg</code> is public.
 * </p>
 *
 * <p>
 * For an example of using <code>LazyArg</code>, see the <a href="Matcher.html#composingMatchers">Composing matchers</a>
 * section in the main documentation for trait <code>Matcher</code>.
 * </p>
 *
 * @param arg the argument
 * @param f a function that given the <code>arg</code> will produce a <code>String</code>
 */
final case class LazyArg(val arg: Any)(f: Any => String) {
 
  /**
   * Returns the result of invoking the function <code>f</code>, passed to the <code>LazyArg</code>
   * constructor, on field <code>arg</code>.
   */
  override def toString = f(arg.toString)
}

