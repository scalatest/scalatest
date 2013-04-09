/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest;

import java.lang.annotation.*; 

/**
 * Annotation used to tag a test, or suite of tests, as ignored.
 *
 * <p>
 * <em>Note: This is actually an annotation defined in Java, not a Scala trait. It must be defined in Java instead of Scala so it will be accessible
 * at runtime. It has been inserted into Scaladoc by pretending it is a trait.</em>
 * </p>
 *
 * <p>
 * If you wish to temporarily ignore an entire suite of tests, you can annotate the test class with <code>@Ignore</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.flatspec.ignoreall
 * 
 * import org.scalatest._
 * 
 * @Ignore
 * class SetSpec extends FlatSpec {
 * 
 *   "An empty Set" should "have size 0" in {
 *     assert(Set.empty.size === 0)
 *   }
 * 
 *   it should "produce NoSuchElementException when head is invoked" in {
 *     intercept[NoSuchElementException] {
 *       Set.empty.head
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * When you mark a test class with a tag annotation, ScalaTest will mark each test defined in that class with that tag.
 * Thus, marking the <code>SetSpec</code> in the above example with the <code>@Ignore</code> tag annotation means that both tests
 * in the class will be ignored. If you run the above <code>SetSpec</code> in the Scala interpreter, you'll see:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new SetSpec execute
 * <span class="stGreen">An empty Set</span>
 * <span class="stYellow">- should have size 0 !!! IGNORED !!!
 * - should produce NoSuchElementException when head is invoked !!! IGNORED !!!</span>
 * </pre>
 *
 * <p>
 * Note that marking a test class as ignored won't prevent it from being discovered by ScalaTest. Ignored classes
 * will be discovered and run, and all their tests will be reported as ignored. This is intended to keep the ignored
 * class somewhat visible, to encourage the developers to eventually fix and un-ignore it. If you want to
 * prevent a class from being discovered at all, use the <a href="DoNotDiscover.html"><code>DoNotDiscover</code></a> annotation instead.
 * </p>
 *
 * <p>
 * Another use case for <code>@Ignore</code> is to mark test <em>methods</em> as ignored in traits <a href="Suite.html"><code>Suite</code></a>
 * and <a href="fixture/Suite.html"><code>fixture.Suite</code></a>. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.suite.ignore
 *
 * import org.scalatest._
 *
 * class SetSuite extends Suite {
 *
 *   @Ignore def &#96;test: an empty Set should have size 0&#96; {
 *     assert(Set.empty.size === 0)
 *   }
 *
 *   def &#96;test: invoking head on an empty Set should produce NoSuchElementException&#96; {
 *     intercept[NoSuchElementException] {
 *       Set.empty.head
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this version of <code>SetSuite</code> in the Scala interpreter, you'll see that it
 * runs only the second test and reports that the first test was ignored:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new SetSuite execute
 * <span class="stGreen">SetSuite:
 * <span class="stYellow">- an empty Set should have size 0 !!! IGNORED !!!</span>
 * <span class="stGreen">- invoking head on an empty Set should produce NoSuchElementException</span>
 * </pre>
 *
 * @author Bill Venners
 * @author Chua Chee Seng
 */
@TagAnnotation
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.TYPE})
public @interface Ignore {
}
