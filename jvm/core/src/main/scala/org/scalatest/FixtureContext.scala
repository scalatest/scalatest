/*
 * Copyright 2001-2025 Artima, Inc.
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

/**
 * Marker trait for fixture-context objects, that enables them
 * to be used in testing styles that require type <code>Assertion</code>
 *
 * <p>
 * A fixture-context object is a way to share fixtures between different
 * tests that is most useful when different tests need different combinations
 * of fixture objects. The fixture-context object technique is only
 * appropriate if you don't need to clean up the fixtures after using them.
 * </p>
 *
 * <p>
 * To use this technique, you define instance variables intialized with fixture
 * objects in traits and/or classes, then in each test instantiate an object that
 * contains just the fixture objects needed by the test. Traits allow you to mix
 * together just the fixture objects needed by each test, whereas classes
 * allow you to pass data in via a constructor to configure the fixture objects.
 * Here's an example <code>FlatSpec</code> in which fixture objects are partitioned
 * into two traits and each test just mixes together the traits it needs:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.flatspec.fixturecontext
 * 
 * import collection.mutable.ListBuffer
 * import org.scalatest.FlatSpec
 * import org.scalatest.FixtureContext
 * 
 * class ExampleSpec extends FlatSpec {
 * 
 *   trait Builder extends FixtureContext {
 *     val builder = new StringBuilder("ScalaTest is ")
 *   }
 * 
 *   trait Buffer extends FixtureContext {
 *     val buffer = ListBuffer("ScalaTest", "is")
 *   }
 * 
 *   // This test needs the StringBuilder fixture
 *   "Testing" should "be productive" in new Builder {
 *     builder.append("productive!")
 *     assert(builder.toString === "ScalaTest is productive!")
 *   }
 * 
 *   // This test needs the ListBuffer[String] fixture
 *   "Test code" should "be readable" in new Buffer {
 *     buffer += ("readable!")
 *     assert(buffer === List("ScalaTest", "is", "readable!"))
 *   }
 * 
 *   // This test needs both the StringBuilder and ListBuffer
 *   it should "be clear and concise" in new Builder with Buffer {
 *     builder.append("clear!")
 *     buffer += ("concise!")
 *     assert(builder.toString === "ScalaTest is clear!")
 *     assert(buffer === List("ScalaTest", "is", "concise!"))
 *   }
 * }
 * </pre>
 *
 * <p>
 * Extending <code>FixtureContext</code>, which extends trait <code>org.scalatest.compatible.Assertion</code> makes
 * it more convenient to use fixture-context objects in styles, such as async styles, that require test bodies
 * to have type <code>Assertion</code>.
 * </p>
 */
trait FixtureContext extends compatible.Assertion

