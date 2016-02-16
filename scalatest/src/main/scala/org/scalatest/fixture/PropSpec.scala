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
package org.scalatest.fixture

import org.scalatest._
import scala.collection.immutable.ListSet
import org.scalatest.Suite.autoTagClassAnnotations

/**
 * A sister class to <code>org.scalatest.PropSpec</code> that can pass a fixture object into its tests.
 *
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Use class <code>fixture.PropSpec</code> in situations for which <a href="../PropSpec.html"><code>PropSpec</code></a>
 * would be a good choice, when all or most tests need the same fixture objects
 * that must be cleaned up afterwards. <em>Note: <code>fixture.PropSpec</code> is intended for use in special
 * situations, with class <code>PropSpec</code> used for general needs. For
 * more insight into where <code>fixture.PropSpec</code> fits in the big picture, see
 * the <a href="../PropSpec.html#withFixtureOneArgTest"><code>withFixture(OneArgTest)</code></a> subsection of
 * the <a href="../PropSpec.html#sharedFixtures">Shared fixtures</a> section in the documentation for class <code>PropSpec</code>.</em>
 * </td></tr></table>
 * 
 * <p>
 * Class <code>fixture.PropSpec</code> behaves similarly to class <code>org.scalatest.PropSpec</code>, except that tests may have a
 * fixture parameter. The type of the
 * fixture parameter is defined by the abstract <code>FixtureParam</code> type, which is a member of this class.
 * This class also has an abstract <code>withFixture</code> method. This <code>withFixture</code> method
 * takes a <code>OneArgTest</code>, which is a nested trait defined as a member of this class.
 * <code>OneArgTest</code> has an <code>apply</code> method that takes a <code>FixtureParam</code>.
 * This <code>apply</code> method is responsible for running a test.
 * This class's <code>runTest</code> method delegates the actual running of each test to <code>withFixture</code>, passing
 * in the test code to run via the <code>OneArgTest</code> argument. The <code>withFixture</code> method (abstract in this class) is responsible
 * for creating the fixture argument and passing it to the test function.
 * </p>
 * 
 * <p>
 * Subclasses of this class must, therefore, do three things differently from a plain old <code>org.scalatest.PropSpec</code>:
 * </p>
 * 
 * <ol>
 * <li>define the type of the fixture parameter by specifying type <code>FixtureParam</code></li>
 * <li>define the <code>withFixture(OneArgTest)</code> method</li>
 * <li>write tests that take a fixture parameter</li>
 * <li>(You can also define tests that don't take a fixture parameter.)</li>
 * </ol>
 *
 * <p>
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.fixture.propspec
 * 
 * import org.scalatest._
 * import prop.PropertyChecks
 * import java.io._
 * 
 * class ExampleSpec extends fixture.PropSpec with PropertyChecks with Matchers {
 * 
 *   // 1. define type FixtureParam
 *   type FixtureParam = FileReader
 * 
 *   // 2. define the withFixture method
 *   def withFixture(test: OneArgTest) = {
 * 
 *     val FileName = "TempFile.txt"
 * 
 *     // Set up the temp file needed by the test
 *     val writer = new FileWriter(FileName)
 *     try {
 *       writer.write("Hello, test!")
 *     }
 *     finally {
 *       writer.close()
 *     }
 * 
 *     // Create the reader needed by the test
 *     val reader = new FileReader(FileName)
 * 
 *     try {
 *       // Run the test using the temp file
 *       test(reader)
 *     }
 *     finally {
 *       // Close and delete the temp file
 *       reader.close()
 *       val file = new File(FileName)
 *       file.delete()
 *     }
 *   }
 * 
 *   // 3. write property-based tests that take a fixture parameter
 *   // (Hopefully less contrived than the examples shown here.)
 *   property("can read from a temp file") { reader =&gt;
 *     var builder = new StringBuilder
 *     var c = reader.read()
 *     while (c != -1) {
 *       builder.append(c.toChar)
 *       c = reader.read()
 *     }
 *     val fileContents = builder.toString
 *     forAll { (c: Char) =&gt;
 *       whenever (c != 'H') {
 *         fileContents should not startWith c.toString
 *       }
 *     }
 *   }
 * 
 *   property("can read the first char of the temp file") { reader =&gt;
 *     val firstChar = reader.read()
 *     forAll { (c: Char) =&gt;
 *       whenever (c != 'H') {
 *         c should not equal firstChar
 *       }
 *     }
 *   }
 * 
 *   // (You can also write tests that don't take a fixture parameter.)
 *   property("can write tests that don't take the fixture") { () =&gt;
 *     forAll { (i: Int) => i + i should equal (2 * i) }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Note: to run the examples on this page, you'll need to include <a href="http://www.scalacheck.org">ScalaCheck</a> on the classpath in addition to ScalaTest.
 * </p>
 *
 * <p>
 * In the previous example, <code>withFixture</code> creates and initializes a temp file, then invokes the test function,
 * passing in a <code>FileReader</code> connected to that file.  In addition to setting up the fixture before a test,
 * the <code>withFixture</code> method also cleans it up afterwards. If you need to do some clean up
 * that must happen even if a test fails, you should invoke the test function from inside a <code>try</code> block and do
 * the cleanup in a <code>finally</code> clause, as shown in the previous example.
 * </p>
 *
 * <p>
 * If a test fails, the <code>OneArgTest</code> function will result in a [[org.scalatest.Failed Failed]] wrapping the
 * exception describing the failure.
 * The reason you must perform cleanup in a <code>finally</code> clause is that in case an exception propagates back through
 * <code>withFixture</code>, the <code>finally</code> clause will ensure the fixture cleanup happens as that exception
 * propagates back up the call stack to <code>runTest</code>.
 * </p>
 *
 * <p>
 * If a test doesn't need the fixture, you can indicate that by providing a no-arg instead of a one-arg function.
 * In other words, instead of starting your function literal
 * with something like &ldquo;<code>reader =&gt;</code>&rdquo;, you'd start it with &ldquo;<code>() =&gt;</code>&rdquo;, as is done
 * in the third test in the above example. For such tests, <code>runTest</code>
 * will not invoke <code>withFixture(OneArgTest)</code>. It will instead directly invoke <code>withFixture(NoArgTest)</code>.
 * </p>
 *
 * <a name="multipleFixtures"></a>
 * <h2>Passing multiple fixture objects</h2>
 *
 * <p>
 * If the fixture you want to pass into your tests consists of multiple objects, you will need to combine
 * them into one object to use this class. One good approach to passing multiple fixture objects is
 * to encapsulate them in a case class. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * case class FixtureParam(builder: StringBuilder, buffer: ListBuffer[String])
 * </pre>
 *
 * <p>
 * To enable the stacking of traits that define <code>withFixture(NoArgTest)</code>, it is a good idea to let
 * <code>withFixture(NoArgTest)</code> invoke the test function instead of invoking the test
 * function directly. To do so, you'll need to convert the <code>OneArgTest</code> to a <code>NoArgTest</code>. You can do that by passing
 * the fixture object to the <code>toNoArgTest</code> method of <code>OneArgTest</code>. In other words, instead of
 * writing &ldquo;<code>test(theFixture)</code>&rdquo;, you'd delegate responsibility for
 * invoking the test function to the <code>withFixture(NoArgTest)</code> method of the same instance by writing:
 * </p>
 *
 * <pre>
 * withFixture(test.toNoArgTest(theFixture))
 * </pre>
 *
 * <p>
 * Here's a complete example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.fixture.propspec.multi
 * 
 * import org.scalatest._
 * import prop.PropertyChecks
 * import scala.collection.mutable.ListBuffer
 * 
 * class ExampleSpec extends fixture.PropSpec with PropertyChecks with Matchers {
 * 
 *   case class FixtureParam(builder: StringBuilder, buffer: ListBuffer[String])
 * 
 *   def withFixture(test: OneArgTest) = {
 * 
 *     // Create needed mutable objects
 *     val stringBuilder = new StringBuilder("ScalaTest is ")
 *     val listBuffer = new ListBuffer[String]
 *     val theFixture = FixtureParam(stringBuilder, listBuffer)
 * 
 *     // Invoke the test function, passing in the mutable objects
 *     withFixture(test.toNoArgTest(theFixture))
 *   }
 * 
 *   property("testing should be easy") { f =&gt;
 *     f.builder.append("easy!")
 *     assert(f.builder.toString === "ScalaTest is easy!")
 *     assert(f.buffer.isEmpty)
 *     val firstChar = f.builder(0)
 *     forAll { (c: Char) =&gt;
 *       whenever (c != 'S') {
 *         c should not equal firstChar
 *       }
 *     }
 *     f.buffer += "sweet"
 *   }
 * 
 *   property("testing should be fun") { f =&gt;
 *     f.builder.append("fun!")
 *     assert(f.builder.toString === "ScalaTest is fun!")
 *     assert(f.buffer.isEmpty)
 *     val firstChar = f.builder(0)
 *     forAll { (c: Char) =&gt;
 *       whenever (c != 'S') {
 *         c should not equal firstChar
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * @author Bill Venners
 */
@Finders(Array("org.scalatest.finders.PropSpecFinder"))
abstract class AnyPropSpec extends AnyPropSpecLike {

  /**
   * Returns a user friendly string for this suite, composed of the
   * simple name of the class (possibly simplified further by removing dollar signs if added by the Scala interpeter) and, if this suite
   * contains nested suites, the result of invoking <code>toString</code> on each
   * of the nested suites, separated by commas and surrounded by parentheses.
   *
   * @return a user-friendly string for this suite
   */
  override def toString: String = Suite.suiteToString(None, this)
}
