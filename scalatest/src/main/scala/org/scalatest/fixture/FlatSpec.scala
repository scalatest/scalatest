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
import FixtureNodeFamily._
import words.{ResultOfTaggedAsInvocation, ResultOfStringPassedToVerb, BehaveWord, ShouldVerb, MustVerb, CanVerb}
import scala.collection.immutable.ListSet
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepth
import java.util.concurrent.atomic.AtomicReference
import java.util.ConcurrentModificationException
import org.scalatest.events._
import org.scalatest.Suite.anExceptionThatShouldCauseAnAbort
import org.scalatest.Suite.autoTagClassAnnotations

/**
 * A sister class to <code>org.scalatest.FlatSpec</code> that can pass a fixture object into its tests.
 *
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Use class <code>fixture.FlatSpec</code> in situations for which <a href="../FlatSpec.html"><code>FlatSpec</code></a>
 * would be a good choice, when all or most tests need the same fixture objects
 * that must be cleaned up afterwards. <em>Note: <code>fixture.FlatSpec</code> is intended for use in special situations, with class <code>FlatSpec</code> used for general needs. For
 * more insight into where <code>fixture.FlatSpec</code> fits in the big picture, see the <a href="../FlatSpec.html#withFixtureOneArgTest"><code>withFixture(OneArgTest)</code></a> subsection of the <a href="../FlatSpec.html#sharedFixtures">Shared fixtures</a> section in the documentation for class <code>FlatSpec</code>.</em>
 * </td></tr></table>
 * 
 * <p>
 * Class <code>fixture.FlatSpec</code> behaves similarly to class <code>org.scalatest.FlatSpec</code>, except that tests may have a
 * fixture parameter. The type of the
 * fixture parameter is defined by the abstract <code>FixtureParam</code> type, which is a member of this class.
 * This class also contains an abstract <code>withFixture</code> method. This <code>withFixture</code> method
 * takes a <code>OneArgTest</code>, which is a nested trait defined as a member of this class.
 * <code>OneArgTest</code> has an <code>apply</code> method that takes a <code>FixtureParam</code>.
 * This <code>apply</code> method is responsible for running a test.
 * This class's <code>runTest</code> method delegates the actual running of each test to <code>withFixture(OneArgTest)</code>, passing
 * in the test code to run via the <code>OneArgTest</code> argument. The <code>withFixture(OneArgTest)</code> method (abstract in this class) is responsible
 * for creating the fixture argument and passing it to the test function.
 * </p>
 * 
 * <p>
 * Subclasses of this class must, therefore, do three things differently from a plain old <code>org.scalatest.FlatSpec</code>:
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
 * If the fixture you want to pass into your tests consists of multiple objects, you will need to combine
 * them into one object to use this class. One good approach to passing multiple fixture objects is
 * to encapsulate them in a case class. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * case class FixtureParam(file: File, writer: FileWriter)
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
 * package org.scalatest.examples.flatspec.oneargtest
 * 
 * import org.scalatest.fixture
 * import java.io._
 * 
 * class ExampleSpec extends fixture.FlatSpec {
 * 
 *   case class FixtureParam(file: File, writer: FileWriter)
 * 
 *   def withFixture(test: OneArgTest) = {
 * 
 *     // create the fixture
 *     val file = File.createTempFile("hello", "world")
 *     val writer = new FileWriter(file)
 *     val theFixture = FixtureParam(file, writer)
 * 
 *     try {
 *       writer.write("ScalaTest is ") // set up the fixture
 *       withFixture(test.toNoArgTest(theFixture)) // "loan" the fixture to the test
 *     }
 *     finally writer.close() // clean up the fixture
 *   }
 * 
 *   "Testing" should "be easy" in { f =&gt;
 *     f.writer.write("easy!")
 *     f.writer.flush()
 *     assert(f.file.length === 18)
 *   }
 * 
 *   it should "be fun" in { f =&gt;
 *     f.writer.write("fun!")
 *     f.writer.flush()
 *     assert(f.file.length === 17)
 *   }
 * }
 * </pre>
 *
 * <p>
 * If a test fails because of an exception, the <code>OneArgTest</code> function will result in a [[org.scalatest.Failed Failed]] wrapping the exception.
 * To ensure clean up happens even if an exception occurs, you should invoke the test function from inside a <code>try</code> block and do the cleanup in a
 * <code>finally</code> clause, as shown in the previous example.
 * </p>
 *
 * <a name="sharingFixturesAcrossClasses"></a><h2>Sharing fixtures across classes</h2>
 *
 * <p>
 * If multiple test classes need the same fixture, you can define the <code>FixtureParam</code> and <code>withFixture(OneArgTest)</code> implementations
 * in a trait, then mix that trait into the test classes that need it. For example, if your application requires a database and your integration tests
 * use that database, you will likely have many test classes that need a database fixture. You can create a "database fixture" trait that creates a
 * database with a unique name, passes the connector into the test, then removes the database once the test completes. This is shown in the following example:
 * </p>
 * 
 * <pre class="stHighlight">
 * package org.scalatest.examples.fixture.flatspec.sharing
 * 
 * import java.util.concurrent.ConcurrentHashMap
 * import org.scalatest.fixture
 * import DbServer._
 * import java.util.UUID.randomUUID
 * 
 * object DbServer { // Simulating a database server
 *   type Db = StringBuffer
 *   private val databases = new ConcurrentHashMap[String, Db]
 *   def createDb(name: String): Db = {
 *     val db = new StringBuffer
 *     databases.put(name, db)
 *     db
 *   }
 *   def removeDb(name: String) {
 *     databases.remove(name)
 *   }
 * }
 * 
 * trait DbFixture { this: fixture.Suite =&gt;
 * 
 *   type FixtureParam = Db
 * 
 *   // Allow clients to populate the database after
 *   // it is created
 *   def populateDb(db: Db) {}
 * 
 *   def withFixture(test: OneArgTest) = {
 *     val dbName = randomUUID.toString
 *     val db = createDb(dbName) // create the fixture
 *     try {
 *       populateDb(db) // setup the fixture
 *       withFixture(test.toNoArgTest(db)) // "loan" the fixture to the test
 *     }
 *     finally removeDb(dbName) // clean up the fixture
 *   }
 * }
 * 
 * class ExampleSpec extends fixture.FlatSpec with DbFixture {
 * 
 *   override def populateDb(db: Db) { // setup the fixture
 *     db.append("ScalaTest is ")
 *   }
 * 
 *   "Testing" should "be easy" in { db =&gt;
 *       db.append("easy!")
 *       assert(db.toString === "ScalaTest is easy!")
 *   }
 * 
 *   it should "be fun" in { db =&gt;
 *       db.append("fun!")
 *       assert(db.toString === "ScalaTest is fun!")
 *   }
 * 
 *   // This test doesn't need a Db
 *   "Test code" should "be clear" in { () =&gt;
 *       val buf = new StringBuffer
 *       buf.append("ScalaTest code is ")
 *       buf.append("clear!")
 *       assert(buf.toString === "ScalaTest code is clear!")
 *   }
 * }
 * </pre>
 *
 * <p>
 * Often when you create fixtures in a trait like <code>DbFixture</code>, you'll still need to enable individual test classes
 * to "setup" a newly created fixture before it gets passed into the tests. A good way to accomplish this is to pass the newly
 * created fixture into a setup method, like <code>populateDb</code> in the previous example, before passing it to the test
 * function. Classes that need to perform such setup can override the method, as does <code>ExampleSpec</code>.
 * </p>
 *
 * <p>
 * If a test doesn't need the fixture, you can indicate that by providing a no-arg instead of a one-arg function, as is done in the
 * third test in the previous example, &ldquo;<code>Test code should be clear</code>&rdquo;. In other words, instead of starting your function literal
 * with something like &ldquo;<code>db =&gt;</code>&rdquo;, you'd start it with &ldquo;<code>() =&gt;</code>&rdquo;. For such tests, <code>runTest</code>
 * will not invoke <code>withFixture(OneArgTest)</code>. It will instead directly invoke <code>withFixture(NoArgTest)</code>.
 * </p>
 *
 * <p>
 * Both examples shown above demonstrate the technique of giving each test its own "fixture sandbox" to play in. When your fixtures
 * involve external side-effects, like creating files or databases, it is a good idea to give each file or database a unique name as is
 * done in these examples. This keeps tests completely isolated, allowing you to run them in parallel if desired. You could mix
 * <a href="../ParallelTestExecution.html"><code>ParallelTestExecution</code></a> into either of these <code>ExampleSpec</code> classes, and the tests would run in parallel just fine.
 * </p>
 *
 * @author Bill Venners
 */
@Finders(Array("org.scalatest.finders.FlatSpecFinder"))
abstract class AnyFlatSpec extends AnyFlatSpecLike {

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
