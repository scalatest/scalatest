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
package org.scalatest.funspec

/**
 * A sister class to <code>org.scalatest.funspec.AsyncFunSpec</code> that can pass a fixture object into its tests.
 *
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Use class <code>FixtureAsyncFunSpec</code> in situations for which <a href="AsyncFunSpec.html"><code>AsyncFunSpec</code></a>
 * would be a good choice, when all or most tests need the same fixture objects
 * that must be cleaned up afterwards. <em>Note: <code>FixtureAsyncFunSpec</code> is intended for use in special situations, with class <code>AsyncFunSpec</code> used for general needs. For
 * more insight into where <code>FixtureAsyncFunSpec</code> fits in the big picture, see the <a href="AsyncFunSpec.html#withFixtureOneArgAsyncTest"><code>withFixture(OneArgAsyncTest)</code></a> subsection of the <a href="AsyncFunSpec.html#sharedFixtures">Shared fixtures</a> section in the documentation for class <code>AsyncFunSpec</code>.</em>
 * </td></tr></table>
 *
 * <p>
 * Class <code>FixtureAsyncFunSpec</code> behaves similarly to class <code>org.scalatest.funspec.AsyncFunSpec</code>, except that tests may have a
 * fixture parameter. The type of the
 * fixture parameter is defined by the abstract <code>FixtureParam</code> type, which is a member of this class.
 * This class also contains an abstract <code>withFixture</code> method. This <code>withFixture</code> method
 * takes a <code>OneArgAsyncTest</code>, which is a nested trait defined as a member of this class.
 * <code>OneArgAsyncTest</code> has an <code>apply</code> method that takes a <code>FixtureParam</code>.
 * This <code>apply</code> method is responsible for running a test.
 * This class's <code>runTest</code> method delegates the actual running of each test to <code>withFixture(OneArgAsyncTest)</code>, passing
 * in the test code to run via the <code>OneArgAsyncTest</code> argument. The <code>withFixture(OneArgAsyncTest)</code> method (abstract in this class) is responsible
 * for creating the fixture argument and passing it to the test function.
 * </p>
 *
 * <p>
 * Subclasses of this class must, therefore, do three things differently from a plain old <code>org.scalatest.funspec.AsyncFunSpec</code>:
 * </p>
 *
 * <ol>
 * <li>define the type of the fixture parameter by specifying type <code>FixtureParam</code></li>
 * <li>define the <code>withFixture(OneArgAsyncTest)</code> method</li>
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
 * To enable the stacking of traits that define <code>withFixture(NoArgAsyncTest)</code>, it is a good idea to let
 * <code>withFixture(NoArgAsyncTest)</code> invoke the test function instead of invoking the test
 * function directly. To do so, you'll need to convert the <code>OneArgAsyncTest</code> to a <code>NoArgAsyncTest</code>. You can do that by passing
 * the fixture object to the <code>toNoArgAsyncTest</code> method of <code>OneArgAsyncTest</code>. In other words, instead of
 * writing &ldquo;<code>test(theFixture)</code>&rdquo;, you'd delegate responsibility for
 * invoking the test function to the <code>withFixture(NoArgAsyncTest)</code> method of the same instance by writing:
 * </p>
 *
 * <pre>
 * withFixture(test.toNoArgAsyncTest(theFixture))
 * </pre>
 *
 * <p>
 * Here's a complete example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.asyncfunspec.oneargasynctest
 *
 * import org.scalatest._
 * import scala.concurrent.Future
 * import scala.concurrent.ExecutionContext
 *
 * // Defining actor messages
 * sealed abstract class StringOp
 * case object Clear extends StringOp
 * case class Append(value: String) extends StringOp
 * case object GetValue
 *
 * class StringActor { // Simulating an actor
 *   private final val sb = new StringBuilder
 *   def !(op: StringOp): Unit =
 *     synchronized {
 *       op match {
 *         case Append(value) => sb.append(value)
 *         case Clear => sb.clear()
 *       }
 *     }
 *   def ?(get: GetValue.type)(implicit c: ExecutionContext): Future[String] =
 *     Future {
 *       synchronized { sb.toString }
 *     }
 * }
 *
 * class ExampleSpec extends funspec.FixtureAsyncFunSpec {
 *
 *   type FixtureParam = StringActor
 *
 *   def withFixture(test: OneArgAsyncTest): FutureOutcome = {
 *
 *     val actor = new StringActor
 *     complete {
 *       actor ! Append("ScalaTest is ") // set up the fixture
 *       withFixture(test.toNoArgAsyncTest(actor))
 *     } lastly {
 *       actor ! Clear // ensure the fixture will be cleaned up
 *     }
 *   }
 *
 *   describe("Testing") {
 *     it("should be easy") { actor =>
 *       actor ! Append("easy!")
 *       val futureString = actor ? GetValue
 *       futureString map { s =>
 *         assert(s == "ScalaTest is easy!")
 *       }
 *     }
 *
 *     it("should be fun") { actor =>
 *       actor ! Append("fun!")
 *       val futureString = actor ? GetValue
 *       futureString map { s =>
 *         assert(s == "ScalaTest is fun!")
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * If a test fails, the future returned by the <code>OneArgAsyncTest</code> function will result in
 * an [[org.scalatest.Failed org.scalatest.Failed]] wrapping the exception describing
 * the failure. To ensure clean up happens even if a test fails, you should invoke the test function and do the cleanup using
 * <code>complete</code>-<code>lastly</code>, as shown in the previous example. The <code>complete</code>-<code>lastly</code> syntax, defined in <code>CompleteLastly</code>, which is extended by <code>AsyncTestSuite</code>, ensures
 * the second, cleanup block of code is executed, whether the the first block throws an exception or returns a future. If it returns a
 * future, the cleanup will be executed when the future completes.
 * </p>
 *
 * <a name="sharingFixturesAcrossClasses"></a><h2>Sharing fixtures across classes</h2>
 *
 * <p>
 * If multiple test classes need the same fixture, you can define the <code>FixtureParam</code> and <code>withFixture(OneArgAsyncTest)</code>
 * implementations in a trait, then mix that trait into the test classes that need it. For example, if your application requires a database and your
 * integration tests use that database, you will likely have many test classes that need a database fixture. You can create a "database fixture" trait
 * that creates a database with a unique name, passes the connector into the test, then removes the database once the test completes. This is shown in
 * the following example:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.fixture.asyncfunspec.sharing
 *
 * import java.util.concurrent.ConcurrentHashMap
 * import org.scalatest._
 * import DbServer._
 * import java.util.UUID.randomUUID
 * import scala.concurrent.Future
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
 * trait DbFixture { this: FixtureAsyncTestSuite =&gt;
 *
 *   type FixtureParam = Db
 *
 *   // Allow clients to populate the database after
 *   // it is created
 *   def populateDb(db: Db) {}
 *
 *   def withFixture(test: OneArgAsyncTest): FutureOutcome = {
 *     val dbName = randomUUID.toString
 *     val db = createDb(dbName) // create the fixture
 *     complete {
 *       populateDb(db) // setup the fixture
 *       withFixture(test.toNoArgAsyncTest(db)) // "loan" the fixture to the test
 *     } lastly {
 *       removeDb(dbName) // ensure the fixture will be cleaned up
 *     }
 *   }
 * }
 *
 * class ExampleSpec extends funspec.FixtureAsyncFunSpec with DbFixture {
 *
 *   override def populateDb(db: Db) { // setup the fixture
 *     db.append("ScalaTest is ")
 *   }
 *
 *   describe("testing") {
 *     it("should be easy") { db =&gt;
 *       Future {
 *         db.append("easy!")
 *         assert(db.toString === "ScalaTest is easy!")
 *       }
 *     }
 *
 *     it("should be fun") { db =&gt;
 *       Future {
 *         db.append("fun!")
 *         assert(db.toString === "ScalaTest is fun!")
 *       }
 *     }
 *
 *     // This test doesn't need a Db
 *     it("code should be clear") { () =&gt;
 *       Future {
 *         val buf = new StringBuffer
 *         buf.append("ScalaTest code is ")
 *         buf.append("clear!")
 *         assert(buf.toString === "ScalaTest code is clear!")
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Often when you create fixtures in a trait like <code>DbFixture</code>, you'll still need to enable individual test classes
 * to "setup" a newly created fixture before it gets passed into the tests. A good way to accomplish this is to pass the newly
 * created fixture into a setup method, like <code>populateDb</code> in the previous example, before passing it to the test
 * function. Classes that need to perform such setup can override the method, as does <code>ExampleSuite</code>.
 * </p>
 *
 * <p>
 * If a test doesn't need the fixture, you can indicate that by providing a no-arg instead of a one-arg function, as is done in the
 * third test in the previous example, &ldquo;<code>test code should be clear</code>&rdquo;. In other words, instead of starting your function literal
 * with something like &ldquo;<code>db =&gt;</code>&rdquo;, you'd start it with &ldquo;<code>() =&gt;</code>&rdquo;. For such tests, <code>runTest</code>
 * will not invoke <code>withFixture(OneArgAsyncTest)</code>. It will instead directly invoke <code>withFixture(NoArgAsyncTest)</code>.
 * </p>
 *
 *
 * <p>
 * Both examples shown above demonstrate the technique of giving each test its own "fixture sandbox" to play in. When your fixtures
 * involve external side-effects, like creating files or databases, it is a good idea to give each file or database a unique name as is
 * done in these examples. This keeps tests completely isolated, allowing you to run them in parallel if desired. You could mix
 * <a href="../ParallelTestExecution.html"><code>ParallelTestExecution</code></a> into either of these <code>ExampleSuite</code> classes, and the tests would run in parallel just fine.
 * </p>
 *
 * @author Bill Venners
 */
abstract class FixtureAsyncFunSpec extends FixtureAsyncFunSpecLike {

  /**
   * Returns a user friendly string for this suite, composed of the
   * simple name of the class (possibly simplified further by removing dollar signs if added by the Scala interpeter) and, if this suite
   * contains nested suites, the result of invoking <code>toString</code> on each
   * of the nested suites, separated by commas and surrounded by parentheses.
   *
   * @return a user-friendly string for this suite
   */
  override def toString: String = org.scalatest.Suite.suiteToString(None, this)
}
