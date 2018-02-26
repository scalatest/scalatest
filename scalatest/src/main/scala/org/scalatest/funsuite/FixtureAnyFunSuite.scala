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
package org.scalatest.funsuite

import org.scalatest._
import org.scalactic.source
import org.scalatest.Suite.autoTagClassAnnotations

/**
  * A sister class to <code>org.scalatest.funsuite.AnyFunSuite</code> that can pass a fixture object into its tests.
  *
  * <table><tr><td class="usage">
  * <strong>Recommended Usage</strong>:
  * Use class <code>fixture.FunSuite</code> in situations for which <a href="../FunSuite.html"><code>FunSuite</code></a>
  * would be a good choice, when all or most tests need the same fixture objects
  * that must be cleaned up afterwards. <em>Note: <code>fixture.FunSuite</code> is intended for use in special situations, with class <code>FunSuite</code> used for general needs. For
  * more insight into where <code>fixture.FunSuite</code> fits in the big picture, see the <a href="../FunSuite.html#withFixtureOneArgTest"><code>withFixture(OneArgTest)</code></a> subsection of the <a href="../FunSuite.html#sharedFixtures">Shared fixtures</a> section in the documentation for class <code>FunSuite</code>.</em>
  * </td></tr></table>
  *
  * <p>
  * Class <code>fixture.FunSuite</code> behaves similarly to class <code>org.scalatest.FunSuite</code>, except that tests may have a
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
  * Subclasses of this class must, therefore, do three things differently from a plain old <code>org.scalatest.FunSuite</code>:
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
  * package org.scalatest.examples.funsuite.oneargtest
  *
  * import org.scalatest.fixture
  * import java.io._
  *
  * class ExampleSuite extends fixture.FunSuite {
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
  *   test("testing should be easy") { f =&gt;
  *     f.writer.write("easy!")
  *     f.writer.flush()
  *     assert(f.file.length === 18)
  *   }
  *
  *   test("testing should be fun") { f =&gt;
  *     f.writer.write("fun!")
  *     f.writer.flush()
  *     assert(f.file.length === 17)
  *   }
  * }
  * </pre>
  *
  * <p>
  * If a test fails, the <code>OneArgTest</code> function will result in a [[org.scalatest.Failed Failed]] wrapping the exception describing the failure.
  * To ensure clean up happens even if a test fails, you should invoke the test function from inside a <code>try</code> block and do the cleanup in a
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
  * package org.scalatest.examples.fixture.funsuite.sharing
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
  * class ExampleSuite extends fixture.FunSuite with DbFixture {
  *
  *   override def populateDb(db: Db) { // setup the fixture
  *     db.append("ScalaTest is ")
  *   }
  *
  *   test("testing should be easy") { db =&gt;
  *     db.append("easy!")
  *     assert(db.toString === "ScalaTest is easy!")
  *   }
  *
  *   test("testing should be fun") { db =&gt;
  *     db.append("fun!")
  *     assert(db.toString === "ScalaTest is fun!")
  *   }
  *
  *   // This test doesn't need a Db
  *   test("test code should be clear") { () =&gt;
  *     val buf = new StringBuffer
  *     buf.append("ScalaTest code is ")
  *     buf.append("clear!")
  *     assert(buf.toString === "ScalaTest code is clear!")
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
  * will not invoke <code>withFixture(OneArgTest)</code>. It will instead directly invoke <code>withFixture(NoArgTest)</code>.
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
//SCALATESTJS-ONLY @scala.scalajs.reflect.annotation.EnableReflectiveInstantiation
@Finders(Array("org.scalatest.finders.FunSuiteFinder"))
trait FixtureAnyFunSuite extends fixture.TestSuite with fixture.TestRegistration with Informing with Notifying with Alerting with Documenting { thisSuite =>

  private final val engine = new FixtureEngine[FixtureParam](Resources.concurrentFixtureFunSuiteMod, "FixtureFunSuite")

  import engine._

  private[scalatest] val sourceFileName = "FixtureAnyFunSuite.scala"

  /**
    * Returns an <code>Informer</code> that during test execution will forward strings passed to its
    * <code>apply</code> method to the current reporter. If invoked in a constructor, it
    * will register the passed string for forwarding later during test execution. If invoked from inside a scope,
    * it will forward the information to the current reporter immediately.  If invoked from inside a test function,
    * it will record the information and forward it to the current reporter only after the test completed, as <code>recordedEvents</code>
    * of the test completed event, such as <code>TestSucceeded</code>. If invoked at any other time, it will print to the standard output.
    * This method can be called safely by any thread.
    */
  protected def info: Informer = atomicInformer.get

  /**
    * Returns a <code>Notifier</code> that during test execution will forward strings (and other objects) passed to its
    * <code>apply</code> method to the current reporter. If invoked in a constructor, it
    * will register the passed string for forwarding later during test execution. If invoked while this
    * <code>fixture.FunSuite</code> is being executed, such as from inside a test function, it will forward the information to
    * the current reporter immediately. If invoked at any other time, it will
    * print to the standard output. This method can be called safely by any thread.
    */
  protected def note: Notifier = atomicNotifier.get

  /**
    * Returns an <code>Alerter</code> that during test execution will forward strings (and other objects) passed to its
    * <code>apply</code> method to the current reporter. If invoked in a constructor, it
    * will register the passed string for forwarding later during test execution. If invoked while this
    * <code>fixture.FunSuite</code> is being executed, such as from inside a test function, it will forward the information to
    * the current reporter immediately. If invoked at any other time, it will
    * print to the standard output. This method can be called safely by any thread.
    */
  protected def alert: Alerter = atomicAlerter.get

  /**
    * Returns a <code>Documenter</code> that during test execution will forward strings passed to its
    * <code>apply</code> method to the current reporter. If invoked in a constructor, it
    * will register the passed string for forwarding later during test execution. If invoked from inside a scope,
    * it will forward the information to the current reporter immediately.  If invoked from inside a test function,
    * it will record the information and forward it to the current reporter only after the test completed, as <code>recordedEvents</code>
    * of the test completed event, such as <code>TestSucceeded</code>. If invoked at any other time, it will print to the standard output.
    * This method can be called safely by any thread.
    */
  protected def markup: Documenter = atomicDocumenter.get

  final def registerTest(testText: String, testTags: Tag*)(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {
    // SKIP-SCALATESTJS,NATIVE-START
    val stackDepthAdjustment = -1
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -4
    engine.registerTest(testText, fixture.Transformer(testFun), Resources.testCannotBeNestedInsideAnotherTest, "FunSuite.scala", "registerTest", 4, stackDepthAdjustment, None, None, Some(pos), None, testTags: _*)
  }

  final def registerIgnoredTest(testText: String, testTags: Tag*)(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {
    // SKIP-SCALATESTJS,NATIVE-START
    val stackDepthAdjustment = -4
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -5
    engine.registerIgnoredTest(testText, fixture.Transformer(testFun), Resources.testCannotBeNestedInsideAnotherTest, "FunSuite.scala", "registerIgnoredTest", 4, stackDepthAdjustment, None, Some(pos), testTags: _*)
  }

  class ResultOfTestInvocation(testName: String, testTags: Tag*) {
    def apply(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      // SKIP-SCALATESTJS,NATIVE-START
      val stackDepth = 3
      val stackDepthAdjustment = -2
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val stackDepth = 5
      //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -5
      engine.registerTest(testName, fixture.Transformer(testFun), Resources.testCannotAppearInsideAnotherTest, sourceFileName, "apply", stackDepth, stackDepthAdjustment, None, None, Some(pos), None, testTags: _*)
    }

    def apply(testFun: () => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      // SKIP-SCALATESTJS,NATIVE-START
      val stackDepth = 3
      val stackDepthAdjustment = -2
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val stackDepth = 6
      //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -6
      engine.registerTest(testName, fixture.Transformer(new fixture.NoArgTestWrapper(testFun)), Resources.testCannotAppearInsideAnotherTest, sourceFileName, "apply", stackDepth, stackDepthAdjustment, None, None, Some(pos), None, testTags: _*)
    }
  }

  /**
    * Register a test with the specified name, optional tags, and function value that takes no arguments.
    * This method will register the test for later execution via an invocation of one of the <code>run</code>
    * methods. The passed test name must not have been registered previously on
    * this <code>FunSuite</code> instance.
    *
    * @param testName the name of the test
    * @param testTags the optional list of tags for this test
    * @param testFun the test function
    * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
    * @throws DuplicateTestNameException if a test with the same name has been registered previously
    * @throws NotAllowedException if <code>testName</code> had been registered previously
    * @throws NullArgumentException if <code>testName</code> or any passed test tag is <code>null</code>
    */
  protected def test(testName: String, testTags: Tag*): ResultOfTestInvocation = new ResultOfTestInvocation(testName, testTags: _*)
  /*
    protected def test(testName: String, testTags: Tag*)(testFun: FixtureParam => Any /* Assertion */) {
      // SKIP-SCALATESTJS,NATIVE-START
      val stackDepth = 4
      val stackDepthAdjustment = -2
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val stackDepth = 6
      //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -6
      engine.registerTest(testName, Transformer(testFun), Resources.testCannotAppearInsideAnotherTest, sourceFileName, "test", stackDepth, stackDepthAdjustment, None, None, None, testTags: _*)
    }
  */

  class ResultOfIgnoreInvocation(testName: String, testTags: Tag*) {
    def apply(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      // SKIP-SCALATESTJS,NATIVE-START
      val stackDepth = 3
      val stackDepthAdjustment = -3
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val stackDepth = 5
      //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -6
      engine.registerIgnoredTest(testName, fixture.Transformer(testFun), Resources.ignoreCannotAppearInsideATest, sourceFileName, "apply", stackDepth, stackDepthAdjustment, None, Some(pos), testTags: _*)
    }

    def apply(testFun: () => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      // SKIP-SCALATESTJS,NATIVE-START
      val stackDepth = 3
      val stackDepthAdjustment = -3
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val stackDepth = 5
      //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -6
      engine.registerIgnoredTest(testName, fixture.Transformer(new fixture.NoArgTestWrapper(testFun)), Resources.ignoreCannotAppearInsideATest, sourceFileName, "apply", stackDepth, stackDepthAdjustment, None, Some(pos), testTags: _*)
    }
  }

  /**
    * Register a test to ignore, which has the specified name, optional tags, and function value that takes no arguments.
    * This method will register the test for later ignoring via an invocation of one of the <code>run</code>
    * methods. This method exists to make it easy to ignore an existing test by changing the call to <code>test</code>
    * to <code>ignore</code> without deleting or commenting out the actual test code. The test will not be run, but a
    * report will be sent that indicates the test was ignored. The passed test name must not have been registered previously on
    * this <code>FunSuite</code> instance.
    *
    * @param testName the name of the test
    * @param testTags the optional list of tags for this test
    * @param testFun the test function
    * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
    * @throws DuplicateTestNameException if a test with the same name has been registered previously
    * @throws NotAllowedException if <code>testName</code> had been registered previously
    */
  protected def ignore(testName: String, testTags: Tag*): ResultOfIgnoreInvocation = new ResultOfIgnoreInvocation(testName, testTags: _*)
  /*
    protected def ignore(testName: String, testTags: Tag*)(testFun: FixtureParam => Any /* Assertion */) {
      // SKIP-SCALATESTJS,NATIVE-START
      val stackDepth = 4
      val stackDepthAdjustment = -3
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val stackDepth = 6
      //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -7
      engine.registerIgnoredTest(testName, Transformer(testFun), Resources.ignoreCannotAppearInsideATest, sourceFileName, "ignore", stackDepth, stackDepthAdjustment, None, testTags: _*)
    }
  */

  /**
    * An immutable <code>Set</code> of test names. If this <code>fixture.FunSuite</code> contains no tests, this method returns an empty <code>Set</code>.
    *
    * <p>
    * This trait's implementation of this method will return a set that contains the names of all registered tests. The set's iterator will
    * return those names in the order in which the tests were registered.
    * </p>
    *
    * @return the <code>Set</code> of test names
    */
  override def testNames: Set[String] = {
    InsertionOrderSet(atomic.get.testNamesList)
  }

  /**
    * Run a test. This trait's implementation runs the test registered with the name specified by <code>testName</code>.
    *
    * @param testName the name of one test to run.
    * @param args the <code>Args</code> for this run
    * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
    * @throws IllegalArgumentException if <code>testName</code> is defined but a test with that name does not exist on this <code>fixture.FunSuite</code>
    * @throws NullArgumentException if <code>testName</code> or <code>args</code> is <code>null</code>.
    */
  protected override def runTest(testName: String, args: Args): Status = {

    def invokeWithFixture(theTest: TestLeaf): Outcome = {
      theTest.testFun match {
        case transformer: org.scalatest.fixture.Transformer[_] =>
          transformer.exceptionalTestFun match {
            case wrapper: fixture.NoArgTestWrapper[_, _] =>
              withFixture(new FixturelessTestFunAndConfigMap(testName, wrapper.test, args.configMap))
            case fun => withFixture(new TestFunAndConfigMap(testName, fun, args.configMap))
          }
        case other =>
          other match {
            case wrapper: fixture.NoArgTestWrapper[_, _] =>
              withFixture(new FixturelessTestFunAndConfigMap(testName, wrapper.test, args.configMap))
            case fun => withFixture(new TestFunAndConfigMap(testName, fun, args.configMap))
          }
      }
    }

    runTestImpl(thisSuite, testName, args, true, invokeWithFixture)
  }

  /**
    * A <code>Map</code> whose keys are <code>String</code> tag names to which tests in this <code>fixture.FunSuite</code> belong, and values
    * the <code>Set</code> of test names that belong to each tag. If this <code>fixture.FunSuite</code> contains no tags, this method returns an empty
    * <code>Map</code>.
    *
    * <p>
    * This trait's implementation returns tags that were passed as strings contained in <code>Tag</code> objects passed to
    * methods <code>test</code> and <code>ignore</code>.
    * </p>
    *
    * <p>
    * In addition, this trait's implementation will also auto-tag tests with class level annotations.
    * For example, if you annotate @Ignore at the class level, all test methods in the class will be auto-annotated with @Ignore.
    * </p>
    */
  override def tags: Map[String, Set[String]] = autoTagClassAnnotations(atomic.get.tagsMap, this)

  /**
    * <p>
    * Run zero to many of this <code>fixture.FunSuiteLike</code>'s tests.
    * </p>
    *
    * <p>
    * This method takes a <code>testName</code> parameter that optionally specifies a test to invoke.
    * If <code>testName</code> is <code>Some</code>, this trait's implementation of this method
    * invokes <code>runTest</code> on this object with passed <code>args</code>.
    * </p>
    *
    * <p>
    * This method takes an <code>args</code> that contains a <code>Set</code> of tag names that should be included (<code>tagsToInclude</code>), and a <code>Set</code>
    * that should be excluded (<code>tagsToExclude</code>), when deciding which of this <code>Suite</code>'s tests to execute.
    * If <code>tagsToInclude</code> is empty, all tests will be executed
    * except those those belonging to tags listed in the <code>tagsToExclude</code> <code>Set</code>. If <code>tagsToInclude</code> is non-empty, only tests
    * belonging to tags mentioned in <code>tagsToInclude</code>, and not mentioned in <code>tagsToExclude</code>
    * will be executed. However, if <code>testName</code> is <code>Some</code>, <code>tagsToInclude</code> and <code>tagsToExclude</code> are essentially ignored.
    * Only if <code>testName</code> is <code>None</code> will <code>tagsToInclude</code> and <code>tagsToExclude</code> be consulted to
    * determine which of the tests named in the <code>testNames</code> <code>Set</code> should be run. For more information on trait tags, see the main documentation for this trait.
    * </p>
    *
    * <p>
    * If <code>testName</code> is <code>None</code>, this trait's implementation of this method
    * invokes <code>testNames</code> on this <code>Suite</code> to get a <code>Set</code> of names of tests to potentially execute.
    * (A <code>testNames</code> value of <code>None</code> essentially acts as a wildcard that means all tests in
    * this <code>Suite</code> that are selected by <code>tagsToInclude</code> and <code>tagsToExclude</code> should be executed.)
    * For each test in the <code>testName</code> <code>Set</code>, in the order
    * they appear in the iterator obtained by invoking the <code>elements</code> method on the <code>Set</code>, this trait's implementation
    * of this method checks whether the test should be run based on the <code>tagsToInclude</code> and <code>tagsToExclude</code> <code>Set</code>s.
    * If so, this implementation invokes <code>runTest</code> with passed <code>args</code>.
    * </p>
    *
    * @param testName an optional name of one test to execute. If <code>None</code>, all relevant tests should be executed.
    *                 I.e., <code>None</code> acts like a wildcard that means execute all relevant tests in this <code>FunSpec</code>.
    * @param args the <code>Args</code> to which results will be reported
    * @return a <code>Status</code> object that indicates when all tests started by this method have completed, and whether or not a failure occurred.
    * @throws NullArgumentException if any of <code>testName</code> or <code>args</code> is <code>null</code>.
    */
  protected override def runTests(testName: Option[String], args: Args): Status = {
    runTestsImpl(thisSuite, testName, args, info, true, runTest)
  }

  override def run(testName: Option[String], args: Args): Status = {
    runImpl(thisSuite, testName, args: Args, super.run)
  }

  /**
    * Registers shared tests.
    *
    * <p>
    * This method enables the following syntax for shared tests in a <code>fixture.FunSuite</code>:
    * </p>
    *
    * <pre class="stHighlight">
    * testsFor(nonEmptyStack(lastValuePushed))
    * </pre>
    *
    * <p>
    * This method just provides syntax sugar intended to make the intent of the code clearer.
    * Because the parameter passed to it is
    * type <code>Unit</code>, the expression will be evaluated before being passed, which
    * is sufficient to register the shared tests. For examples of shared tests, see the
    * <a href="../FunSuite.html#SharedTests">Shared tests section</a> in the main documentation for
    * trait <code>FunSuite</code>.
    * </p>
    */
  protected def testsFor(unit: Unit): Unit = {}

  import scala.language.implicitConversions

  /**
    * Implicitly converts a function that takes no parameters and results in <code>PendingStatement</code> to
    * a function from <code>FixtureParam</code> to <code>Any</code>, to enable pending tests to registered as by-name parameters
    * by methods that require a test function that takes a <code>FixtureParam</code>.
    *
    * <p>
    * This method makes it possible to write pending tests as simply <code>(pending)</code>, without needing
    * to write <code>(fixture => pending)</code>.
    * </p>
    *
    * @param f a function
    * @return a function of <code>FixtureParam => Any</code>
    */
  protected implicit def convertPendingToFixtureFunction(f: => PendingStatement): (FixtureParam => Any /* Assertion */) = {
    fixture => { f; Succeeded }
  }

  /**
    * Implicitly converts a function that takes no parameters and results in <code>Any</code> to
    * a function from <code>FixtureParam</code> to <code>Any</code>, to enable no-arg tests to registered
    * by methods that require a test function that takes a <code>FixtureParam</code>.
    *
    * @param fun a function
    * @return a function of <code>FixtureParam => Any</code>
    */
  /*
    protected implicit def convertNoArgToFixtureFunction(fun: () => Any /* Assertion */): (FixtureParam => Any /* Assertion */) =
      new NoArgTestWrapper(fun)
  */

  /**
    * Suite style name.
    *
    * @return <code>org.scalatest.fixture.FunSuite</code>
    */
  final override val styleName: String = oldStyleName.getOrElse("org.scalatest.funsuite.FixtureAnyFunSuite")

  override def testDataFor(testName: String, theConfigMap: ConfigMap = ConfigMap.empty): TestData = createTestDataFor(testName, theConfigMap, this)
}
