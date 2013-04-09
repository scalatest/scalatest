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
package org.scalatest.fixture

import scala.collection.immutable.ListSet
import org.scalatest.Suite.{IgnoreAnnotation, autoTagClassAnnotations}
import org.scalatest._
import Spec._
import Suite._
import org.scalatest.events.{TopOfClass, TopOfMethod}
import scala.reflect.NameTransformer._
import java.lang.reflect.{Method, Modifier, InvocationTargetException}

/**
 * A sister class to <code>org.scalatest.Spec</code> that can pass a fixture object into its tests.
 *
 * <table><tr><td class="usage">
 * <strong>Recommended Usage</strong>:
 * Use class <code>fixture.Spec</code> in situations for which <a href="../Spec.html"><code>Spec</code></a>
 * would be a good choice, when all or most tests need the same fixture objects
 * that must be cleaned up afterwords. <em>Note: <code>fixture.Spec</code> is intended for use in special situations, with class <code>Spec</code> used for general needs. For
 * more insight into where <code>fixture.Spec</code> fits in the big picture, see the <a href="../Spec.html#withFixtureOneArgTest"><code>withFixture(OneArgTest)</code></a> subsection of the <a href="../Spec.html#sharedFixtures">Shared fixtures</a> section in the documentation for class <code>Spec</code>.</em>
 * </td></tr></table>
 * 
 * <p>
 * Class <code>fixture.Spec</code> behaves similarly to class <code>org.scalatest.Spec</code>, except that tests may have a
 * fixture parameter. The type of the
 * fixture parameter is defined by the abstract <code>FixtureParam</code> type, which is a member of this class.
 * This class also has an abstract <code>withFixture</code> method. This <code>withFixture</code> method
 * takes a <code>OneArgTest</code>, which is a nested trait defined as a member of this class.
 * <code>OneArgTest</code> has an <code>apply</code> method that takes a <code>FixtureParam</code>.
 * This <code>apply</code> method is responsible for running a test.
 * This class's <code>runTest</code> method delegates the actual running of each test to <code>withFixture(OneArgTest)</code>, passing
 * in the test code to run via the <code>OneArgTest</code> argument. The <code>withFixture(OneArgTest)</code> method (abstract in this class) is responsible
 * for creating the fixture argument and passing it to the test function.
 * </p>
 * 
 * <p>
 * Subclasses of this class must, therefore, do three things differently from a plain old <code>org.scalatest.Spec</code>:
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
 * package org.scalatest.examples.spec.oneargtest
 * 
 * import org.scalatest.fixture
 * import java.io._
 * 
 * class ExampleSpec extends fixture.Spec {
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
 *   object &#96;Testing &#96; {
 *     def &#96;should be easy&#96; { f&#58; FixtureParam =&gt;
 *       f.writer.write("easy!")
 *       f.writer.flush()
 *       assert(f.file.length === 18)
 *     }
 * 
 *     def &#96;should be fun&#96; { f&#58; FixtureParam =&gt;
 *       f.writer.write("fun!")
 *       f.writer.flush()
 *       assert(f.file.length === 17)
 *     }
 *   } 
 * }
 * </pre>
 *
 * <p>
 * If a test fails, the <code>OneArgTest</code> function will complete abruptly with an exception describing the failure.
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
 * package org.scalatest.examples.fixture.spec.sharing
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
 * class ExampleSpec extends fixture.Spec with DbFixture {
 * 
 *   override def populateDb(db: Db) { // setup the fixture
 *     db.append("ScalaTest is ")
 *   }
 * 
 *   object &#96;Testing &#96; {
 *     def &#96;should be easy&#96; (db: Db) {
 *       db.append("easy!")
 *       assert(db.toString === "ScalaTest is easy!")
 *     }
 *     
 *     def &#96;should be fun&#96; (db: Db) {
 *       db.append("fun!")
 *       assert(db.toString === "ScalaTest is fun!")
 *     }
 *   }
 *   
 *   // This test doesn't need a Db
 *   object &#96;Test code&#96; {
 *     def &#96;should be clear&#96; {
 *       val buf = new StringBuffer
 *       buf.append("ScalaTest code is ")
 *       buf.append("clear!")
 *       assert(buf.toString === "ScalaTest code is clear!")
 *     }
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
 * If a test doesn't need the fixture, you can indicate that by leaving off the fixture parameter, as is done in the
 * third test in the previous example, &ldquo;<code>Test code should be clear</code>&rdquo;. For such methods, <code>runTest</code>
 * will not invoke <code>withFixture(OneArgTest)</code>. It will instead directly invoke <code>withFixture(NoArgTest)</code>.
 * </p>
 *
 * <p>
 * Both examples shown above demonstrate the technique of giving each test its own "fixture sandbox" to play in. When your fixtures
 * involve external side-effects, like creating files or databases, it is a good idea to give each file or database a unique name as is
 * done in these examples. This keeps tests completely isolated, allowing you to run them in parallel if desired. You could mix
 * <code>ParallelTestExecution</code> into either of these <code>ExampleSpec</code> classes, and the tests would run in parallel just fine.
 * </p>
 *
 * @author Bill Venners
 */
@Finders(Array("org.scalatest.finders.SpecFinder"))
abstract class Spec extends SpecLike {

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

private[scalatest] object Spec {
  
  def isTestMethod(m: Method): Boolean = {

    val isInstanceMethod = !Modifier.isStatic(m.getModifiers())

    val paramTypes = m.getParameterTypes
    val hasNoParamOrFixtureParam = paramTypes.isEmpty || paramTypes.length == 1

    // name must have at least one encoded space: "$u0220"
    val includesEncodedSpace = m.getName.indexOf("$u0020") >= 0
    
    val isOuterMethod = m.getName.endsWith("$$outer")
    
    val isNestedMethod = m.getName.matches(".+\\$\\$.+\\$[1-9]+")

    // def maybe(b: Boolean) = if (b) "" else "!"
    // println("m.getName: " + m.getName + ": " + maybe(isInstanceMethod) + "isInstanceMethod, " + maybe(hasNoParams) + "hasNoParams, " + maybe(includesEncodedSpace) + "includesEncodedSpace")
    isInstanceMethod && hasNoParamOrFixtureParam && includesEncodedSpace && !isOuterMethod && !isNestedMethod
  }
  
  import java.security.MessageDigest
  import scala.io.Codec
  
  // The following compactify code is written based on scala compiler source code at:-
  // https://github.com/scala/scala/blob/master/src/reflect/scala/reflect/internal/StdNames.scala#L47
  
  private val compactifiedMarker = "$$$$"
  
  def equalIfRequiredCompactify(value: String, compactified: String): Boolean = {
    if (compactified.matches(".+\\$\\$\\$\\$.+\\$\\$\\$\\$.+")) {
      val firstDolarIdx = compactified.indexOf("$$$$")
      val lastDolarIdx = compactified.lastIndexOf("$$$$")
      val prefix = compactified.substring(0, firstDolarIdx)
      val suffix = compactified.substring(lastDolarIdx + 4)
      val lastIndexOfDot = value.lastIndexOf(".")
      val toHash = 
        if (lastIndexOfDot >= 0) 
          value.substring(0, value.length - 1).substring(value.lastIndexOf(".") + 1)
        else
          value
          
      val bytes = Codec.toUTF8(toHash.toArray)
      val md5 = MessageDigest.getInstance("MD5")
      md5.update(bytes)
      val md5chars = (md5.digest() map (b => (b & 0xFF).toHexString)).mkString
      (prefix + compactifiedMarker + md5chars + compactifiedMarker + suffix) == compactified
    }
    else
      value == compactified
  }
}
