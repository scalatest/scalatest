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

import scala.collection.immutable.ListSet
import org.scalatest.Suite.{IgnoreTagName, autoTagClassAnnotations}
import org.scalatest._
import Spec._
import Suite._
import org.scalatest.events.{TopOfClass, TopOfMethod}
import scala.reflect.NameTransformer._
import java.lang.reflect.{Method, Modifier, InvocationTargetException}
import org.scalactic.source.SourceInfo

/**
 * <strong>Trait <code>fixture.SpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
 * <code>org.scalatest.fixture.FunSpec</code> instead.</strong>
 *
 * <p>
 * Because this style uses reflection at runtime to discover scopes and tests, it can only be supported on the JVM, not Scala.js.
 * Thus in ScalaTest 3.0.0, class <code>org.scalatest.SpecLike</code> was moved to the <code>org.scalatest.refspec</code> package and renamed
 * <code>RefSpecLike</code>, with the intention of later moving it to a separate module available only on the JVM. If the 
 * <code>org.scalatest.refspec._</code> package contained a <code>fixture</code> subpackage, then importing <code>org.scalatest.refspec._</code>
 * would import the name <code>fixture</code> as <code>org.scalatest.refspec.fixture</code>. This would likely be confusing for users,
 * who expect <code>fixture</code> to mean <code>org.scalatest.fixture</code>.
 * </p>
 *
 * <p>
 * As a result this class has been deprecated and will <em>not</em>
 * be moved to package <code>org.scalatest.refspec</code>. Instead we recommend you rewrite any test classes that currently extend
 * <code>org.scalatest.fixture.SpecLike</code> to extend <a href="FunSpecLike.html"><code>org.scalatest.fixture.FunSpecLike</code></a> instead,
 * replacing any scope <code>object</code>
 * with a <code>describe</code> clause, and any test method with an <code>it</code> clause.
 * </p>
 *
 * @author Bill Venners
 */
@Finders(Array("org.scalatest.finders.SpecFinder"))
@deprecated("fixture.SpecLike has been deprecated and will be removed in a future version of ScalaTest. Please use org.scalatest.fixture.FunSpecLike instead.")
trait SpecLike extends TestSuite with Informing with Notifying with Alerting with Documenting  { thisSuite => 

  private final val engine = new FixtureEngine[FixtureParam](Resources.concurrentSpecMod, "Spec")
  import engine._
  // Sychronized on thisSuite, only accessed from ensureScopesAndTestsRegistered
  private var scopesRegistered = false

  private def ensureScopesAndTestsRegistered() {

    thisSuite.synchronized {
      if (!scopesRegistered) {
        scopesRegistered = true
        def getMethod(o: AnyRef, testName: String) = { 
          val methodName = encode(simpleNameForTest(testName))
          val candidateMethods = o.getClass.getMethods.filter(_.getName == methodName)
          if (candidateMethods.size == 0)
            throw new IllegalArgumentException(Resources.testNotFound(testName))
          candidateMethods(0)
        }
        
        def getMethodTags(o: AnyRef, methodName: String) =
          for {
            a <- getMethod(o, methodName).getDeclaredAnnotations
            annotationClass = a.annotationType
            if annotationClass.isAnnotationPresent(classOf[TagAnnotation])
          } yield annotationClass.getName
          
        def getScopeClassName(o: AnyRef): String = {
          val className = o.getClass.getName
          if (className.endsWith("$"))
            className 
          else
            className + "$"
        }
          
        def isScopeMethod(o: AnyRef, m: Method): Boolean = {
          val scopeMethodName = getScopeClassName(o)+ m.getName + "$"
          
          val returnTypeName = m.getReturnType.getName
          
          equalIfRequiredCompactify(scopeMethodName, returnTypeName)
        }
        
        def getScopeDesc(m: Method): String = {
          val objName = m.getReturnType.getName
          val objClassName = decode(objName.substring(0, objName.length - 1))
          objClassName.substring(objClassName.lastIndexOf("$") + 1)
        }
        
        val testTags = tags
        object MethodNameEncodedOrdering extends Ordering[Method] {
          def compare(x: Method, y: Method): Int = {
            decode(x.getName) compareTo decode(y.getName)
          }
        }
        
        def register(o: AnyRef) {
          val testMethods = o.getClass.getMethods.filter(isTestMethod(_)).sorted(MethodNameEncodedOrdering)
          
// TODO: Detect duplicate test names, one with fixture param and one without.
          testMethods.foreach { m =>
            val scope = isScopeMethod(o, m)
            if (scope) {
              val scopeDesc = getScopeDesc(m)
              def scopeFun = {
                try {
                  val scopeObj = m.invoke(o)
                  register(scopeObj)
                }
                catch {
                  case ite: InvocationTargetException if ite.getTargetException != null =>
                    throw ite.getTargetException
                }
              }
              val scopeLocation = TopOfClass(m.getReturnType.getName)
              try {
                registerNestedBranch(scopeDesc, None, scopeFun, Resources.registrationAlreadyClosed, sourceFileName, "ensureScopesAndTestsRegistered", 2, 0, Some(scopeLocation))
              }
              catch {
                case e: exceptions.TestFailedException => throw new exceptions.NotAllowedException(FailureMessages.assertionShouldBePutInsideDefNotObject, Some(e), e => 8)
                case e: exceptions.TestCanceledException => throw new exceptions.NotAllowedException(FailureMessages.assertionShouldBePutInsideDefNotObject, Some(e), e => 8)
                case dtne: exceptions.DuplicateTestNameException => throw dtne
                case other: Throwable if (!Suite.anExceptionThatShouldCauseAnAbort(other)) => throw new exceptions.NotAllowedException(FailureMessages.exceptionWasThrownInObject(UnquotedString(other.getClass.getName), UnquotedString(scopeDesc)), Some(other), e => 8)
                case other: Throwable => throw other
              }
            }
            else {
              val methodName = m.getName
              val testName = 
                // if (m.getParameterTypes.length == 0)
                  decode(methodName)
                // else
                  // decode(methodName) + FixtureInParens
              val methodTags = getMethodTags(o, testName)
              val testFun: FixtureParam => Unit = (fixture: FixtureParam) => { 
                val anyRefFixture: AnyRef = fixture.asInstanceOf[AnyRef] // TODO zap this cast
                val argsArray: Array[Object] = 
                  if (m.getParameterTypes.length == 0)
                    Array.empty
                  else
                    Array(anyRefFixture)  
                try m.invoke(o, argsArray: _*)
                catch {
                  case ite: InvocationTargetException => 
                    throw ite.getTargetException
                }
              }
          
              val testLocation = TopOfMethod(getScopeClassName(o), m.toGenericString)
              val isIgnore = testTags.get(methodName) match {
                case Some(tagSet) => tagSet.contains(IgnoreTagName) || methodTags.contains(IgnoreTagName)
                case None => methodTags.contains(IgnoreTagName)
              }

              val sourceInfo = SourceInfo("NA", "NA", 0)

              if (isIgnore)
                registerIgnoredTest(testName, Transformer(testFun), Resources.registrationAlreadyClosed, sourceFileName, "ensureScopesAndTestsRegistered", 3, 0, Some(testLocation), sourceInfo, methodTags.map(new Tag(_)): _*)
              else
                registerTest(testName, Transformer(testFun), Resources.registrationAlreadyClosed, sourceFileName, "ensureScopesAndTestsRegistered", 2, 1, None, Some(testLocation), sourceInfo, None, methodTags.map(new Tag(_)): _*)
            }
          }
        }
     
        register(thisSuite)
      }
    }
  }

  // TODO: Probably make this private final val sourceFileName in a singleton object so it gets compiled in rather than carried around in each instance
  private[scalatest] val sourceFileName = "SpecLike.scala"

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
   * <code>Spec</code> is being executed, such as from inside a test function, it will forward the information to
   * the current reporter immediately. If invoked at any other time, it will
   * print to the standard output. This method can be called safely by any thread.
   */
  protected def note: Notifier = atomicNotifier.get

  /**
   * Returns an <code>Alerter</code> that during test execution will forward strings (and other objects) passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>Spec</code> is being executed, such as from inside a test function, it will forward the information to
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
  
  /**
   * An immutable <code>Set</code> of test names. If this <code>Spec</code> contains no tests, this method returns an
   * empty <code>Set</code>.
   *
   * <p>
   * This trait's implementation of this method will return a set that contains the names of all registered tests. The set's
   * iterator will return those names in the order in which the tests were registered. Each test's name is composed
   * of the concatenation of the name of each surrounding <em>scope object</em>, in order from outside in, and the name of the
   * test method itself, with all components separated by a space. For example, consider this <code>Spec</code>:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest.refspec.RefSpec
   *
   * class StackSpec extends RefSpec {
   *   object &#96;A Stack&#96; {
   *     object &#96;(when not empty)&#96; {
   *       def &#96;must allow me to pop&#96; {}
   *     }
   *     object &#96;(when not full)&#96; {
   *       def &#96;must allow me to push&#96; {}
   *     }
   *   }
   * }
   * </pre>
   *
   * <p>
   * Invoking <code>testNames</code> on this <code>Spec</code> will yield a set that contains the following
   * two test name strings:
   * </p>
   *
   * <pre class="stExamples">
   * "A Stack (when not empty) must allow me to pop"
   * "A Stack (when not full) must allow me to push"
   * </pre>
   *
   * <p>
   * This trait's implementation of this method will first ensure that the discovery of scope objects and test methods
   * has been performed.
   * </p>
   *
   * @return the <code>Set</code> of test names
   */
  override def testNames: Set[String] = {
    ensureScopesAndTestsRegistered()
    // I'm returning a ListSet here so that they tests will be run in registration order
    ListSet(atomic.get.testNamesList.toArray: _*)
  }
  
  /**
   * Run a test. This trait's implementation runs the test registered with the name specified by
   * <code>testName</code>. Each test's name is a concatenation of the text of all <em>scope objects</em> surrounding a test,
   * from outside in, and the test method's name, with one space placed between each item. (See the documentation
   * for <code>testNames</code> for an example.)
   *
   * <p>
   * This trait's implementation of this method will first ensure that the discovery of scope objects and test methods
   * has been performed.
   * </p>
   *
   * @param testName the name of one test to execute.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
   * @throws NullArgumentException if <code>testName</code> or <code>args</code> is <code>null</code>.
   */
  protected override def runTest(testName: String, args: Args): Status = {

    ensureScopesAndTestsRegistered()

    def invokeWithFixture(theTest: TestLeaf): Outcome = {
      val theConfigMap = args.configMap
      val testData = testDataFor(testName, theConfigMap)
      withFixture(
        new OneArgTest {
          val name = testData.name
          def apply(fixture: FixtureParam): Outcome = { theTest.testFun(fixture) }
          val configMap = testData.configMap
          val scopes = testData.scopes
          val text = testData.text
          val tags = testData.tags
          val sourceInfo = testData.sourceInfo
        }
        //new TestFunAndConfigMap(testName, theTest.testFun, theConfigMap)
      )
    }

    runTestImpl(thisSuite, testName, args, true, invokeWithFixture)
  }
  
  /**
   * The total number of tests that are expected to run when this <code>Spec</code>'s <code>run</code> method is invoked.
   *
   * <p>
   * This trait's implementation of this method returns the sum of:
   * </p>
   *
   * <ul>
   * <li>the size of the <code>testNames</code> <code>List</code>, minus the number of tests marked as ignored and
   * any tests that are exluded by the passed <code>Filter</code></li>
   * <li>the sum of the values obtained by invoking
   *     <code>expectedTestCount</code> on every nested <code>Suite</code> contained in
   *     <code>nestedSuites</code></li>
   * </ul>
   *
   * <p>
   * This trait's implementation of this method will first ensure that the discovery of scope objects and test methods
   * has been performed.
   * </p>
   *
   * @param filter a <code>Filter</code> with which to filter tests to count based on their tags
   * @return the expected number test count
   */
  final override def expectedTestCount(filter: Filter): Int = {
    ensureScopesAndTestsRegistered()
    super.expectedTestCount(filter)
  }

  /**
   * A <code>Map</code> whose keys are <code>String</code> tag names to which tests in this <code>Spec</code> belong, and values
   * the <code>Set</code> of test names that belong to each tag. If this <code>Spec</code> contains no tags, this method returns an empty <code>Map</code>.
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
   *
   * <p>
   * This trait's implementation of this method will first ensure that the discovery of scope objects and test methods
   * has been performed.
   * </p>
   */
  override def tags: Map[String, Set[String]] = {
    ensureScopesAndTestsRegistered()
    autoTagClassAnnotations(atomic.get.tagsMap, this)
  }

  /**
   * Run zero to many of this <code>Spec</code>'s tests.
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Spec</code>.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when all tests started by this method have completed, and whether or not a failure occurred.
   * @throws NullArgumentException if any of the passed parameters is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Spec</code>
   */
  protected override def runTests(testName: Option[String], args: Args): Status = {
    ensureScopesAndTestsRegistered()
    runTestsImpl(thisSuite, testName, args, info, true, runTest)
  }

  /**
   * Runs this <code>fixture.Spec</code>.
   *
   * <p>If <code>testName</code> is <code>None</code>, this trait's implementation of this method
   * calls these two methods on this object in this order:</p>
   *
   * <ol>
   * <li><code>runNestedSuites(report, stopper, tagsToInclude, tagsToExclude, configMap, distributor)</code></li>
   * <li><code>runTests(testName, report, stopper, tagsToInclude, tagsToExclude, configMap)</code></li>
   * </ol>
   *
   * <p>
   * If <code>testName</code> is defined, then this trait's implementation of this method
   * calls <code>runTests</code>, but does not call <code>runNestedSuites</code>. This behavior
   * is part of the contract of this method. Subclasses that override <code>run</code> must take
   * care not to call <code>runNestedSuites</code> if <code>testName</code> is defined. (The
   * <code>OneInstancePerTest</code> trait depends on this behavior, for example.)
   * </p>
   *
   * <p>
   * This trait's implementation of this method will first ensure that the discovery of scope objects and test methods
   * has been performed.
   * </p>
   *
   *
   * @param testName an optional name of one test to run. If <code>None</code>, all relevant tests should be run.
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>fixture.SpecLike</code>.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when all tests and nested suites started by this method have completed, and whether or not a failure occurred.
   * @throws NullArgumentException if any passed parameter is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  override def run(testName: Option[String], args: Args): Status = {
    ensureScopesAndTestsRegistered()
    runImpl(thisSuite, testName, args, super.run)
  }
  
  /**
   * Suite style name.
   *
   * @return <code>org.scalatest.fixture.Spec</code>
   */
  final override val styleName: String = "org.scalatest.fixture.Spec"

  override def testDataFor(testName: String, theConfigMap: ConfigMap = ConfigMap.empty): TestData = createTestDataFor(testName, theConfigMap, this)
}
