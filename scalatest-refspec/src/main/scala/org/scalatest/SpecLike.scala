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
package org.scalatest

import scala.collection.immutable.ListSet
import Suite._
import Spec.isTestMethod
import Spec.equalIfRequiredCompactify
import org.scalatest.events._
import scala.reflect.NameTransformer._
import java.lang.reflect.{Method, Modifier, InvocationTargetException}
import org.scalactic.Requirements._

/**
 * Implementation trait for class <code>Spec</code>, which facilitates a &ldquo;behavior-driven&rdquo; style of development (BDD), in which tests
 * are methods, optionally nested inside singleton objects defining textual scopes.
 *
 * <p>
 * <a href="Spec.html"><code>Spec</code></a> is a class, not a trait, to minimize compile time given there is a slight compiler overhead to
 * mixing in traits compared to extending classes. If you need to mix the behavior of <code>Spec</code>
 * into some other class, you can use this trait instead, because class <code>Spec</code> does nothing more than extend this trait and add a nice <code>toString</code> implementation.
 * </p>
 *
 * <p>
 * See the documentation of the class for a <a href="Spec.html">detailed overview of <code>Spec</code></a>.
 * </p>
 *
 * @author Bill Venners
 */
@Finders(Array("org.scalatest.finders.SpecFinder"))
@deprecated("Please use RefSpecLike instead.")
trait SpecLike extends Suite with Informing with Notifying with Alerting with Documenting { thisSuite =>

  private final val engine = new Engine(Resources.concurrentSpecMod, "SpecLike")
  import engine._
  // Sychronized on thisSuite, only accessed from ensureScopesAndTestsRegistered
  private var scopesRegistered = false

  private def ensureScopesAndTestsRegistered() {

    thisSuite.synchronized {
      if (!scopesRegistered) {
        scopesRegistered = true
        def getMethod(o: AnyRef, methodName: String) = {
          o.getClass.getMethod(
            simpleNameForTest(methodName),
            new Array[Class[_]](0): _*
          )
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
                case other: Throwable if (!Suite.anExceptionThatShouldCauseAnAbort(other)) => throw new exceptions.NotAllowedException(FailureMessages.exceptionWasThrownInObject(UnquotedString(other.getClass.getName), UnquotedString(scopeDesc)), Some(other), e => 8)
                case other: Throwable => throw other
              }
            }
            else {
              val methodName = m.getName
              val testName = decode(methodName)
              val methodTags = getMethodTags(o, methodName)
              val testFun: () => Unit = () => {
                val argsArray: Array[Object] = Array.empty
                try m.invoke(o, argsArray: _*)
                catch {
                  case ite: InvocationTargetException =>
                    throw ite.getTargetException
                }
              }

              val testLocation = TopOfMethod(getScopeClassName(o), m.toGenericString)
              val isIgnore = testTags.get(methodName) match {
                case Some(tagSet) => tagSet.contains(Suite.IgnoreAnnotation) || methodTags.contains(Suite.IgnoreAnnotation)
                case None => methodTags.contains(Suite.IgnoreAnnotation)
              }
              if (isIgnore)
                registerIgnoredTest(testName, Transformer(testFun), Resources.registrationAlreadyClosed, sourceFileName, "ensureScopesAndTestsRegistered", 3, 0, Some(testLocation), methodTags.map(new Tag(_)): _*)
              else
                registerTest(testName, Transformer(testFun), Resources.registrationAlreadyClosed, sourceFileName, "ensureScopesAndTestsRegistered", 2, 1, None, Some(testLocation), None, methodTags.map(new Tag(_)): _*)
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
   * of the concatenation of the text of each surrounding describer, in order from outside in, and the text of the
   * example itself, with all components separated by a space. For example, consider this <code>Spec</code>:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest.Spec
   *
   * class StackSpec extends Spec {
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
   */
  override def testNames: Set[String] = {
    ensureScopesAndTestsRegistered()
    // I'm returning a ListSet here so that they tests will be run in registration order
    ListSet(atomic.get.testNamesList.toArray: _*)
  }

  /**
   * Run a test. This trait's implementation runs the test registered with the name specified by
   * <code>testName</code>. Each test's name is a concatenation of the text of all describers surrounding a test,
   * from outside in, and the test's  spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.)
   *
   * @param testName the name of one test to execute.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
   *
   * @throws NullPointerException if any of <code>testName</code>, <code>reporter</code>, <code>stopper</code>, or <code>configMap</code>
   *     is <code>null</code>.
   */
  protected override def runTest(testName: String, args: Args): Status = {
    requireNonNull(testName, args)

    ensureScopesAndTestsRegistered()

    def invokeWithFixture(theTest: TestLeaf): Outcome = {
      val theConfigMap = args.configMap
      val testData = testDataFor(testName, theConfigMap)
      withFixture(
        new NoArgTest {
          val name = testData.name
          def apply(): Outcome = { theTest.testFun() }
          val configMap = testData.configMap
          val scopes = testData.scopes
          val text = testData.text
          val tags = testData.tags
        }
      )
    }

    runTestImpl(thisSuite, testName, args, true, invokeWithFixture)
  }


  final override def expectedTestCount(filter: Filter): Int = {
    ensureScopesAndTestsRegistered()
    super.expectedTestCount(filter)
  }


  /**
   * A <code>Map</code> whose keys are <code>String</code> names of tagged tests and whose associated values are
   * the <code>Set</code> of tags for the test. If this <code>Spec</code> contains no tags, this method returns an empty <code>Map</code>.
   *
   * <p>
   * This trait's implementation of this method uses Java reflection to discover any Java annotations attached to its test methods. The
   * fully qualified name of each unique annotation that extends <code>TagAnnotation</code> is considered a tag. This trait's
   * implementation of this method, therefore, places one key/value pair into to the
   * <code>Map</code> for each test for which a tag annotation is discovered through reflection.
   * </p>
   *
   * <p>
   * In addition, this trait's implementation will also auto-tag tests with class level annotations.  
   * For example, if you annotate <code>@Ignore</code> at the class level, all test methods in the class will be auto-annotated with
   * <code>org.scalatest.Ignore</code>.
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
   *                 I.e., <code>None</code> acts like a wildcard that means run all relevant tests in this <code>Suite</code>.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when all tests started by this method have completed, and whether or not a failure occurred.
   *
   * @throws NullPointerException if any of the passed parameters is <code>null</code>.
   * @throws IllegalArgumentException if <code>testName</code> is defined, but no test with the specified test name
   *     exists in this <code>Suite</code>
   */
  protected override def runTests(testName: Option[String], args: Args): Status = {
    ensureScopesAndTestsRegistered()
    runTestsImpl(thisSuite, testName, args, info, true, runTest)
  }

  override def run(testName: Option[String], args: Args): Status = {
    ensureScopesAndTestsRegistered()
    runImpl(thisSuite, testName, args, super.run)
  }

  /**
   * Suite style name.
   */
  final override val styleName: String = "org.scalatest.Spec"

  override def testDataFor(testName: String, theConfigMap: ConfigMap = ConfigMap.empty): TestData = createTestDataFor(testName, theConfigMap, this)
}

