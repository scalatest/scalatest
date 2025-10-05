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
package org.scalatest.flatspec

import org.scalactic.{source, Prettifier}
import org.scalatest._
import Suite.anExceptionThatShouldCauseAnAbort
import Suite.autoTagClassAnnotations
import java.util.ConcurrentModificationException
import java.util.concurrent.atomic.AtomicReference
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepth
import verbs.{ResultOfTaggedAsInvocation, ResultOfStringPassedToVerb, BehaveWord, ShouldVerb, MustVerb, CanVerb, StringVerbStringInvocation, StringVerbBehaveLikeInvocation}

/**
 * Implementation trait for class <code>AnyFlatSpec</code>, which facilitates a
 * &ldquo;behavior-driven&rdquo; style of development (BDD), in which tests
 * are combined with text that specifies the behavior the tests verify.
 *
 * <p>
 * <a href="AnyFlatSpec.html"><code>AnyFlatSpec</code></a> is a class, not a trait,
 * to minimize compile time given there is a slight compiler overhead to
 * mixing in traits compared to extending classes. If you need to mix the
 * behavior of <code>AnyFlatSpec</code> into some other class, you can use this
 * trait instead, because class <code>AnyFlatSpec</code> does nothing more than
 * extend this trait and add a nice <code>toString</code> implementation.
 * </p>
 *
 * <p>
 * See the documentation of the class for a <a href="AnyFlatSpec.html">detailed
 * overview of <code>AnyFlatSpec</code></a>.
 * </p>
 *
 * @author Bill Venners
 */
@Finders(Array("org.scalatest.finders.FlatSpecFinder"))
//SCALATESTJS-ONLY @scala.scalajs.reflect.annotation.EnableReflectiveInstantiation
//SCALATESTNATIVE-ONLY @scala.scalanative.reflect.annotation.EnableReflectiveInstantiation
trait AnyFlatSpecLike extends TestSuite with ShouldVerb with MustVerb with CanVerb with Informing with Notifying with Alerting with Documenting { thisSuite =>

  private final val engine = new Engine(Resources.concurrentSpecMod, "Spec")
  import engine._

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
   * Returns a <code>Notifier</code> that during test execution will forward strings passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>AnyFlatSpec</code> is being executed, such as from inside a test function, it will forward the information to
   * the current reporter immediately. If invoked at any other time, it will
   * print to the standard output. This method can be called safely by any thread.
   */
  protected def note: Notifier = atomicNotifier.get

  /**
   * Returns an <code>Alerter</code> that during test execution will forward strings passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>AnyFlatSpec</code> is being executed, such as from inside a test function, it will forward the information to
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

  private final def registerTestImpl(testText: String, testTags: Tag*)(testFun: => Any /* Assertion */, pos: source.Position): Unit = {
    // SKIP-SCALATESTJS,NATIVE-START
    val stackDepthAdjustment = -1
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -4
    engine.registerTest(testText, Transformer(() => testFun), Resources.testCannotBeNestedInsideAnotherTest, "AnyFlatSpecLike.scala", "registerTest", 4, stackDepthAdjustment, None, None, Some(pos), None, testTags: _*)
  }

  // SKIP-DOTTY-START
  final def registerTest(testText: String, testTags: Tag*)(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
    registerTestImpl(testText, testTags: _*)(testFun, pos)
  }
  // SKIP-DOTTY-END
  //DOTTY-ONLY inline def registerTest(testText: String, testTags: Tag*)(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
  //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestImpl(testText, testTags: _*)(testFun, pos) }) } 
  //DOTTY-ONLY }

  private final def registerIgnoredTestImpl(testText: String, testTags: Tag*)(testFun: => Any /* Assertion */, pos: source.Position): Unit = {
    // SKIP-SCALATESTJS,NATIVE-START
    val stackDepthAdjustment = -3
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -4
    engine.registerIgnoredTest(testText, Transformer(() => testFun), Resources.testCannotBeNestedInsideAnotherTest, "AnyFlatSpecLike.scala", "registerIgnoredTest", 4, stackDepthAdjustment, None, Some(pos), testTags: _*)
  }

  // SKIP-DOTTY-START
  final def registerIgnoredTest(testText: String, testTags: Tag*)(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
    registerIgnoredTestImpl(testText, testTags: _*)(testFun, pos)
  }
  // SKIP-DOTTY-END
  //DOTTY-ONLY inline def registerIgnoredTest(testText: String, testTags: Tag*)(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
  //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerIgnoredTestImpl(testText, testTags: _*)(testFun, pos) }) } 
  //DOTTY-ONLY }

  /**
   * Register a test with the given spec text, optional tags, and test function value that takes no arguments.
   * An invocation of this method is called an &ldquo;example.&rdquo;
   *
   * This method will register the test for later execution via an invocation of one of the <code>execute</code>
   * methods. The name of the test will be a concatenation of the text of all surrounding describers,
   * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
   * this <code>AnyFlatSpec</code> instance.
   *
   * @param specText the specification text, which will be combined with the descText of any surrounding describers
   * to form the test name
   * @param methodName Method name of the caller
   * @param testTags the optional list of tags for this test
   * @param testFun the test function
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws NullArgumentException if <code>specText</code> or any passed test tag is <code>null</code>
   */
  private def registerTestToRun(specText: String, methodName: String, testTags: List[Tag], testFun: () => Any /* Assertion */, pos: source.Position): Unit = {
    // SKIP-SCALATESTJS,NATIVE-START
    val stackDepth = 4
    val stackDepthAdjustment = -3
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val stackDepth = 6
    //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -6
    def testRegistrationClosedMessageFun: String =
      methodName match {
        case "in" => Resources.inCannotAppearInsideAnotherInOrIs
        case "is" => Resources.isCannotAppearInsideAnotherInOrIs
      }
    engine.registerTest(specText, Transformer(testFun), testRegistrationClosedMessageFun, "AnyFlatSpecLike.scala", methodName, stackDepth, stackDepthAdjustment, None, None, Some(pos), None, testTags: _*)
  }

  /**
   * Class that supports the registration of a &ldquo;subject&rdquo; being specified and tested via the
   * instance referenced from <code>AnyFlatSpec</code>'s <code>behavior</code> field.
   *
   * <p>
   * This field enables syntax such as the following subject registration:
   * </p>
   *
   * <pre class="stHighlight">
   * behavior of "A Stack"
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>behavior</code> field, see the <a href="AnyFlatSpec.html">main documentation</a>
   * for trait <code>AnyFlatSpec</code>.
   * </p>
   */
  protected final class BehaviorWord {

    private final def ofImpl(description: String, pos: source.Position): Unit = {
      // SKIP-SCALATESTJS,NATIVE-START
      val stackDepth = 3
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val stackDepth = 5
      registerFlatBranch(description, Resources.behaviorOfCannotAppearInsideAnIn, "AnyFlatSpecLike.scala", "of", stackDepth, 0, Some(pos))
    }

    /**
     * Supports the registration of a &ldquo;subject&rdquo; being specified and tested via the
     * instance referenced from <code>AnyFlatSpec</code>'s <code>behavior</code> field.
     *
     * <p>
     * This method enables syntax such as the following subject registration:
     * </p>
     *
     * <pre class="stHighlight">
     * behavior of "A Stack"
     *          ^
     * </pre>
     *
     * <p>
     * For more information and examples of the use of this method, see the <a href="AnyFlatSpec.html">main documentation</a>
     * for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def of(description: String)(implicit pos: source.Position): Unit = {
      ofImpl(description, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def of(description: String): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => ofImpl(description, pos) }) } 
    //DOTTY-ONLY }
  }

  /**
   * Supports the registration of a &ldquo;subject&rdquo; being specified and tested.
   *
   * <p>
   * This field enables syntax such as the following subject registration:
   * </p>
   *
   * <pre class="stHighlight">
   * behavior of "A Stack"
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>behavior</code> field, see the main documentation
   * for this trait.
   * </p>
   */
  protected val behavior = new BehaviorWord

  /**
   * Class that supports the registration of tagged tests via the <code>ItWord</code> instance
   * referenced from <code>AnyFlatSpec</code>'s <code>it</code> field.
   *
   * <p>
   * This class enables syntax such as the following tagged test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
   *                                                                      ^
   * </pre>
   *
   * <p>
   * It also enables syntax such as the following registration of an ignored, tagged test:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" taggedAs(SlowTest) ignore { ... }
   *                                                                      ^
   * </pre>
   *
   * <p>
   * In addition, it enables syntax such as the following registration of a pending, tagged test:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" taggedAs(SlowTest) is (pending)
   *                                                                      ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field to register tagged tests, see
   * the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a> in the main documentation for trait <code>AnyFlatSpec</code>.
   * For examples of tagged test registration, see
   * the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a> in the main documentation for trait <code>AnyFlatSpec</code>.
   * </p>
   */
  protected final class ItVerbStringTaggedAs(verb: String, name: String, tags: List[Tag]) {

    /**
     * Supports the registration of tagged tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
     *                                                                    ^
     * </pre>
     *
     * <p>
     * For examples of tagged test registration, see
     * the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a> in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def in(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToRun(verb.trim + " " + name.trim, "in", tags, () => testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def in(testFun: => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToRun(verb.trim + " " + name.trim, "in", tags, () => testFun, pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports the registration of pending, tagged tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" taggedAs(SlowTest) is (pending)
     *                                                                    ^
     * </pre>
     *
     * <p>
     * For examples of pending test registration, see the <a href="AnyFlatSpec.html#pendingTests">Pending tests section</a> in the main documentation
     * for trait <code>AnyFlatSpec</code>.  And for examples of tagged test registration, see
     * the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a> in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def is(testFun: => PendingStatement)(implicit pos: source.Position): Unit = {
      registerTestToRun(verb.trim + " " + name.trim, "is", tags, () => { testFun; succeed }, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def is(testFun: => PendingStatement): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToRun(verb.trim + " " + name.trim, "is", tags, () => { testFun; succeed }, pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports the registration of ignored, tagged tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" taggedAs(SlowTest) ignore { ... }
     *                                                                    ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a> in the main documentation
     * for trait <code>AnyFlatSpec</code>.  And for examples of tagged test registration, see
     * the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a> in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def ignore(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToIgnore(verb.trim + " " + name.trim, tags, "ignore", () => testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def ignore(testFun: => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToIgnore(verb.trim + " " + name.trim, tags, "ignore", () => testFun, pos) }) } 
    //DOTTY-ONLY }
  }

  /**
   * Class that supports test registration via the <code>ItWord</code> instance referenced from <code>AnyFlatSpec</code>'s <code>it</code> field.
   *
   * <p>
   * This class enables syntax such as the following test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" in { ... }
   *                                                   ^
   * </pre>
   *
   * <p>
   * It also enables syntax such as the following registration of an ignored test:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" ignore { ... }
   *                                                   ^
   * </pre>
   *
   * <p>
   * In addition, it enables syntax such as the following registration of a pending test:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" is (pending)
   *                                                   ^
   * </pre>
   *
   * <p>
   * And finally, it also enables syntax such as the following tagged test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
   *                                                   ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field, see the <a href="AnyFlatSpec.html">main documentation</a>
   * for trait <code>AnyFlatSpec</code>.
   * </p>
   */
  protected final class ItVerbString(verb: String, name: String) {

    /**
     * Supports the registration of tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" in { ... }
     *                                                 ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="AnyFlatSpec.html">main documentation</a>
     * for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def in(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToRun(verb.trim + " " + name.trim, "in", List(), () => testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def in(testFun: => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToRun(verb.trim + " " + name.trim, "in", List(), () => testFun, pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports the registration of pending tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" is (pending)
     *                                                 ^
     * </pre>
     *
     * <p>
     * For examples of pending test registration, see the <a href="AnyFlatSpec.html#pendingTests">Pending tests section</a> in the main documentation
     * for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def is(testFun: => PendingStatement)(implicit pos: source.Position): Unit = {
      registerTestToRun(verb.trim + " " + name.trim, "is", List(), () => { testFun; succeed }, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def is(testFun: => PendingStatement): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToRun(verb.trim + " " + name.trim, "is", List(), () => { testFun; succeed }, pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports the registration of ignored tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" ignore { ... }
     *                                                 ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a> in the main documentation
     * for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def ignore(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToIgnore(verb.trim + " " + name.trim, List(), "ignore", () => testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def ignore(testFun: => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToIgnore(verb.trim + " " + name.trim, List(), "ignore", () => testFun, pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports the registration of tagged tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
     *                                                 ^
     * </pre>
     *
     * <p>
     * For examples of tagged test registration, see the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a> in the main documentation
     * for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix 
    def taggedAs(firstTestTag: Tag, otherTestTags: Tag*) = {
      val tagList = firstTestTag :: otherTestTags.toList
      new ItVerbStringTaggedAs(verb, name, tagList)
    }
  }

  /**
   * Class that supports test (and shared test) registration via the instance referenced from <code>AnyFlatSpec</code>'s <code>it</code> field.
   *
   * <p>
   * This class enables syntax such as the following test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" in { ... }
   * ^
   * </pre>
   *
   * <p>
   * It also enables syntax such as the following shared test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * it should behave like nonEmptyStack(lastItemPushed)
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field, see the main documentation
   * for this trait.
   * </p>
   */
  protected final class ItWord {

    /**
     * Supports the registration of tests with <code>should</code> in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it should "pop values in last-in-first-out order" in { ... }
     *    ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="AnyFlatSpec.html">main documentation</a>
     * for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix  
    def should(string: String) = new ItVerbString("should", string)

    /**
     * Supports the registration of tests with <code>must</code> in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must "pop values in last-in-first-out order" in { ... }
     *    ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="AnyFlatSpec.html">main documentation</a>
     * for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix  
    def must(string: String) = new ItVerbString("must", string)

    /**
     * Supports the registration of tests with <code>can</code> in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it can "pop values in last-in-first-out order" in { ... }
     *    ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="AnyFlatSpec.html">main documentation</a>
     * for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix  
    def can(string: String) = new ItVerbString("can", string)

    /**
     * Supports the registration of shared tests with <code>should</code> in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it should behave like nonFullStack(stackWithOneItem)
     *    ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="AnyFlatSpec.html#sharedTests">Shared tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix  
    def should(behaveWord: BehaveWord) = behaveWord

    /**
     * Supports the registration of shared tests with <code>must</code> in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it must behave like nonFullStack(stackWithOneItem)
     *    ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="AnyFlatSpec.html#sharedTests">Shared tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix  
    def must(behaveWord: BehaveWord) = behaveWord

    /**
     * Supports the registration of shared tests with <code>can</code> in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * it can behave like nonFullStack(stackWithOneItem)
     *    ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="AnyFlatSpec.html#sharedTests">Shared tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix  
    def can(behaveWord: BehaveWord) = behaveWord
  }

  /**
   * Supports test (and shared test) registration in <code>AnyFlatSpec</code>s.
   *
   * <p>
   * This field enables syntax such as the following test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * it should "pop values in last-in-first-out order" in { ... }
   * ^
   * </pre>
   *
   * <p>
   * It also enables syntax such as the following shared test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * it should behave like nonEmptyStack(lastItemPushed)
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field, see the main documentation
   * for this trait.
   * </p>
   */
  protected val it = new ItWord

  /**
   * Class that supports registration of ignored, tagged tests via the <code>IgnoreWord</code> instance referenced
   * from <code>AnyFlatSpec</code>'s <code>ignore</code> field.
   *
   * <p>
   * This class enables syntax such as the following registration of an ignored, tagged test:
   * </p>
   *
   * <pre class="stHighlight">
   * ignore should "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
   *                                                                          ^
   * </pre>
   *
   * <p>
   * In addition, it enables syntax such as the following registration of an ignored, tagged, pending test:
   * </p>
   *
   * <pre class="stHighlight">
   * ignore should "pop values in last-in-first-out order" taggedAs(SlowTest) is (pending)
   *                                                                          ^
   * </pre>
   *
   * <p>
   * Note: the <code>is</code> method is provided for completeness and design symmetry, given there's no way
   * to prevent changing <code>is</code> to <code>ignore</code> and marking a pending test as ignored that way.
   * Although it isn't clear why someone would want to mark a pending test as ignored, it can be done.
   * </p>
   *
   * <p>
   * For more information and examples of the use of the <code>ignore</code> field, see the <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a>
   * in the main documentation for trait <code>AnyFlatSpec</code>. For examples of tagged test registration, see
   * the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a> in the main documentation for trait <code>AnyFlatSpec</code>.
   * </p>
   */
  protected final class IgnoreVerbStringTaggedAs(verb: String, name: String, tags: List[Tag]) {

    /**
     * Supports the registration of ignored, tagged tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore must "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
     *                                                                        ^
     * </pre>
     *
     * <p>
     * For examples of the registration of ignored tests, see the <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>. For examples of tagged test registration, see
     * the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a> in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def in(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToIgnore(verb.trim + " " + name.trim, tags, "in", () => testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def in(testFun: => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToIgnore(verb.trim + " " + name.trim, tags, "in", () => testFun, pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports the registration of ignored, tagged, pending tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore must "pop values in last-in-first-out order" taggedAs(SlowTest) is (pending)
     *                                                                        ^
     * </pre>
     *
     * <p>
     * Note: this <code>is</code> method is provided for completeness and design symmetry, given there's no way
     * to prevent changing <code>is</code> to <code>ignore</code> and marking a pending test as ignored that way.
     * Although it isn't clear why someone would want to mark a pending test as ignored, it can be done.
     * </p>
     *
     * <p>
     * For examples of pending test registration, see the <a href="AnyFlatSpec.html#pendingTests">Pending tests section</a> in the main documentation
     * for trait <code>AnyFlatSpec</code>.  For examples of the registration of ignored tests,
     * see the <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>. For examples of tagged test registration, see
     * the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a> in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def is(testFun: => PendingStatement)(implicit pos: source.Position): Unit = {
      registerTestToIgnore(verb.trim + " " + name.trim, tags, "is", () => { testFun; succeed }, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def is(testFun: => PendingStatement): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToIgnore(verb.trim + " " + name.trim, tags, "is", () => { testFun; succeed }, pos) }) } 
    //DOTTY-ONLY }

    // Note: no def ignore here, so you can't put two ignores in the same line
  }

  /**
   * Class that supports registration of ignored tests via the <code>IgnoreWord</code> instance referenced
   * from <code>AnyFlatSpec</code>'s <code>ignore</code> field.
   *
   * <p>
   * This class enables syntax such as the following registration of an ignored test:
   * </p>
   *
   * <pre class="stHighlight">
   * ignore should "pop values in last-in-first-out order" in { ... }
   *                                                       ^
   * </pre>
   *
   * <p>
   * In addition, it enables syntax such as the following registration of an ignored, pending test:
   * </p>
   *
   * <pre class="stHighlight">
   * ignore should "pop values in last-in-first-out order" is (pending)
   *                                                       ^
   * </pre>
   *
   * <p>
   * Note: the <code>is</code> method is provided for completeness and design symmetry, given there's no way
   * to prevent changing <code>is</code> to <code>ignore</code> and marking a pending test as ignored that way.
   * Although it isn't clear why someone would want to mark a pending test as ignored, it can be done.
   * </p>
   *
   * <p>
   * And finally, it also enables syntax such as the following ignored, tagged test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * ignore should "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
   *                                                       ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>ignore</code> field, see the <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a>
   * in the main documentation for trait <code>AnyFlatSpec</code>.
   * </p>
   */
  protected final class IgnoreVerbString(verb: String, name: String) {

    /**
     * Supports the registration of ignored tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore must "pop values in last-in-first-out order" in { ... }
     *                                                     ^
     * </pre>
     *
     * <p>
     * For examples of the registration of ignored tests, see the <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def in(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToIgnore(verb.trim + " " + name.trim, List(), "in", () => testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def in(testFun: => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToIgnore(verb.trim + " " + name.trim, List(), "in", () => testFun, pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports the registration of ignored, pending tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore must "pop values in last-in-first-out order" is (pending)
     *                                                     ^
     * </pre>
     *
     * <p>
     * Note: this <code>is</code> method is provided for completeness and design symmetry, given there's no way
     * to prevent changing <code>is</code> to <code>ignore</code> and marking a pending test as ignored that way.
     * Although it isn't clear why someone would want to mark a pending test as ignored, it can be done.
     * </p>
     *
     * <p>
     * For examples of pending test registration, see the <a href="AnyFlatSpec.html#pendingTests">Pending tests section</a> in the main documentation
     * for trait <code>AnyFlatSpec</code>.  For examples of the registration of ignored tests,
     * see the <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def is(testFun: => PendingStatement)(implicit pos: source.Position): Unit = {
      registerTestToIgnore(verb.trim + " " + name.trim, List(), "is", () => { testFun; succeed }, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def is(testFun: => PendingStatement): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToIgnore(verb.trim + " " + name.trim, List(), "is", () => { testFun; succeed }, pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports the registration of ignored, tagged tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore must "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
     *                                                     ^
     * </pre>
     *
     * <p>
     * For examples of tagged test registration, see the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a> in the main documentation
     * for trait <code>AnyFlatSpec</code>.  For examples of the registration of ignored tests,
     * see the <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix  
    def taggedAs(firstTestTag: Tag, otherTestTags: Tag*) = {
      val tagList = firstTestTag :: otherTestTags.toList
      new IgnoreVerbStringTaggedAs(verb, name, tagList)
    }
  }

  /**
   * Class that supports registration of ignored tests via the <code>ItWord</code> instance
   * referenced from <code>AnyFlatSpec</code>'s <code>ignore</code> field.
   *
   * <p>
   * This class enables syntax such as the following registration of an ignored test:
   * </p>
   *
   * <pre class="stHighlight">
   * ignore should "pop values in last-in-first-out order" in { ... }
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>ignore</code> field, see <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a>
   * in the main documentation for this trait.
   * </p>
   */
  protected final class IgnoreWord {

    /**
     * Supports the registration of ignored tests with <code>should</code> in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore should "pop values in last-in-first-out order" in { ... }
     *        ^
     * </pre>
     *
     * <p>
     * For more information and examples of the use of the <code>ignore</code> field, see <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix  
    def should(string: String) = new IgnoreVerbString("should", string)

    /**
     * Supports the registration of ignored tests with <code>must</code> in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore must "pop values in last-in-first-out order" in { ... }
     *        ^
     * </pre>
     *
     * <p>
     * For more information and examples of the use of the <code>ignore</code> field, see <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix  
    def must(string: String) = new IgnoreVerbString("must", string)

    /**
     * Supports the registration of ignored tests with <code>can</code> in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * ignore can "pop values in last-in-first-out order" in { ... }
     *        ^
     * </pre>
     *
     * <p>
     * For more information and examples of the use of the <code>ignore</code> field, see <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix  
    def can(string: String) = new IgnoreVerbString("can", string)
  }

  /**
   * Supports registration of ignored tests in <code>AnyFlatSpec</code>s.
   *
   * <p>
   * This field enables syntax such as the following registration of an ignored test:
   * </p>
   *
   * <pre class="stHighlight">
   * ignore should "pop values in last-in-first-out order" in { ... }
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>ignore</code> field, see the <a href="#ignoredTests">Ignored tests section</a>
   * in the main documentation for this trait.
   * </p>
   */
  protected val ignore = new IgnoreWord

  /**
   * Class that supports the registration of tagged tests via the <code>TheyWord</code> instance
   * referenced from <code>AnyFlatSpec</code>'s <code>they</code> field.
   *
   * <p>
   * This class enables syntax such as the following tagged test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
   *                                                                        ^
   * </pre>
   *
   * <p>
   * It also enables syntax such as the following registration of an ignored, tagged test:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" taggedAs(SlowTest) ignore { ... }
   *                                                                        ^
   * </pre>
   *
   * <p>
   * In addition, it enables syntax such as the following registration of a pending, tagged test:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" taggedAs(SlowTest) is (pending)
   *                                                                        ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>they</code> field to register tagged tests, see
   * the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a> in the main documentation for trait <code>AnyFlatSpec</code>.
   * For examples of tagged test registration, see
   * the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a> in the main documentation for trait <code>AnyFlatSpec</code>.
   * </p>
   */
  protected final class TheyVerbStringTaggedAs(verb: String, name: String, tags: List[Tag]) {

    /**
     * Supports the registration of tagged tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
     *                                                                      ^
     * </pre>
     *
     * <p>
     * For examples of tagged test registration, see
     * the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a> in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def in(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToRun(verb.trim + " " + name.trim, "in", tags, () => testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def in(testFun: => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToRun(verb.trim + " " + name.trim, "in", tags, () => testFun, pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports the registration of pending, tagged tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" taggedAs(SlowTest) is (pending)
     *                                                                      ^
     * </pre>
     *
     * <p>
     * For examples of pending test registration, see the <a href="AnyFlatSpec.html#pendingTests">Pending tests section</a> in the main documentation
     * for trait <code>AnyFlatSpec</code>.  And for examples of tagged test registration, see
     * the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a> in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def is(testFun: => PendingStatement)(implicit pos: source.Position): Unit = {
      registerTestToRun(verb.trim + " " + name.trim, "is", tags, () => { testFun; succeed }, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def is(testFun: => PendingStatement): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToRun(verb.trim + " " + name.trim, "is", tags, () => { testFun; succeed }, pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports the registration of ignored, tagged tests in a <code>FlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" taggedAs(SlowTest) ignore { ... }
     *                                                                      ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a> in the main documentation
     * for trait <code>AnyFlatSpec</code>.  And for examples of tagged test registration, see
     * the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a> in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def ignore(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToIgnore(verb.trim + " " + name.trim, tags, "ignore", () => testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def ignore(testFun: => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToIgnore(verb.trim + " " + name.trim, tags, "ignore", () => testFun, pos) }) } 
    //DOTTY-ONLY }
  }

  /**
   * Class that supports test registration via the <code>TheyWord</code> instance referenced from <code>AnyFlatSpec</code>'s <code>they</code> field.
   *
   * <p>
   * This class enables syntax such as the following test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" in { ... }
   *                                                     ^
   * </pre>
   *
   * <p>
   * It also enables syntax such as the following registration of an ignored test:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" ignore { ... }
   *                                                     ^
   * </pre>
   *
   * <p>
   * In addition, it enables syntax such as the following registration of a pending test:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" is (pending)
   *                                                     ^
   * </pre>
   *
   * <p>
   * And finally, it also enables syntax such as the following tagged test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
   *                                                     ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field, see the <a href="AnyFlatSpec.html">main documentation</a>
   * for trait <code>AnyFlatSpec</code>.
   * </p>
   */
  protected final class TheyVerbString(verb: String, name: String) {

    /**
     * Supports the registration of tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" in { ... }
     *                                                   ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="AnyFlatSpec.html">main documentation</a>
     * for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def in(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToRun(verb.trim + " " + name.trim, "in", List(), () => testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def in(testFun: => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToRun(verb.trim + " " + name.trim, "in", List(), () => testFun, pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports the registration of pending tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" is (pending)
     *                                                   ^
     * </pre>
     *
     * <p>
     * For examples of pending test registration, see the <a href="AnyFlatSpec.html#pendingTests">Pending tests section</a> in the main documentation
     * for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def is(testFun: => PendingStatement)(implicit pos: source.Position): Unit = {
      registerTestToRun(verb.trim + " " + name.trim, "is", List(), () => { testFun; succeed }, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def is(testFun: => PendingStatement): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToRun(verb.trim + " " + name.trim, "is", List(), () => { testFun; succeed }, pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports the registration of ignored tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" ignore { ... }
     *                                                   ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a> in the main documentation
     * for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def ignore(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToIgnore(verb.trim + " " + name.trim, List(), "ignore", () => testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def ignore(testFun: => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToIgnore(verb.trim + " " + name.trim, List(), "ignore", () => testFun, pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports the registration of tagged tests in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
     *                                                   ^
     * </pre>
     *
     * <p>
     * For examples of tagged test registration, see the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a> in the main documentation
     * for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix  
    def taggedAs(firstTestTag: Tag, otherTestTags: Tag*) = {
      val tagList = firstTestTag :: otherTestTags.toList
      new ItVerbStringTaggedAs(verb, name, tagList)
    }
  }

  /**
   * Class that supports test (and shared test) registration via the instance referenced from <code>AnyFlatSpec</code>'s <code>it</code> field.
   *
   * <p>
   * This class enables syntax such as the following test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" in { ... }
   * ^
   * </pre>
   *
   * <p>
   * It also enables syntax such as the following shared test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * they should behave like nonEmptyStack(lastItemPushed)
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field, see the main documentation
   * for this trait.
   * </p>
   */
  protected final class TheyWord {

    /**
     * Supports the registration of tests with <code>should</code> in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they should "pop values in last-in-first-out order" in { ... }
     *      ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="AnyFlatSpec.html">main documentation</a>
     * for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix  
    def should(string: String) = new ItVerbString("should", string)

    /**
     * Supports the registration of tests with <code>must</code> in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must "pop values in last-in-first-out order" in { ... }
     *      ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="AnyFlatSpec.html">main documentation</a>
     * for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix  
    def must(string: String) = new ItVerbString("must", string)

    /**
     * Supports the registration of tests with <code>can</code> in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they can "pop values in last-in-first-out order" in { ... }
     *      ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="AnyFlatSpec.html">main documentation</a>
     * for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix  
    def can(string: String) = new ItVerbString("can", string)

    /**
     * Supports the registration of shared tests with <code>should</code> in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they should behave like nonFullStack(stackWithOneItem)
     *      ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="AnyFlatSpec.html#sharedTests">Shared tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix  
    def should(behaveWord: BehaveWord) = behaveWord

    /**
     * Supports the registration of shared tests with <code>must</code> in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they must behave like nonFullStack(stackWithOneItem)
     *      ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="AnyFlatSpec.html#sharedTests">Shared tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix  
    def must(behaveWord: BehaveWord) = behaveWord

    /**
     * Supports the registration of shared tests with <code>can</code> in a <code>AnyFlatSpec</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * they can behave like nonFullStack(stackWithOneItem)
     *      ^
     * </pre>
     *
     * <p>
     * For examples of shared tests, see the <a href="AnyFlatSpec.html#sharedTests">Shared tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    //DOTTY-ONLY     infix  
    def can(behaveWord: BehaveWord) = behaveWord
  }

  /**
   * Supports test (and shared test) registration in <code>AnyFlatSpec</code>s.
   *
   * <p>
   * This field enables syntax such as the following test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * they should "pop values in last-in-first-out order" in { ... }
   * ^
   * </pre>
   *
   * <p>
   * It also enables syntax such as the following shared test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * they should behave like nonEmptyStack(lastItemPushed)
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field, see the main documentation
   * for this trait.
   * </p>
   */
  protected val they = new TheyWord

  /**
   * Class that supports test registration in shorthand form.
   *
   * <p>
   * For example, this class enables syntax such as the following test registration
   * in shorthand form:
   * </p>
   *
   * <pre class="stHighlight">
   * "A Stack (when empty)" should "be empty" in { ... }
   *                                          ^
   * </pre>
   *
   * <p>
   * This class also enables syntax such as the following ignored test registration
   * in shorthand form:
   * </p>
   *
   * <pre class="stHighlight">
   * "A Stack (when empty)" should "be empty" ignore { ... }
   *                                          ^
   * </pre>
   *
   * <p>
   * This class is used via an implicit conversion (named <code>convertToInAndIgnoreMethods</code>)
   * from <code>ResultOfStringPassedToVerb</code>. The <code>ResultOfStringPassedToVerb</code> class
   * does not declare any methods named <code>in</code>, because the
   * type passed to <code>in</code> differs in a <code>AnyFlatSpec</code> and a <code>FixtureAnyFlatSpec</code>.
   * A <code>FixtureAnyFlatSpec</code> needs two <code>in</code> methods, one that takes a no-arg
   * test function and another that takes a one-arg test function (a test that takes a
   * <code>Fixture</code> as its parameter). By constrast, a <code>AnyFlatSpec</code> needs
   * only one <code>in</code> method that takes a by-name parameter. As a result,
   * <code>AnyFlatSpec</code> and <code>FixtureAnyFlatSpec</code> each provide an implicit conversion
   * from <code>ResultOfStringPassedToVerb</code> to a type that provides the appropriate
   * <code>in</code> methods.
   * </p>
   *
   * @author Bill Venners
   */
  protected final class InAndIgnoreMethods(resultOfStringPassedToVerb: ResultOfStringPassedToVerb) {

    import resultOfStringPassedToVerb.rest
    import resultOfStringPassedToVerb.verb

    /**
     * Supports the registration of tests in shorthand form.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" must "pop values in last-in-first-out order" in { ... }
     *                                                        ^
     * </pre>
     *
     * <p>
     * For examples of test registration, see the <a href="AnyFlatSpec.html">main documentation</a>
     * for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def in(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToRun(verb.trim + " " + rest.trim, "in", List(), () => testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY def in(testFun: => Any /* Assertion */)(using pos: source.Position): Unit = 
    //DOTTY-ONLY   registerTestToRun(verb.trim + " " + rest.trim, "in", List(), () => testFun, pos)

    /**
     * Supports the registration of ignored tests in shorthand form.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" must "pop values in last-in-first-out order" ignore { ... }
     *                                                        ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def ignore(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToIgnore(verb.trim + " " + rest.trim, List(), "ignore", () => testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def ignore(testFun: => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToIgnore(verb.trim + " " + rest.trim, List(), "ignore", () => testFun, pos) }) } 
    //DOTTY-ONLY }
  }

  import scala.language.implicitConversions

  /**
  // SKIP-DOTTY-START
   * Implicitly converts an object of type <code>ResultOfStringPassedToVerb</code> to an
  // SKIP-DOTTY-END
  //DOTTY-ONLY  * converts an object of type <code>ResultOfStringPassedToVerb</code> to an
   * <code>InAndIgnoreMethods</code>, to enable <code>in</code> and <code>ignore</code>
   * methods to be invokable on that object.
   */
  // SKIP-DOTTY-START
  protected implicit def convertToInAndIgnoreMethods(resultOfStringPassedToVerb: ResultOfStringPassedToVerb): InAndIgnoreMethods =
  // SKIP-DOTTY-END
  //DOTTY-ONLY protected def convertToInAndIgnoreMethods(resultOfStringPassedToVerb: ResultOfStringPassedToVerb): InAndIgnoreMethods =
    new InAndIgnoreMethods(resultOfStringPassedToVerb)
  

  //DOTTY-ONLY extension (resultOfStringPassedToVerb: ResultOfStringPassedToVerb) {
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Supports the registration of tagged tests in shorthand form.
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * This method supports syntax such as the following:
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <pre class="stHighlight">
  //DOTTY-ONLY    * "A Stack" must "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
  //DOTTY-ONLY    *                                                                           ^
  //DOTTY-ONLY    * </pre>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * For examples of tagged test registration, see the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a>
  //DOTTY-ONLY    * in the main documentation for trait <code>AnyFlatSpec</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    */ 
  //DOTTY-ONLY   inline infix def in(testFun: => Any /* Assertion */): Unit =
  //DOTTY-ONLY     ${ source.Position.withPosition[Unit]('{(pos: source.Position) => 
  //DOTTY-ONLY       convertToInAndIgnoreMethods(resultOfStringPassedToVerb).in(testFun)(using pos)
  //DOTTY-ONLY     }) }
  //DOTTY-ONLY   /**
  //DOTTY-ONLY     * Supports the registration of tagged, ignored tests in shorthand form.
  //DOTTY-ONLY     *
  //DOTTY-ONLY     * <p>
  //DOTTY-ONLY     * This method supports syntax such as the following:
  //DOTTY-ONLY     * </p>
  //DOTTY-ONLY     *
  //DOTTY-ONLY     * <pre class="stHighlight">
  //DOTTY-ONLY     * "A Stack" must "pop values in last-in-first-out order" taggedAs(SlowTest) ignore { ... }
  //DOTTY-ONLY     *                                                                           ^
  //DOTTY-ONLY     * </pre>
  //DOTTY-ONLY     *
  //DOTTY-ONLY     * <p>
  //DOTTY-ONLY     * For examples of ignored test registration, see the <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a>
  //DOTTY-ONLY     * in the main documentation for trait <code>AnyFlatSpec</code>.
  //DOTTY-ONLY     * For examples of tagged test registration, see the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a>
  //DOTTY-ONLY     * in the main documentation for trait <code>AnyFlatSpec</code>.
  //DOTTY-ONLY     * </p>
  //DOTTY-ONLY     */
  //DOTTY-ONLY   inline infix def ignore(testFun: => Any /* Assertion */)(using pos: source.Position): Unit =
  //DOTTY-ONLY     convertToInAndIgnoreMethods(resultOfStringPassedToVerb).ignore(testFun)
  //DOTTY-ONLY }
   
  /**
   * Class that supports tagged test registration in shorthand form.
   *
   * <p>
   * For example, this class enables syntax such as the following tagged test registration
   * in shorthand form:
   * </p>
   *
   * <pre class="stHighlight">
   * "A Stack (when empty)" should "be empty" taggedAs() in { ... }
   *                                                     ^
   * </pre>
   *
   * <p>
   * This class also enables syntax such as the following tagged, ignored test registration
   * in shorthand form:
   * </p>
   *
   * <pre class="stHighlight">
   * "A Stack (when empty)" should "be empty" taggedAs(SlowTest) ignore { ... }
   *                                                             ^
   * </pre>
   *
   * <p>
   * This class is used via an implicit conversion (named <code>convertToInAndIgnoreMethodsAfterTaggedAs</code>)
   * from <code>ResultOfTaggedAsInvocation</code>. The <code>ResultOfTaggedAsInvocation</code> class
   * does not declare any methods named <code>in</code>, because the
   * type passed to <code>in</code> differs in a <code>AnyFlatSpec</code> and a <code>FixtureAnyFlatSpec</code>.
   * A <code>FixtureAnyFlatSpec</code> needs two <code>in</code> methods, one that takes a no-arg
   * test function and another that takes a one-arg test function (a test that takes a
   * <code>Fixture</code> as its parameter). By constrast, a <code>AnyFlatSpec</code> needs
   * only one <code>in</code> method that takes a by-name parameter. As a result,
   * <code>AnyFlatSpec</code> and <code>FixtureAnyFlatSpec</code> each provide an implicit conversion
   * from <code>ResultOfTaggedAsInvocation</code> to a type that provides the appropriate
   * <code>in</code> methods.
   * </p>
   *
   * @author Bill Venners
   */
  protected final class InAndIgnoreMethodsAfterTaggedAs(resultOfTaggedAsInvocation: ResultOfTaggedAsInvocation) {

    import resultOfTaggedAsInvocation.verb
    import resultOfTaggedAsInvocation.rest
    import resultOfTaggedAsInvocation.{tags => tagsList}

    /**
     * Supports the registration of tagged tests in shorthand form.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" must "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
     *                                                                           ^
     * </pre>
     *
     * <p>
     * For examples of tagged test registration, see the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def in(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToRun(verb.trim + " " + rest.trim, "in", tagsList, () => testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY def in(testFun: => Any /* Assertion */)(using pos: source.Position): Unit = 
    //DOTTY-ONLY   registerTestToRun(verb.trim + " " + rest.trim, "in", tagsList, () => testFun, pos)

    /**
     * Supports the registration of tagged, ignored tests in shorthand form.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" must "pop values in last-in-first-out order" taggedAs(SlowTest) ignore { ... }
     *                                                                           ^
     * </pre>
     *
     * <p>
     * For examples of ignored test registration, see the <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>.
     * For examples of tagged test registration, see the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a>
     * in the main documentation for trait <code>AnyFlatSpec</code>.
     * </p>
     */
    // SKIP-DOTTY-START
    def ignore(testFun: => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToIgnore(verb.trim + " " + rest.trim, tagsList, "ignore", () => testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def ignore(testFun: => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToIgnore(verb.trim + " " + rest.trim, tagsList, "ignore", () => testFun, pos) }) } 
    //DOTTY-ONLY }
  }

  /**
  // SKIP-DOTTY-START
   * Implicitly converts an object of type <code>ResultOfTaggedAsInvocation</code> to an
  // SKIP-DOTTY-END
  //DOTTY-ONLY  * Converts an object of type <code>ResultOfTaggedAsInvocation</code> to an
   * <code>InAndIgnoreMethodsAfterTaggedAs</code>, to enable <code>in</code> and <code>ignore</code>
   * methods to be invokable on that object.
   */
  // SKIP-DOTTY-START
  protected implicit def convertToInAndIgnoreMethodsAfterTaggedAs(resultOfTaggedAsInvocation: ResultOfTaggedAsInvocation): InAndIgnoreMethodsAfterTaggedAs =
  // SKIP-DOTTY-END
  //DOTTY-ONLY protected def convertToInAndIgnoreMethodsAfterTaggedAs(resultOfTaggedAsInvocation: ResultOfTaggedAsInvocation): InAndIgnoreMethodsAfterTaggedAs =
    new InAndIgnoreMethodsAfterTaggedAs(resultOfTaggedAsInvocation)
  

  //DOTTY-ONLY extension (resultOfTaggedAsInvocation: ResultOfTaggedAsInvocation) {
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Supports the registration of tagged tests in shorthand form.
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * This method supports syntax such as the following:
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <pre class="stHighlight">
  //DOTTY-ONLY    * "A Stack" must "pop values in last-in-first-out order" taggedAs(SlowTest) in { ... }
  //DOTTY-ONLY    *                                                                           ^
  //DOTTY-ONLY    * </pre>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * For examples of tagged test registration, see the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a>
  //DOTTY-ONLY    * in the main documentation for trait <code>AnyFlatSpec</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    */
  //DOTTY-ONLY   inline infix def in(testFun: => Any /* Assertion */): Unit =
  //DOTTY-ONLY     ${ source.Position.withPosition[Unit]('{(pos: source.Position) =>
  //DOTTY-ONLY       convertToInAndIgnoreMethodsAfterTaggedAs(resultOfTaggedAsInvocation).in(testFun)(using pos)
  //DOTTY-ONLY     }) }
  //DOTTY-ONLY   /**
  //DOTTY-ONLY    * Supports the registration of tagged, ignored tests in shorthand form.
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * This method supports syntax such as the following:
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <pre class="stHighlight">
  //DOTTY-ONLY    * "A Stack" must "pop values in last-in-first-out order" taggedAs(SlowTest) ignore { ... }
  //DOTTY-ONLY    *                                                                           ^
  //DOTTY-ONLY    * </pre>
  //DOTTY-ONLY    *
  //DOTTY-ONLY    * <p>
  //DOTTY-ONLY    * For examples of ignored test registration, see the <a href="AnyFlatSpec.html#ignoredTests">Ignored tests section</a>
  //DOTTY-ONLY    * in the main documentation for trait <code>AnyFlatSpec</code>.
  //DOTTY-ONLY    * For examples of tagged test registration, see the <a href="AnyFlatSpec.html#taggingTests">Tagging tests section</a>
  //DOTTY-ONLY    * in the main documentation for trait <code>AnyFlatSpec</code>.
  //DOTTY-ONLY    * </p>
  //DOTTY-ONLY    */
  //DOTTY-ONLY   inline infix def ignore(testFun: => Any /* Assertion */)(using pos: source.Position): Unit =
  //DOTTY-ONLY     convertToInAndIgnoreMethodsAfterTaggedAs(resultOfTaggedAsInvocation).ignore(testFun)
  //DOTTY-ONLY }


  /**
   * Supports the shorthand form of test registration.
   *
   * <p>
   * For example, this method enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * "A Stack (when empty)" should "be empty" in { ... }
   *                        ^
   * </pre>
   *
   * <p>
   * This function is passed as an implicit parameter to a <code>should</code> method
   * provided in <code>ShouldVerb</code>, a <code>must</code> method
   * provided in <code>MustVerb</code>, and a <code>can</code> method
   * provided in <code>CanVerb</code>. When invoked, this function registers the
   * subject description (the first parameter to the function) and returns a <code>ResultOfStringPassedToVerb</code>
   * initialized with the verb and rest parameters (the second and third parameters to
   * the function, respectively).
   * </p>
   */
  // SKIP-DOTTY-START
  protected implicit val shorthandTestRegistrationFunction: StringVerbStringInvocation =
  // SKIP-DOTTY-END
  //DOTTY-ONLY  protected val shorthandTestRegistrationFunction: StringVerbStringInvocation =
    new StringVerbStringInvocation {
      def apply(subject: String, verb: String, rest: String, pos: source.Position): ResultOfStringPassedToVerb = {
        // SKIP-SCALATESTJS,NATIVE-START
        val stackDepth = 6
        // SKIP-SCALATESTJS,NATIVE-END
        //SCALATESTJS,NATIVE-ONLY val stackDepth = 8
        registerFlatBranch(subject, Resources.shouldCannotAppearInsideAnIn, "AnyFlatSpecLike.scala", "apply", stackDepth, 0, Some(pos))
        new ResultOfStringPassedToVerb(verb, rest) {

          //DOTTY-ONLY         infix
          def is(testFun: => PendingStatement): Unit = {
            registerTestToRun(this.verb.trim + " " + this.rest.trim, "is", List(), () => { testFun; succeed }, pos)
          }
          // Note, won't have an is method that takes fixture => PendingStatement one, because don't want
          // to say is (fixture => pending), rather just say is (pending)
          //DOTTY-ONLY         infix
          def taggedAs(firstTestTag: Tag, otherTestTags: Tag*) = {
            val tagList = firstTestTag :: otherTestTags.toList
            new ResultOfTaggedAsInvocation(this.verb, this.rest, tagList) {
              // "A Stack" should "bla bla" taggedAs(SlowTest) is (pending)
              //                                               ^
              def is(testFun: => PendingStatement): Unit = {
                registerTestToRun(this.verb.trim + " " + this.rest.trim, "is", this.tags, () => { testFun; succeed }, pos)
              }
            }
          }
        }
      }
    }
  //DOTTY-ONLY given StringVerbStringInvocation = shorthandTestRegistrationFunction  

  /**
   * Supports the shorthand form of shared test registration.
   *
   * <p>
   * For example, this method enables syntax such as the following in:
   * </p>
   *
   * <pre class="stHighlight">
   * "A Stack (with one item)" should behave like nonEmptyStack(stackWithOneItem, lastValuePushed)
   *                           ^
   * </pre>
   *
   * <p>
   * This function is passed as an implicit parameter to a <code>should</code> method
   * provided in <code>ShouldVerb</code>, a <code>must</code> method
   * provided in <code>MustVerb</code>, and a <code>can</code> method
   * provided in <code>CanVerb</code>. When invoked, this function registers the
   * subject description (the  parameter to the function) and returns a <code>BehaveWord</code>.
   * </p>
   */
  // SKIP-DOTTY-START
  protected implicit val shorthandSharedTestRegistrationFunction: StringVerbBehaveLikeInvocation =
  // SKIP-DOTTY-END
  //DOTTY-ONLY protected val shorthandSharedTestRegistrationFunction: StringVerbBehaveLikeInvocation =
    new StringVerbBehaveLikeInvocation {
      def apply(subject: String, pos: source.Position): BehaveWord = {
        registerFlatBranch(subject, Resources.shouldCannotAppearInsideAnIn, "AnyFlatSpecLike.scala", "apply", 5, 0, Some(pos))
        new BehaveWord
      }
    }
  //DOTTY-ONLY given StringVerbBehaveLikeInvocation = shorthandSharedTestRegistrationFunction  

// TODO: I got a:
// runsuite:
// [scalatest] *** RUN ABORTED ***
// [scalatest]   An exception or error caused a run to abort: Duplicate test name: should return the new exception with the clue string appended, separated by a space char if passed a function that does that (Engine.scala:464)
// Shouldn't be Engine.scala clearly
  /**
   * Register a test to ignore, which has the given spec text, optional tags, and test function value that takes no arguments.
   * This method will register the test for later ignoring via an invocation of one of the <code>execute</code>
   * methods. This method exists to make it easy to ignore an existing test by changing the call to <code>it</code>
   * to <code>ignore</code> without deleting or commenting out the actual test code. The test will not be executed, but a
   * report will be sent that indicates the test was ignored. The name of the test will be a concatenation of the text of all surrounding describers,
   * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
   * this <code>AnyFlatSpec</code> instance.
   *
   * @param specText the specification text, which will be combined with the descText of any surrounding describers
   * to form the test name
   * @param testTags the optional list of tags for this test
   * @param methodName caller's method name
   * @param testFun the test function
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws NullArgumentException if <code>specText</code> or any passed test tag is <code>null</code>
   */
  private def registerTestToIgnore(specText: String, testTags: List[Tag], methodName: String, testFun: () => Any /* Assertion */, pos: source.Position): Unit = {
    // SKIP-SCALATESTJS,NATIVE-START
    val stackDepth = 4
    val stackDepthAdjustment = -4
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val stackDepth = 6
    //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -6
    engine.registerIgnoredTest(specText, Transformer(testFun), Resources.ignoreCannotAppearInsideAnInOrAnIs, "AnyFlatSpecLike.scala", methodName, stackDepth, stackDepthAdjustment, None, Some(pos), testTags: _*)
  }

  /**
   * A <code>Map</code> whose keys are <code>String</code> names of tagged tests and whose associated values are
   * the <code>Set</code> of tags for the test. If this <code>AnyFlatSpec</code> contains no tags, this method returns an empty <code>Map</code>.
   *
   * <p>
   * This trait's implementation returns tags that were passed as strings contained in <code>Tag</code> objects passed to
   * <code>taggedAs</code>.
   * </p>
   *
   * <p>
   * In addition, this trait's implementation will also auto-tag tests with class level annotations.
   * For example, if you annotate <code>@Ignore</code> at the class level, all test methods in the class will be auto-annotated with
   * <code>org.scalatest.Ignore</code>.
   * </p>
   */
  override def tags: Map[String, Set[String]] = autoTagClassAnnotations(atomic.get.tagsMap, this)

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
   * @throws NullArgumentException if any of <code>testName</code>, <code>reporter</code>, <code>stopper</code>, or <code>configMap</code>
   *     is <code>null</code>.
   */
  protected override def runTest(testName: String, args: Args): Status = {

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
          val pos = testData.pos
        }
      )
    }

    runTestImpl(thisSuite, testName, args, true, invokeWithFixture)
  }

  /**
   * Run zero to many of this <code>AnyFlatSpec</code>'s tests.
   *
   * <p>
   * This method takes a <code>testName</code> parameter that optionally specifies a test to invoke.
   * If <code>testName</code> is <code>Some</code>, this trait's implementation of this method
   * invokes <code>runTest</code> on this object, passing in:
   * </p>
   *
   * <ul>
   * <li><code>testName</code> - the <code>String</code> value of the <code>testName</code> <code>Option</code> passed
   *   to this method</li>
   * <li><code>reporter</code> - the <code>Reporter</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>stopper</code> - the <code>Stopper</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>configMap</code> - the <code>configMap</code> passed to this method, or one that wraps and delegates to it</li>
   * </ul>
   *
   * <p>
   * This method takes a <code>Set</code> of tag names that should be included (<code>tagsToInclude</code>), and a <code>Set</code>
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
   * If so, this implementation invokes <code>runTest</code>, passing in:
   * </p>
   *
   * <ul>
   * <li><code>testName</code> - the <code>String</code> name of the test to run (which will be one of the names in the <code>testNames</code> <code>Set</code>)</li>
   * <li><code>reporter</code> - the <code>Reporter</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>stopper</code> - the <code>Stopper</code> passed to this method, or one that wraps and delegates to it</li>
   * <li><code>configMap</code> - the <code>configMap</code> passed to this method, or one that wraps and delegates to it</li>
   * </ul>
   *
   * @param testName an optional name of one test to execute. If <code>None</code>, all relevant tests should be executed.
   *                 I.e., <code>None</code> acts like a wildcard that means execute all relevant tests in this <code>AnyFlatSpec</code>.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when all tests started by this method have completed, and whether or not a failure occurred.
   *
   * @throws NullArgumentException if any of <code>testName</code>, <code>reporter</code>, <code>stopper</code>, <code>tagsToInclude</code>,
   *     <code>tagsToExclude</code>, or <code>configMap</code> is <code>null</code>.
   */
  protected override def runTests(testName: Option[String], args: Args): Status = {
    runTestsImpl(thisSuite, testName, args, info, true, runTest)
  }

  /**
   * An immutable <code>Set</code> of test names. If this <code>AnyFlatSpec</code> contains no tests, this method returns an
   * empty <code>Set</code>.
   *
   * <p>
   * This trait's implementation of this method will return a set that contains the names of all registered tests. The set's
   * iterator will return those names in the order in which the tests were registered. Each test's name is composed
   * of the concatenation of the text of each surrounding describer, in order from outside in, and the text of the
   * example itself, with all components separated by a space. For example, consider this <code>AnyFlatSpec</code>:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest.flatspec.AnyFlatSpec
   *
   * class StackSpec extends AnyFlatSpec {
   *
   *   "A Stack (when not empty)" must "allow me to pop" in {}
   *   it must "not be empty" in {}
   *
   *   "A Stack (when not full)" must "allow me to push" in {}
   *   it must "not be full" in {}
   * }
   * </pre>
   *
   * <p>
   * Invoking <code>testNames</code> on this <code>AnyFlatSpec</code> will yield a set that contains the following
   * two test name strings:
   * </p>
   *
   * <pre>
   * "A Stack (when not empty) must allow me to pop"
   * "A Stack (when not empty) must not be empty"
   * "A Stack (when not full) must allow me to push"
   * "A Stack (when not full) must not be full"
   * </pre>
   */
  override def testNames: Set[String] = {
    InsertionOrderSet(atomic.get.testNamesList)
  }

  override def run(testName: Option[String], args: Args): Status = {

    runImpl(thisSuite, testName, args, super.run)
  }

  /**
   * Supports shared test registration in <code>AnyFlatSpec</code>s.
   *
   * <p>
   * This field supports syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * it should behave like nonFullStack(stackWithOneItem)
   *           ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of <code>behave</code>, see the <a href="#sharedTests">Shared tests section</a>
   * in the main documentation for this trait.
   * </p>
   */
  protected val behave = new BehaveWord
    
  override def testDataFor(testName: String, theConfigMap: ConfigMap = ConfigMap.empty): TestData = createTestDataFor(testName, theConfigMap, this)
}
