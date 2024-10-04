/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest.wordspec

import org.scalatest._
import org.scalatest.exceptions._
import org.scalactic.{source, Prettifier}
import java.util.ConcurrentModificationException
import java.util.concurrent.atomic.AtomicReference
import org.scalatest.Suite.anExceptionThatShouldCauseAnAbort
import org.scalatest.Suite.autoTagClassAnnotations
import verbs.{CanVerb, ResultOfAfterWordApplication, ShouldVerb, BehaveWord, MustVerb,
StringVerbBlockRegistration, SubjectWithAfterWordRegistration}


/**
 * Implementation trait for class <code>FixtureAnyWordSpec</code>, which is
 * a sister class to <a href="AnyWordSpec.html"><code>org.scalatest.wordspec.AnyWordSpec</code></a> that can pass a
 * fixture object into its tests.
 *
 * <p>
 * <a href="FixtureAnyWordSpec.html"><code>FixtureAnyWordSpec</code></a> is a class,
 * not a trait, to minimize compile time given there is a slight compiler
 * overhead to mixing in traits compared to extending classes. If you need
 * to mix the behavior of <code>FixtureAnyWordSpec</code> into some other
 * class, you can use this trait instead, because class
 * <code>FixtureAnyWordSpec</code> does nothing more than extend this trait and add a nice <code>toString</code> implementation.
 * </p>
 *
 * <p>
 * See the documentation of the class for a <a href="FixtureAnyWordSpec.html">detailed
 * overview of <code>FixtureAnyWordSpec</code></a>.
 * </p>
 *
 * @author Bill Venners
 */
//SCALATESTJS-ONLY @scala.scalajs.reflect.annotation.EnableReflectiveInstantiation
//SCALATESTNATIVE-ONLY @scala.scalanative.reflect.annotation.EnableReflectiveInstantiation
@Finders(Array("org.scalatest.finders.WordSpecFinder"))
trait FixtureAnyWordSpecLike extends org.scalatest.FixtureTestSuite with ShouldVerb with MustVerb with CanVerb with Informing with Notifying with Alerting with Documenting { thisSuite =>

  private final val engine = new FixtureEngine[FixtureParam](Resources.concurrentFixtureWordSpecMod, "FixtureWordSpec")

  import engine._

  private[scalatest] val sourceFileName = "FixtureAnyWordSpecLike.scala"

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
   * <code>FixtureAnyWordSpec</code> is being executed, such as from inside a test function, it will forward the information to
   * the current reporter immediately. If invoked at any other time, it will
   * print to the standard output. This method can be called safely by any thread.
   */
  protected def note: Notifier = atomicNotifier.get

  /**
   * Returns an <code>Alerter</code> that during test execution will forward strings (and other objects) passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>FixtureAnyWordSpec</code> is being executed, such as from inside a test function, it will forward the information to
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

  private final def registerTestImpl(testText: String, testTags: Tag*)(testFun: FixtureParam => Any /* Assertion */, pos: source.Position): Unit = {
    // SKIP-SCALATESTJS,NATIVE-START
    val stackDepthAdjustment = -1
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -4
    engine.registerTest(testText, org.scalatest.fixture.Transformer(testFun), Resources.testCannotBeNestedInsideAnotherTest, sourceFileName, "registerTest", 4, stackDepthAdjustment, None, None, Some(pos), None, testTags: _*)
  }

  // SKIP-DOTTY-START
  final def registerTest(testText: String, testTags: Tag*)(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {
    registerTestImpl(testText, testTags: _*)(testFun, pos)
  }
  // SKIP-DOTTY-END
  //DOTTY-ONLY inline def registerTest(testText: String, testTags: Tag*)(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {  // Note: we can't remove the implicit pos here because it is the signature of registerTest in TestRegistration.
  //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestImpl(testText, testTags: _*)(testFun, pos) }) }
  //DOTTY-ONLY }

  private final def registerIgnoredTestImpl(testText: String, testTags: Tag*)(testFun: FixtureParam => Any /* Assertion */, pos: source.Position): Unit = {
    // SKIP-SCALATESTJS,NATIVE-START
    val stackDepthAdjustment = -3
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -5
    engine.registerIgnoredTest(testText, org.scalatest.fixture.Transformer(testFun), Resources.testCannotBeNestedInsideAnotherTest, sourceFileName, "registerIgnoredTest", 4, stackDepthAdjustment, None, Some(pos), testTags: _*)
  }

  // SKIP-DOTTY-START
  final def registerIgnoredTest(testText: String, testTags: Tag*)(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {
    registerIgnoredTestImpl(testText, testTags: _*)(testFun, pos)
  }
  // SKIP-DOTTY-END
  //DOTTY-ONLY inline def registerIgnoredTest(testText: String, testTags: Tag*)(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {  // Note: we can't remove the implicit pos here because it is the signature of registerTest in TestRegistration.
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
   * this <code>FixtureAnyWordSpec</code> instance.
   *
   * @param specText the specification text, which will be combined with the descText of any surrounding describers
   * to form the test name
   * @param testTags the optional list of tags for this test
   * @param methodName Caller's method name
   * @param testFun the test function
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws NullArgumentException if <code>specText</code> or any passed test tag is <code>null</code>
   */
  private def registerTestToRun(specText: String, testTags: List[Tag], methodName: String, testFun: FixtureParam => Any /* Assertion */, pos: source.Position): Unit = {
    // SKIP-SCALATESTJS,NATIVE-START
    val stackDepth = 4
    val stackDepthAdjustment = -3
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val stackDepth = 6
    //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -6
    engine.registerTest(specText, org.scalatest.fixture.Transformer(testFun), Resources.inCannotAppearInsideAnotherIn, sourceFileName, methodName, stackDepth, stackDepthAdjustment, None, None, Some(pos), None, testTags: _*)
  }

  private def registerPendingTestToRun(specText: String, testTags: List[Tag], methodName: String, testFun: FixtureParam => PendingStatement, pos: source.Position): Unit = {
    engine.registerTest(specText, org.scalatest.fixture.Transformer(testFun), Resources.inCannotAppearInsideAnotherIn, sourceFileName, methodName, 4, -3, None, None, Some(pos), None, testTags: _*)
  }

  /**
   * Register a test to ignore, which has the given spec text, optional tags, and test function value that takes no arguments.
   * This method will register the test for later ignoring via an invocation of one of the <code>execute</code>
   * methods. This method exists to make it easy to ignore an existing test by changing the call to <code>it</code>
   * to <code>ignore</code> without deleting or commenting out the actual test code. The test will not be executed, but a
   * report will be sent that indicates the test was ignored. The name of the test will be a concatenation of the text of all surrounding describers,
   * from outside in, and the passed spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.) The resulting test name must not have been registered previously on
   * this <code>FixtureAnyWordSpec</code> instance.
   *
   * @param specText the specification text, which will be combined with the descText of any surrounding describers
   * to form the test name
   * @param testTags the optional list of tags for this test
   * @param methodName Caller's method name
   * @param testFun the test function
   * @throws DuplicateTestNameException if a test with the same name has been registered previously
   * @throws TestRegistrationClosedException if invoked after <code>run</code> has been invoked on this suite
   * @throws NullArgumentException if <code>specText</code> or any passed test tag is <code>null</code>
   */
  private def registerTestToIgnore(specText: String, testTags: List[Tag], methodName: String, testFun: FixtureParam => Any /* Assertion */, pos: source.Position): Unit = {
    // SKIP-SCALATESTJS,NATIVE-START
    val stackDepth = 4
    val stackDepthAdjustment = -4
    // SKIP-SCALATESTJS,NATIVE-END
    //SCALATESTJS,NATIVE-ONLY val stackDepth = 6
    //SCALATESTJS,NATIVE-ONLY val stackDepthAdjustment = -7
    engine.registerIgnoredTest(specText, org.scalatest.fixture.Transformer(testFun), Resources.ignoreCannotAppearInsideAnIn, sourceFileName, methodName, stackDepth, stackDepthAdjustment, None, Some(pos), testTags: _*)
  }

  private def registerPendingTestToIgnore(specText: String, testTags: List[Tag], methodName: String, testFun: FixtureParam => PendingStatement, pos: source.Position): Unit = {
    engine.registerIgnoredTest(specText, org.scalatest.fixture.Transformer(testFun), Resources.ignoreCannotAppearInsideAnIn, sourceFileName, methodName, 4, -4, None, Some(pos), testTags: _*)
  }

  private def exceptionWasThrownInClauseMessageFun(verb: String, className: UnquotedString, description: String, errorMessage: String): String =
    verb match {
      case "when" => FailureMessages.exceptionWasThrownInWhenClause(Prettifier.default, className, description, errorMessage)
      case "which" => FailureMessages.exceptionWasThrownInWhichClause(Prettifier.default, className, description, errorMessage)
      case "that" => FailureMessages.exceptionWasThrownInThatClause(Prettifier.default, className, description, errorMessage)
      case "should" => FailureMessages.exceptionWasThrownInShouldClause(Prettifier.default, className, description, errorMessage)
      case "must" => FailureMessages.exceptionWasThrownInMustClause(Prettifier.default, className, description, errorMessage)
      case "can" => FailureMessages.exceptionWasThrownInCanClause(Prettifier.default, className, description, errorMessage)
    }

  private def rethrowIfCauseIsNAEOrDTNE(e: StackDepthException, pos: source.Position): Unit = 
    e.cause match {  
      case Some(c) if c.isInstanceOf[NotAllowedException] || c.isInstanceOf[DuplicateTestNameException] =>
        throw c 
      case _ => 
        throw new NotAllowedException(
          FailureMessages.assertionShouldBePutInsideItOrTheyClauseNotShouldMustWhenThatWhichOrCanClause, 
          Some(e), 
          e.position.getOrElse(pos)
        )
    }  

  private def registerBranch(description: String, childPrefix: Option[String], verb: String, methodName: String, stackDepth: Int, adjustment: Int, pos: source.Position, fun: () => Unit): Unit = {

    def registrationClosedMessageFun: String =
      verb match {
        case "should" => Resources.shouldCannotAppearInsideAnIn
        case "when" => Resources.whenCannotAppearInsideAnIn
        case "which" => Resources.whichCannotAppearInsideAnIn
        case "that" => Resources.thatCannotAppearInsideAnIn
        case "must" => Resources.mustCannotAppearInsideAnIn
        case "can" => Resources.canCannotAppearInsideAnIn
      }

    try {
      registerNestedBranch(description, childPrefix, fun(), registrationClosedMessageFun, sourceFileName, methodName, stackDepth, adjustment, None, Some(pos))
    }
    catch {
      case e: TestFailedException => rethrowIfCauseIsNAEOrDTNE(e, pos)
      case e: TestCanceledException => rethrowIfCauseIsNAEOrDTNE(e, pos)
      case nae: NotAllowedException => throw nae
      case trce: TestRegistrationClosedException => throw trce
      case e: DuplicateTestNameException => throw new NotAllowedException(exceptionWasThrownInClauseMessageFun(verb, UnquotedString(e.getClass.getName), description, e.getMessage), Some(e), e.position.getOrElse(pos))
      case other: Throwable if (!Suite.anExceptionThatShouldCauseAnAbort(other)) => throw new NotAllowedException(exceptionWasThrownInClauseMessageFun(verb, UnquotedString(other.getClass.getName), if (description.endsWith(" " + verb)) description.substring(0, description.length - (" " + verb).length) else description, other.getMessage), Some(other), pos)
      case other: Throwable => throw other
    }
  }

  private def registerShorthandBranch(childPrefix: Option[String], notAllowMessageFun: => String, methodName:String, stackDepth: Int, adjustment: Int, pos: source.Position, fun: () => Unit): Unit = {

    // Shorthand syntax only allow at top level, and only after "..." when, "..." should/can/must, or it should/can/must
    if (engine.currentBranchIsTrunk) {
      val currentBranch = engine.atomic.get.currentBranch
      // headOption because subNodes are in reverse order
      currentBranch.subNodes.headOption match {
        case Some(last) =>
          last match {
            case DescriptionBranch(_, descriptionText, _, _) =>

              def registrationClosedMessageFun: String =
                methodName match {
                  case "when" => Resources.whenCannotAppearInsideAnIn
                  case "which" => Resources.whichCannotAppearInsideAnIn
                  case "that" => Resources.thatCannotAppearInsideAnIn
                  case "should" => Resources.shouldCannotAppearInsideAnIn
                  case "must" => Resources.mustCannotAppearInsideAnIn
                  case "can" => Resources.canCannotAppearInsideAnIn
                }
              try {
                registerNestedBranch(descriptionText, childPrefix, fun(), registrationClosedMessageFun, "WordSpecLike.scala", methodName, stackDepth, adjustment, None, Some(pos))
              }
              catch {
                case e: TestFailedException => rethrowIfCauseIsNAEOrDTNE(e, pos)
                case e: TestCanceledException => rethrowIfCauseIsNAEOrDTNE(e, pos)
                case nae: NotAllowedException => throw nae
                case trce: TestRegistrationClosedException => throw trce
                case e: DuplicateTestNameException => throw new NotAllowedException(exceptionWasThrownInClauseMessageFun(methodName, UnquotedString(e.getClass.getName), descriptionText, e.getMessage), Some(e), e.position.getOrElse(pos))
                case other: Throwable if (!Suite.anExceptionThatShouldCauseAnAbort(other)) => throw new NotAllowedException(exceptionWasThrownInClauseMessageFun(methodName, UnquotedString(other.getClass.getName), if (descriptionText.endsWith(" " + methodName)) descriptionText.substring(0, descriptionText.length - (" " + methodName).length) else descriptionText, other.getMessage), Some(other), pos)
                case other: Throwable => throw other
              }

            case _ =>
              throw new NotAllowedException(notAllowMessageFun, pos)
          }
        case None =>
          throw new NotAllowedException(notAllowMessageFun, pos)
      }
    }
    else
      throw new NotAllowedException(notAllowMessageFun, pos)
  }

  /**
   * Class that supports the registration of tagged tests.
   *
   * <p>
   * Instances of this class are returned by the <code>taggedAs</code> method of
   * class <code>WordSpecStringWrapper</code>.
   * </p>
   *
   * @author Bill Venners
   */
  protected final class ResultOfTaggedAsInvocationOnString(specText: String, tags: List[Tag]) {

    /**
     * Supports tagged test registration.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "complain on peek" taggedAs(SlowTest) in { fixture => ... }
     *                                       ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="FixtureAnyWordSpec.html">main documentation</a> for trait <code>FixtureAnyWordSpec</code>.
     * </p>
     *
     * @param testFun the test function
     */
    // SKIP-DOTTY-START 
    def in(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToRun(specText, tags, "in", testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def in(testFun: FixtureParam => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToRun(specText, tags, "in", testFun, pos) }) } 
    //DOTTY-ONLY }      

    /**
     * Supports tagged test registration, for tests that don't take a fixture.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "complain on peek" taggedAs(SlowTest) in { () => ... }
     *                                       ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="FixtureAnyWordSpec.html">main documentation</a> for trait <code>FixtureAnyWordSpec</code>.
     * </p>
     *
     * @param testFun the test function
     */
    // SKIP-DOTTY-START 
    def in(testFun: () => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToRun(specText, tags, "in", new org.scalatest.fixture.NoArgTestWrapper(testFun), pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def in(testFun: () => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToRun(specText, tags, "in", new org.scalatest.fixture.NoArgTestWrapper(testFun), pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports registration of tagged, pending tests.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "complain on peek" taggedAs(SlowTest) is (pending)
     *                                       ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="FixtureAnyWordSpec.html">main documentation</a> for trait <code>FixtureAnyWordSpec</code>.
     * </p>
     *
     * @param testFun the test function
     */
    // SKIP-DOTTY-START
    def is(testFun: => PendingStatement)(implicit pos: source.Position): Unit = {
      registerPendingTestToRun(specText, tags, "is", unusedFixtureParam => testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def is(testFun: => PendingStatement): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerPendingTestToRun(specText, tags, "is", unusedFixtureParam => testFun, pos)}) } 
    //DOTTY-ONLY }

    /**
     * Supports registration of tagged, ignored tests.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "complain on peek" taggedAs(SlowTest) ignore { fixture => ... }
     *                                       ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="FixtureAnyWordSpec.html">main documentation</a> for trait <code>FixtureAnyWordSpec</code>.
     * </p>
     *
     * @param testFun the test function
     */
    // SKIP-DOTTY-START
    def ignore(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToIgnore(specText, tags, "ignore", testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def ignore(testFun: FixtureParam => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToIgnore(specText, tags, "ignore", testFun, pos)}) } 
    //DOTTY-ONLY }

    private final def ignoreImpl(testFun: () => Any /* Assertion */, pos: source.Position): Unit = {
      registerTestToIgnore(specText, tags, "ignore", new org.scalatest.fixture.NoArgTestWrapper(testFun), pos)
    }

    /**
     * Supports registration of tagged, ignored tests that take no fixture parameter.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "complain on peek" taggedAs(SlowTest) ignore { () => ... }
     *                                       ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="FixtureAnyWordSpec.html">main documentation</a> for trait <code>FixtureAnyWordSpec</code>.
     * </p>
     *
     * @param testFun the test function
     */
    // SKIP-DOTTY-START
    def ignore(testFun: () => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      ignoreImpl(testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def ignore(testFun: () => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => ignoreImpl(testFun, pos) }) } 
    //DOTTY-ONLY }
  }

  /**
   * A class that via an implicit conversion (named <code>convertToWordSpecStringWrapper</code>) enables
   * methods <code>when</code>, <code>which</code>, <code>in</code>, <code>is</code>, <code>taggedAs</code>
   * and <code>ignore</code> to be invoked on <code>String</code>s.
   *
   * <p>
   * This class provides much of the syntax for <code>FixtureAnyWordSpec</code>, however, it does not add
   * the verb methods (<code>should</code>, <code>must</code>, and <code>can</code>) to <code>String</code>.
   * Instead, these are added via the <code>ShouldVerb</code>, <code>MustVerb</code>, and <code>CanVerb</code>
   * traits, which <code>FixtureAnyWordSpec</code> mixes in, to avoid a conflict with implicit conversions provided
   * in <code>Matchers</code> and <code>MustMatchers</code>.
   * </p>
   *
   * @param string the string that is wrapped
   *
   * @author Bill Venners
   */
  protected final class WordSpecStringWrapper(string: String) {

    /**
     * Supports test registration.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "complain on peek" in { fixture => ... }
     *                    ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="FixtureAnyWordSpec.html">main documentation</a> for trait <code>FixtureAnyWordSpec</code>.
     * </p>
     *
     * @param testFun the test function
     */
    // SKIP-DOTTY-START
    def in(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToRun(string, List(), "in", testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def in(testFun: FixtureParam => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToRun(string, List(), "in", testFun, pos)}) } 
    //DOTTY-ONLY }

    private final def inImpl(testFun: () => Any /* Assertion */, pos: source.Position): Unit = {
      registerTestToRun(string, List(), "in", new org.scalatest.fixture.NoArgTestWrapper(testFun), pos)
    }

    /**
     * Supports registration of tests that take no fixture.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "complain on peek" in { () => ... }
     *                    ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="FixtureAnyWordSpec.html">main documentation</a> for trait <code>FixtureAnyWordSpec</code>.
     * </p>
     *
     * @param testFun the test function
     */
    // SKIP-DOTTY-START
    def in(testFun: () => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      inImpl(testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def in(testFun: () => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => inImpl(testFun, pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports pending test registration.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "complain on peek" is (pending)
     *                    ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="FixtureAnyWordSpec.html">main documentation</a> for trait <code>FixtureAnyWordSpec</code>.
     * </p>
     *
     * @param testFun the test function
     */
    // SKIP-DOTTY-START 
    def is(testFun: => PendingStatement)(implicit pos: source.Position): Unit = {
      registerPendingTestToRun(string, List(), "is", unusedFixtureParam => testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def is(testFun: => PendingStatement): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerPendingTestToRun(string, List(), "is", unusedFixtureParam => testFun, pos)}) } 
    //DOTTY-ONLY }

    /**
     * Supports ignored test registration.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "complain on peek" ignore { fixture => ... }
     *                    ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="FixtureAnyWordSpec.html">main documentation</a> for trait <code>FixtureAnyWordSpec</code>.
     * </p>
     *
     * @param testFun the test function
     */
    // SKIP-DOTTY-START
    def ignore(testFun: FixtureParam => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      registerTestToIgnore(string, List(), "ignore", testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def ignore(testFun: FixtureParam => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerTestToIgnore(string, List(), "ignore", testFun, pos)}) } 
    //DOTTY-ONLY }

    private final def ignoreImpl(testFun: () => Any /* Assertion */, pos: source.Position): Unit = {
      registerTestToIgnore(string, List(), "ignore", new org.scalatest.fixture.NoArgTestWrapper(testFun), pos)
    }

    /**
     * Supports registration of ignored tests that take no fixture.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "complain on peek" ignore { () => ... }
     *                    ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="FixtureAnyWordSpec.html">main documentation</a> for trait <code>FixtureAnyWordSpec</code>.
     * </p>
     *
     * @param testFun the test function
     */
    // SKIP-DOTTY-START
    def ignore(testFun: () => Any /* Assertion */)(implicit pos: source.Position): Unit = {
      ignoreImpl(testFun, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def ignore(testFun: () => Any /* Assertion */): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => ignoreImpl(testFun, pos) }) } 
    //DOTTY-ONLY }

    /**
     * Supports tagged test registration.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "complain on peek" taggedAs(SlowTest) in { fixture => ... }
     *                    ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="FixtureAnyWordSpec.html">main documentation</a> for trait <code>FixtureAnyWordSpec</code>.
     * </p>
     *
     * @param firstTestTag the first mandatory test tag
     * @param otherTestTags the others additional test tags
     * @return an new instance of <code>ResultOfTaggedAsInvocationOnString</code>
     */
    //DOTTY-ONLY     infix  
    def taggedAs(firstTestTag: Tag, otherTestTags: Tag*) = {
      val tagList = firstTestTag :: otherTestTags.toList
      new ResultOfTaggedAsInvocationOnString(string, tagList)
    }

    /**
     * Registers a <code>when</code> clause.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" when { ... }
     *           ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="FixtureAnyWordSpec.html">main documentation</a> for trait <code>FixtureAnyWordSpec</code>.
     * </p>
     *
     * @param f the function which is the body of the scope
     */
    // SKIP-DOTTY-START
    def when(f: => Unit)(implicit pos: source.Position): Unit = {
      // SKIP-SCALATESTJS,NATIVE-START
      val stackDepth = 4
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val stackDepth = 6
      registerBranch(string, Some("when"), "when", "when", stackDepth, -2, pos, () => f)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def when(f: => Unit): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerBranch(string, Some("when"), "when", "when", 4, -2, pos, () => f)}) } 
    //DOTTY-ONLY }

    /**
     * Registers a <code>when</code> clause that is followed by an <em>after word</em>.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * val theUser = afterWord("the user")
     *
     * "A Stack" when theUser { ... }
     *           ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="FixtureAnyWordSpec.html">main documentation</a> for trait <code>FixtureAnyWordSpec</code>.
     * </p>
     *
     * @param resultOfAfterWordApplication a <code>ResultOfAfterWordApplication</code>
     */
    // SKIP-DOTTY-START
    def when(resultOfAfterWordApplication: ResultOfAfterWordApplication)(implicit pos: source.Position): Unit = {
      registerBranch(string, Some("when " + resultOfAfterWordApplication.text), "when", "when", 4, -2, pos, resultOfAfterWordApplication.f)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def when(resultOfAfterWordApplication: ResultOfAfterWordApplication): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerBranch(string, Some("when " + resultOfAfterWordApplication.text), "when", "when", 4, -2, pos, resultOfAfterWordApplication.f)}) } 
    //DOTTY-ONLY }

    /**
     * Registers a <code>that</code> clause.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "a rerun button" that {
     *                  ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="FixtureAnyWordSpec.html">main documentation</a> for trait <code>FixtureAnyWordSpec</code>.
     * </p>
     *
     * @param f the function which is the body of the scope
     */
    // SKIP-DOTTY-START
    def that(f: => Unit)(implicit pos: source.Position): Unit = {
      // SKIP-SCALATESTJS,NATIVE-START
      val stackDepth = 4
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val stackDepth = 6
      registerBranch(string.trim + " that", None, "that", "that", stackDepth, -2, pos, () => f)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def that(f: => Unit): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerBranch(string.trim + " that", None, "that", "that", stackDepth, -2, pos, () => f)}) } 
    //DOTTY-ONLY }

    /**
     * Registers a <code>which</code> clause.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "a rerun button," which {
     *                  ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="FixtureAnyWordSpec.html">main documentation</a> for trait <code>FixtureAnyWordSpec</code>.
     * </p>
     *
     * @param f the function which is the body of the scope
     */
    // SKIP-DOTTY-START
    def which(f: => Unit)(implicit pos: source.Position): Unit = {
      // SKIP-SCALATESTJS,NATIVE-START
      val stackDepth = 4
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY val stackDepth = 6
      registerBranch(string.trim + " which", None, "which", "which", stackDepth, -2, pos, () => f)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def which(f: => Unit): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerBranch(string.trim + " which", None, "which", "which", stackDepth, -2, pos, () => f)}) } 
    //DOTTY-ONLY }

    /**
     * Registers a <code>that</code> clause.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "a rerun button," that {
     *                  ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="FixtureAnyWordSpec.html">main documentation</a> for trait <code>FixtureAnyWordSpec</code>.
     * </p>
     *
     * @param resultOfAfterWordApplication a <code>ResultOfAfterWordApplication</code>
     */
    // SKIP-DOTTY-START
    def that(resultOfAfterWordApplication: ResultOfAfterWordApplication)(implicit pos: source.Position): Unit = {
      registerBranch(string.trim + " that " + resultOfAfterWordApplication.text.trim, None, "that", "that", 4, -2, pos, resultOfAfterWordApplication.f)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def that(resultOfAfterWordApplication: ResultOfAfterWordApplication): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerBranch(string.trim + " that " + resultOfAfterWordApplication.text.trim, None, "that", "that", 4, -2, pos, resultOfAfterWordApplication.f)}) } 
    //DOTTY-ONLY }

    /**
     * Registers a <code>which</code> clause.
     *
     * <p>
     * For example, this method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "a rerun button," which {
     *                  ^
     * </pre>
     *
     * <p>
     * For more information and examples of this method's use, see the <a href="FixtureAnyWordSpec.html">main documentation</a> for trait <code>FixtureAnyWordSpec</code>.
     * </p>
     *
     * @param resultOfAfterWordApplication a <code>ResultOfAfterWordApplication</code>
     */
    // SKIP-DOTTY-START
    def which(resultOfAfterWordApplication: ResultOfAfterWordApplication)(implicit pos: source.Position): Unit = {
      registerBranch(string.trim + " which " + resultOfAfterWordApplication.text.trim, None, "which", "which", 4, -2, pos, resultOfAfterWordApplication.f)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def which(resultOfAfterWordApplication: ResultOfAfterWordApplication): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => registerBranch(string.trim + " which " + resultOfAfterWordApplication.text.trim, None, "which", "which", 4, -2, pos, resultOfAfterWordApplication.f)}) } 
    //DOTTY-ONLY }
  }

  /**
   * Class whose instances are <em>after word</em>s, which can be used to reduce text duplication.
   *
   * <p>
   * If you are repeating a word or phrase at the beginning of each string inside
   * a block, you can "move the word or phrase" out of the block with an after word.
   * You create an after word by passing the repeated word or phrase to the <code>afterWord</code> method.
   * Once created, you can place the after word after <code>when</code>, a verb
   * (<code>should</code>, <code>must</code>, or <code>can</code>), or
   * <code>which</code>. (You can't place one after <code>in</code> or <code>is</code>, the
   * words that introduce a test.) Here's an example that has after words used in all three
   * places:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest._
   * import ConfigMapFixture
   *
   * class ScalaTestGUISpec extends wordspec.FixtureAnyWordSpec with ConfigMapFixture {
   *
   *   def theUser = afterWord("the user")
   *   def display = afterWord("display")
   *   def is = afterWord("is")
   *
   *   "The ScalaTest GUI" when theUser {
   *     "clicks on an event report in the list box" should display {
   *       "a blue background in the clicked-on row in the list box" in { cm =&gt; }
   *       "the details for the event in the details area" in { cm =&gt; }
   *       "a rerun button," which is {
   *         "enabled if the clicked-on event is rerunnable" in { cm =&gt; }
   *         "disabled if the clicked-on event is not rerunnable" in { cm =&gt; }
   *       }
   *     }
   *   }
   * }
   * </pre>
   *
   * <p>
   * Running the previous <code>FixtureAnyWordSpec</code> in the Scala interpreter would yield:
   * </p>
   *
   * <pre class="stREPL">
   * scala> (new ScalaTestGUISpec).run()
   * <span class="stGreen">The ScalaTest GUI (when the user clicks on an event report in the list box)
   * - should display a blue background in the clicked-on row in the list box
   * - should display the details for the event in the details area
   * - should display a rerun button, which is enabled if the clicked-on event is rerunnable
   * - should display a rerun button, which is disabled if the clicked-on event is not rerunnable</span>
   * </pre>
   *
   * @param text the afterword text
   */
  protected final class AfterWord(text: String) {

    /**
     * Supports the use of <em>after words</em>.
     *
     * <p>
     * This method transforms a block of code into a <code>ResultOfAfterWordApplication</code>, which
     * is accepted by <code>when</code>, <code>should</code>, <code>must</code>, <code>can</code>, and <code>which</code>
     * methods.  For more information, see the <a href="AnyWordSpec.html#AfterWords">main documentation</code></a> for trait <code>org.scalatest.wordspec.AnyWordSpec</code>.
     * </p>
     *
     * @param f the function to be transformed into <code>ResultOfAfterWordApplication</code>
     * @return an new instance of <code>ResultOfAfterWordApplication</code>
     */
    def apply(f: => Unit) = new ResultOfAfterWordApplication(text, () => f)
  }

  /**
   * Creates an <em>after word</em> that an be used to reduce text duplication.
   *
   * <p>
   * If you are repeating a word or phrase at the beginning of each string inside
   * a block, you can "move the word or phrase" out of the block with an after word.
   * You create an after word by passing the repeated word or phrase to the <code>afterWord</code> method.
   * Once created, you can place the after word after <code>when</code>, a verb
   * (<code>should</code>, <code>must</code>, or <code>can</code>), or
   * <code>which</code>. (You can't place one after <code>in</code> or <code>is</code>, the
   * words that introduce a test.) Here's an example that has after words used in all three
   * places:
   * </p>
   *
   * <pre class="stHighlight">
   * import org.scalatest._
   * import ConfigMapFixture
   *
   * class ScalaTestGUISpec extends wordspec.FixtureAnyWordSpec with ConfigMapFixture {
   *
   *   def theUser = afterWord("the user")
   *   def display = afterWord("display")
   *   def is = afterWord("is")
   *
   *   "The ScalaTest GUI" when theUser {
   *     "clicks on an event report in the list box" should display {
   *       "a blue background in the clicked-on row in the list box" in { cm =&gt; }
   *       "the details for the event in the details area" in { cm =&gt; }
   *       "a rerun button," which is {
   *         "enabled if the clicked-on event is rerunnable" in { cm =&gt; }
   *         "disabled if the clicked-on event is not rerunnable" in { cm =&gt; }
   *       }
   *     }
   *   }
   * }
   * </pre>
   *
   * <p>
   * Running the previous <code>FixtureAnyWordSpec</code> in the Scala interpreter would yield:
   * </p>
   *
   * <pre class="stREPL">
   * scala> (new ScalaTestGUISpec).run()
   * <span class="stGreen">The ScalaTest GUI (when the user clicks on an event report in the list box)
   * - should display a blue background in the clicked-on row in the list box
   * - should display the details for the event in the details area
   * - should display a rerun button, which is enabled if the clicked-on event is rerunnable
   * - should display a rerun button, which is disabled if the clicked-on event is not rerunnable</span>
   * </pre>
   *
   * @param text the after word text
   * @return an instance of <code>AfterWord</code>
   */
  protected def afterWord(text: String) = new AfterWord(text)

  // SKIP-SCALATESTJS,NATIVE-START
  private[scalatest] val stackDepth = 3
  // SKIP-SCALATESTJS,NATIVE-END
  //SCALATESTJS,NATIVE-ONLY private[scalatest] val stackDepth: Int = 10

  /**
   * Class that supports shorthand scope registration via the instance referenced from <code>FixtureAnyWordSpecLike</code>'s <code>it</code> field.
   *
   * <p>
   * This class enables syntax such as the following test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * "A Stack" when { ... }
   *
   * it should { ... }
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field, see the main documentation
   * for <code>FixtureAnyWordSpec</code>.
   * </p>
   */
  protected final class ItWord {

    private final def shouldImpl(right: => Unit, pos: source.Position): Unit = {
      registerShorthandBranch(Some("should"), Resources.itMustAppearAfterTopLevelSubject, "should", stackDepth, -2, pos, () => right)
    } 

    /**
     * Supports the registration of scope with <code>should</code> in a <code>FixtureAnyWordSpecLike</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" when { ... }
     *
     * it should { ... }
     *    ^
     * </pre>
     *
     * <p>
     * For examples of scope registration, see the <a href="AnyWordSpec.html">main documentation</a>
     * for <code>AnyWordSpec</code>.
     * </p>
     *
     * @param right the body function
     */
    // SKIP-DOTTY-START
    def should(right: => Unit)(implicit pos: source.Position): Unit = {
      shouldImpl(right, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def should(right: => Unit): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => shouldImpl(right, pos) }) } 
    //DOTTY-ONLY }

    private final def mustImpl(right: => Unit, pos: source.Position): Unit = {
      registerShorthandBranch(Some("must"), Resources.itMustAppearAfterTopLevelSubject, "must", stackDepth, -2, pos, () => right)
    }

    /**
     * Supports the registration of scope with <code>must</code> in a <code>FixtureAnyWordSpecLike</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" when { ... }
     *
     * it must { ... }
     *    ^
     * </pre>
     *
     * <p>
     * For examples of scope registration, see the <a href="AnyWordSpec.html">main documentation</a>
     * for <code>AnyWordSpec</code>.
     * </p>
     *
     * @param right the body function
     */
    // SKIP-DOTTY-START
    def must(right: => Unit)(implicit pos: source.Position): Unit = {
      mustImpl(right, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def must(right: => Unit): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => mustImpl(right, pos) }) } 
    //DOTTY-ONLY }

    private final def canImpl(right: => Unit, pos: source.Position): Unit = {
      registerShorthandBranch(Some("can"), Resources.itMustAppearAfterTopLevelSubject, "can", stackDepth, -2, pos, () => right)
    }

    /**
     * Supports the registration of scope with <code>can</code> in a <code>FixtureAnyWordSpecLike</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" when { ... }
     *
     * it can { ... }
     *    ^
     * </pre>
     *
     * <p>
     * For examples of scope registration, see the <a href="AnyWordSpec.html">main documentation</a>
     * for <code>AnyWordSpec</code>.
     * </p>
     *
     * @param right the body function
     */
    // SKIP-DOTTY-START
    def can(right: => Unit)(implicit pos: source.Position): Unit = {
      canImpl(right, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def can(right: => Unit): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => canImpl(right, pos) }) } 
    //DOTTY-ONLY }

    private final def whenImpl(right: => Unit, pos: source.Position): Unit = {
      registerShorthandBranch(Some("when"), Resources.itMustAppearAfterTopLevelSubject, "when", stackDepth, -2, pos, () => right)
    }

    /**
     * Supports the registration of scope with <code>when</code> in a <code>FixtureAnyWordSpecLike</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "A Stack" should { ... }
     *
     * it when { ... }
     *    ^
     * </pre>
     *
     * <p>
     * For examples of scope registration, see the <a href="AnyWordSpec.html">main documentation</a>
     * for <code>AnyWordSpec</code>.
     * </p>
     *
     * @param right the body function
     */
    // SKIP-DOTTY-START
    def when(right: => Unit)(implicit pos: source.Position): Unit = {
      whenImpl(right, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def when(right: => Unit): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => whenImpl(right, pos) }) } 
    //DOTTY-ONLY }
  }

  /**
   * Supports shorthand scope registration in <code>FixtureAnyWordSpecLike</code>s.
   *
   * <p>
   * This field enables syntax such as the following test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * "A Stack" when { ... }
   *
   * it should { ... }
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>it</code> field, see the main documentation
   * for <code>AnyWordSpec</code>.
   * </p>
   */
  protected val it = new ItWord

  /**
   * Class that supports shorthand scope registration via the instance referenced from <code>FixtureAnyWordSpecLike</code>'s <code>they</code> field.
   *
   * <p>
   * This class enables syntax such as the following test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * "Basketball players" when { ... }
   *
   * they should { ... }
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>they</code> field, see the main documentation
   * for <code>AnyWordSpec</code>.
   * </p>
   */
  protected final class TheyWord {

    private final def shouldImpl(right: => Unit, pos: source.Position): Unit = {
      registerShorthandBranch(Some("should"), Resources.theyMustAppearAfterTopLevelSubject, "should", stackDepth, -2, pos, () => right)
    }

    /**
     * Supports the registration of scope with <code>should</code> in a <code>FixtureAnyWordSpecLike</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "Basketball players" when { ... }
     *
     * they should { ... }
     *      ^
     * </pre>
     *
     * <p>
     * For examples of scope registration, see the <a href="AnyWordSpec.html">main documentation</a>
     * for <code>AnyWordSpec</code>.
     * </p>
     *
     * @param right the body function
     */
    // SKIP-DOTTY-START
    def should(right: => Unit)(implicit pos: source.Position): Unit = {
      shouldImpl(right, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def should(right: => Unit): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => shouldImpl(right, pos) }) } 
    //DOTTY-ONLY }

    private final def mustImpl(right: => Unit, pos: source.Position): Unit = {
      registerShorthandBranch(Some("must"), Resources.theyMustAppearAfterTopLevelSubject, "must", stackDepth, -2, pos, () => right)
    }

    /**
     * Supports the registration of scope with <code>must</code> in a <code>FixtureAnyWordSpecLike</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "Basketball players" when { ... }
     *
     * they must { ... }
     *      ^
     * </pre>
     *
     * <p>
     * For examples of scope registration, see the <a href="AnyWordSpec.html">main documentation</a>
     * for <code>AnyWordSpec</code>.
     * </p>
     *
     * @param right the body function
     */
    // SKIP-DOTTY-START
    def must(right: => Unit)(implicit pos: source.Position): Unit = {
      mustImpl(right, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def must(right: => Unit): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => mustImpl(right, pos) }) } 
    //DOTTY-ONLY }

    private final def canImpl(right: => Unit, pos: source.Position): Unit = {
      registerShorthandBranch(Some("can"), Resources.theyMustAppearAfterTopLevelSubject, "can", stackDepth, -2, pos, () => right)
    }

    /**
     * Supports the registration of scope with <code>can</code> in a <code>FixtureAnyWordSpecLike</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "Basketball players" when { ... }
     *
     * they can { ... }
     *      ^
     * </pre>
     *
     * <p>
     * For examples of scope registration, see the <a href="AnyWordSpec.html">main documentation</a>
     * for <code>AnyWordSpec</code>.
     * </p>
     *
     * @param right the body function
     */
    // SKIP-DOTTY-START
    def can(right: => Unit)(implicit pos: source.Position): Unit = {
      canImpl(right, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def can(right: => Unit): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => canImpl(right, pos) }) } 
    //DOTTY-ONLY }

    private final def whenImpl(right: => Unit, pos: source.Position): Unit = {
      registerShorthandBranch(Some("when"), Resources.theyMustAppearAfterTopLevelSubject, "when", stackDepth, -2, pos, () => right)
    }

    /**
     * Supports the registration of scope with <code>when</code> in a <code>FixtureAnyWordSpecLike</code>.
     *
     * <p>
     * This method supports syntax such as the following:
     * </p>
     *
     * <pre class="stHighlight">
     * "Basketball players" should { ... }
     *
     * they when { ... }
     *      ^
     * </pre>
     *
     * <p>
     * For examples of scope registration, see the <a href="AnyWordSpec.html">main documentation</a>
     * for <code>AnyWordSpec</code>.
     * </p>
     *
     * @param right the body function
     */
    // SKIP-DOTTY-START
    def when(right: => Unit)(implicit pos: source.Position): Unit = {
      whenImpl(right, pos)
    }
    // SKIP-DOTTY-END
    //DOTTY-ONLY inline infix def when(right: => Unit): Unit = {
    //DOTTY-ONLY   ${ source.Position.withPosition[Unit]('{(pos: source.Position) => whenImpl(right, pos) }) } 
    //DOTTY-ONLY }
  }

  /**
   * Supports shorthand scope registration in <code>FixtureAnyWordSpecLike</code>s.
   *
   * <p>
   * This field enables syntax such as the following test registration:
   * </p>
   *
   * <pre class="stHighlight">
   * "A Stack" when { ... }
   *
   * they should { ... }
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of the <code>they</code> field, see the main documentation
   * for <code>AnyWordSpec</code>.
   * </p>
   */
  protected val they = new TheyWord

  import scala.language.implicitConversions

  /**
   * Implicitly converts <code>String</code>s to <code>WordSpecStringWrapper</code>, which enables
   * methods <code>when</code>, <code>which</code>, <code>in</code>, <code>is</code>, <code>taggedAs</code>
   * and <code>ignore</code> to be invoked on <code>String</code>s.
   *
   * @param s <code>String</code> to be wrapped
   * @return an instance of <code>WordSpecStringWrapper</code>
   */
  protected implicit def convertToWordSpecStringWrapper(s: String): WordSpecStringWrapper = new WordSpecStringWrapper(s)

  /**
   * Supports the registration of subjects.
   *
   * <p>
   * For example, this method enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * "A Stack" should { ...
   *           ^
   * </pre>
   *
   * <p>
   * This function is passed as an implicit parameter to a <code>should</code> method
   * provided in <code>ShouldVerb</code>, a <code>must</code> method
   * provided in <code>MustVerb</code>, and a <code>can</code> method
   * provided in <code>CanVerb</code>. When invoked, this function registers the
   * subject and executes the block.
   * </p>
   */
  protected implicit val subjectRegistrationFunction: StringVerbBlockRegistration =
    new StringVerbBlockRegistration {
      def apply(left: String, verb: String, pos: source.Position, f: () => Unit): Unit = registerBranch(left, Some(verb), verb, "apply", 6, -2, pos, f)
    }

  /**
   * Supports the registration of subject descriptions with after words.
   *
   * <p>
   * For example, this method enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * def provide = afterWord("provide")
   *
   * "The ScalaTest Matchers DSL" can provide { ... }
   *                              ^
   * </pre>
   *
   * <p>
   * This function is passed as an implicit parameter to a <code>should</code> method
   * provided in <code>ShouldVerb</code>, a <code>must</code> method
   * provided in <code>MustVerb</code>, and a <code>can</code> method
   * provided in <code>CanVerb</code>. When invoked, this function registers the
   * subject and executes the block.
   * </p>
   */
  protected implicit val subjectWithAfterWordRegistrationFunction: SubjectWithAfterWordRegistration =
    new SubjectWithAfterWordRegistration {
      def apply(left: String, verb: String, resultOfAfterWordApplication: ResultOfAfterWordApplication, pos: source.Position): Unit = {
        val afterWordFunction =
          () => {
            // SKIP-SCALATESTJS,NATIVE-START
            val stackDepth = 10
            // SKIP-SCALATESTJS,NATIVE-END
            //SCALATESTJS,NATIVE-ONLY val stackDepth = 15
            registerBranch(resultOfAfterWordApplication.text, None, verb, "apply", stackDepth, -2, pos, resultOfAfterWordApplication.f)
          }
        // SKIP-SCALATESTJS,NATIVE-START
        val stackDepth = 7
        // SKIP-SCALATESTJS,NATIVE-END
        //SCALATESTJS,NATIVE-ONLY val stackDepth = 9
        registerBranch(left, Some(verb), verb, "apply", stackDepth, -2, pos, afterWordFunction)
      }
    }

  /**
   * A <code>Map</code> whose keys are <code>String</code> tag names to which tests in this <code>FixtureAnyWordSpec</code> belong, and values
   * the <code>Set</code> of test names that belong to each tag. If this <code>FixtureAnyWordSpec</code> contains no tags, this method returns an empty <code>Map</code>.
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
   * Run a test. This trait's implementation runs the test registered with the name specified by
   * <code>testName</code>. Each test's name is a concatenation of the text of all describers surrounding a test,
   * from outside in, and the test's  spec text, with one space placed between each item. (See the documenation
   * for <code>testNames</code> for an example.)
   *
   * @param testName the name of one test to execute.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when the test started by this method has completed, and whether or not it failed .
   * @throws NullArgumentException if any of <code>testName</code> or <code>args</code> is <code>null</code>.
   */
  protected override def runTest(testName: String, args: Args): Status = {

    def invokeWithFixture(theTest: TestLeaf): Outcome = {
      theTest.testFun match {
        case transformer: org.scalatest.fixture.Transformer[_] =>
          transformer.exceptionalTestFun match {
            case wrapper: org.scalatest.fixture.NoArgTestWrapper[_, _] =>
              withFixture(new FixturelessTestFunAndConfigMap(testName, wrapper.test, args.configMap))
            case fun => withFixture(new TestFunAndConfigMap(testName, fun, args.configMap))
          }
        case other =>
          other match {
            case wrapper: org.scalatest.fixture.NoArgTestWrapper[_, _] =>
              withFixture(new FixturelessTestFunAndConfigMap(testName, wrapper.test, args.configMap))
            case fun => withFixture(new TestFunAndConfigMap(testName, fun, args.configMap))
          }
      }
    }

    runTestImpl(thisSuite, testName, args, true, invokeWithFixture)
  }

  /**
   * <p>
   * Run zero to many of this <code>FixtureAnyWordSpec</code>'s tests.
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
   *                 I.e., <code>None</code> acts like a wildcard that means execute all relevant tests in this <code>FixtureAnyWordSpec</code>.
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when all tests started by this method have completed, and whether or not a failure occurred.
   * @throws NullArgumentException if any of <code>testName</code> or <code>args</code> is <code>null</code>.
   */
  protected override def runTests(testName: Option[String], args: Args): Status = {
    runTestsImpl(thisSuite, testName, args, info, true, runTest)
  }

  /**
   * An immutable <code>Set</code> of test names. If this <code>FixtureAnyWordSpec</code> contains no tests, this method returns an
   * empty <code>Set</code>.
   *
   * <p>
   * This trait's implementation of this method will return a set that contains the names of all registered tests. The set's
   * iterator will return those names in the order in which the tests were registered. Each test's name is composed
   * of the concatenation of the text of each surrounding describer, in order from outside in, and the text of the
   * example itself, with all components separated by a space.
   * </p>
   *
   * @return the <code>Set</code> of test names
   */
  override def testNames: Set[String] = {
    InsertionOrderSet(atomic.get.testNamesList)
  }

  override def run(testName: Option[String], args: Args): Status = {
    runImpl(thisSuite, testName, args, super.run)
  }

  /**
   * Supports shared test registration in <code>FixtureAnyWordSpec</code>s.
   *
   * <p>
   * This field enables syntax such as the following:
   * </p>
   *
   * <pre class="stHighlight">
   * behave like nonFullStack(stackWithOneItem)
   * ^
   * </pre>
   *
   * <p>
   * For more information and examples of the use of <cod>behave</code>, see the <a href="AnyWordSpec.html#SharedTests">Shared tests section</a>
   * in the main documentation for trait <code>org.scalatest.wordspec.AnyWordSpec</code>.
   * </p>
   */
  protected val behave = new BehaveWord

  override def testDataFor(testName: String, theConfigMap: ConfigMap = ConfigMap.empty): TestData = createTestDataFor(testName, theConfigMap, this)
}
