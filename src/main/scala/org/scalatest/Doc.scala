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

import scala.xml.Elem
import Suite.reportMarkupProvided
import Doc.stripMargin
import Doc.trimMarkup
import java.util.concurrent.atomic.AtomicReference
import java.util.ConcurrentModificationException
import collection.mutable.ListBuffer

/**
 * A <code>Doc</code> class that takes one XML node markup
 *  which will be returned from its <code>nestedSuites</code> method.
 *
 * <p>
 * For example, you can define a suite that always executes a list of
 * nested suites like this:
 * </p>
 *
 * <pre class="stHighlight">
 * class StepsSuite extends Suites(
 *   new Step1Suite,
 *   new Step2Suite,
 *   new Step3Suite,
 *   new Step4Suite,
 *   new Step5Suite
 * )
 * </pre>
 *
 * <p>
 * When <code>StepsSuite</code> is executed, it will execute its
 * nested suites in the passed order: <code>Step1Suite</code>, <code>Step2Suite</code>,
 * <code>Step3Suite</code>, <code>Step4Suite</code>, and <code>Step5Suite</code>.
 * </p>
 *
 * @param suitesToNest a sequence of <code>Suite</code>s to nest.
 *
 * @throws NullPointerException if <code>suitesToNest</code>, or any suite
 * it contains, is <code>null</code>.
 *
 * @author Bill Venners
 */
private[scalatest] trait Doc extends Suite { thisDoc =>

  // If registrationThreadName is None, then registration isn't open
  // and include calls will return what they are supposed to return, but
  // won't have the side effect of registering the suites and tests to run.
  private class Bundle private(
    val registrationThreadName: Option[String],
    val registeredSuites: Map[String, Suite]
  ) {
    def unpack = (registrationThreadName, registeredSuites)
  }

  private object Bundle {
    def apply(
      registrationThreadName: Option[String],
      registeredSuites: Map[String, Suite]
    ): Bundle =
      new Bundle(registrationThreadName, registeredSuites)
  }

  private final val atomic = new AtomicReference[Bundle](Bundle(None, Map()))

  private def updateAtomic(oldBundle: Bundle, newBundle: Bundle) {
    val shouldBeOldBundle = atomic.getAndSet(newBundle)
    if (!(shouldBeOldBundle eq oldBundle))
      throw new ConcurrentModificationException("concurrentDocSpecMod")
  }

  // TODO: Make it atomic
  // TODO: A test that throws NotAllowedE if it is called twice
  // TODO: A test that throws TestRegistrationClosedE if it is called after run has been called
  private var bodyText: Option[String] = None
  def body(elem: Elem) {
    bodyText = Some(elem.text)
  }

  protected def include(suite: Suite): String = {

      val oldBundle = atomic.get
      var (registrationThreadName, registeredSuites) = oldBundle.unpack
// TODO: register two instances of the same class, which will break this key
      registeredSuites += (suite.getClass.getName -> suite) // TODO: Check thread name, do nothing if None or non-matching
      updateAtomic(oldBundle, Bundle(registrationThreadName, registeredSuites))
    "\ninclude[" + suite.getClass.getName + "]\n"
  }

  // TODO write a test to ensure you get a proper exception when 
  // body is not called
  private lazy val snippets: Vector[Snippet] = getSnippets(bodyText.get)

  /*
   * Returns a list containing the suites mentioned in the body XML element,
   * in the order they were mentioned.
   */
  final override lazy val nestedSuites: collection.immutable.IndexedSeq[Suite] = for (IncludedSuite(suite) <- snippets) yield suite
/*
println("^^^^^^^^^^^")
println(body.text)
println("###########")
println(snippets)
println("&&&&&&&&&&&")
*/

/*
  override protected def runNestedSuites(reporter: Reporter, stopper: Stopper, filter: Filter,
      configMap: ConfigMap, distributor: Option[Distributor], tracker: Tracker) {
    reportMarkupProvided(thisDoc, reporter, tracker, None, trimMarkup(stripMargin(body.text)), 0, true, None, None)
  }
*/
  override protected def runNestedSuites(args: Args): Status = {

    import args._
    
    val statusBuffer = new ListBuffer[Status]()

    val (_, registeredSuites) = atomic.get.unpack
    snippets foreach {
      case Markup(text) => 
        reportMarkupProvided(thisDoc, reporter, tracker, None, trimMarkup(stripMargin(text)), 0, None, true)
      case IncludedSuite(suite) =>
        println("Send SuiteStarting ... ")  // TODO: Why is runTestedSuites even here?
        statusBuffer += suite.run(None, args)
        println("Send SuiteCompleted or Aborted ...")
    }
    new CompositeStatus(Set.empty ++ statusBuffer)
  }

  private[scalatest] def getSnippets(text: String): Vector[Snippet] = {
//println("text: " + text)
    val lines = Vector.empty ++ text.lines
//println("lines: " + lines)
    val pairs = lines map { line =>
      val trimmed = line.trim
      val suite =
        if (trimmed.startsWith("include[") && trimmed.endsWith("]")) {
//println("GOT HERE: " + trimmed + ", " + trimmed.substring(8).init)
          Some(trimmed.substring(8).init)
}
        else
          None
      (line, suite)
    }
//println("pairs: " + pairs)
    // val zipped = pairs.zipWithIndex
    // val insertionIndexes = for (((_, Some(_)), index) <- zipped) yield index
// Output of my fold left is: List[Snippet] (left is a list of snippets, right is a pair
    val backwards =
      (Vector[Snippet](Markup("")) /: pairs) { (left: Vector[Snippet], right: (String, Option[String])) =>
        right match {
          case (_, Some(key)) =>
            var (_, registeredSuites) = atomic.get.unpack
// TODO: Maybe give a better error message if this key doesn't exist?
            IncludedSuite(registeredSuites(key)) +: left
          case (line, None) =>
            left.head match {
              case Markup(text) => Markup(text + "\n" + line) +: left.tail
              case _ => Markup(line) +: left
            }
        }
      }
    backwards.reverse
  }

  private[scalatest] sealed trait Snippet
  private[scalatest] case class Markup(text: String) extends Snippet
  private[scalatest] case class IncludedSuite(suite: Suite) extends Snippet
}

private[scalatest] object Doc {

  private[scalatest] def trimMarkup(text: String): String = {
    val lines = text.lines.toList
    val zipLines = lines.zipWithIndex
    val firstNonWhiteLine = zipLines.find { case (line, _) => !line.trim.isEmpty }
    val lastNonWhiteLine = zipLines.reverse.find { case (line, _) => !line.trim.isEmpty } 
    (firstNonWhiteLine, lastNonWhiteLine) match {
      case (None, None) => text.trim // Will be either (None, None) or (Some, Some)
      case (Some((_, frontIdx)), Some((_, backIdx))) => lines.take(backIdx + 1).drop(frontIdx).mkString("\n")
    }
  }

  private[scalatest] def stripMargin(text: String): String = {
    val lines = text.lines.toList
    val firstNonWhiteLine = lines.find(!_.trim.isEmpty)
    firstNonWhiteLine match {
      case None => text.trim
      case Some(nonWhiteLine) =>
        val initialWhite = nonWhiteLine.dropWhile(_.isWhitespace)
        val margin =  nonWhiteLine.length - initialWhite.length
        val choppedLines = lines map { line =>
          val strip = if (line.length > margin) margin else line.length
          line.substring(strip)
        }
        choppedLines.mkString("\n")
    }
  }
}

