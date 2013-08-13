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
import DocSpec.stripMargin
import DocSpec.trimMarkup
import java.util.concurrent.atomic.AtomicReference
import java.util.ConcurrentModificationException
import collection.mutable.ListBuffer
import Suite.reportMarkupProvided
import scala.collection.mutable.ListBuffer

private[scalatest] trait DocSpecLike extends Suite with Informing with Updating with Alerting { thisSuite =>

  private final val engine = new Engine("concurrentFunSuiteMod", "FunSuite")
  import engine._
  
  /**
   * Returns an <code>Informer</code> that during test execution will forward strings (and other objects) passed to its
   * <code>apply</code> method to the current reporter. If invoked in a constructor, it
   * will register the passed string for forwarding later during test execution. If invoked while this
   * <code>FeatureSpec</code> is being executed, such as from inside a test function, it will forward the information to
   * the current reporter immediately. If invoked at any other time, it will
   * throw an exception. This method can be called safely by any thread.
   */
  protected def info: Informer = atomicInformer.get

  protected def update: Updater = atomicUpdater.get
  protected def alert: Alerter = atomicAlerter.get

  sealed abstract class Snippet
  case class MarkupSnippet(text: String) extends Snippet
  case class SuiteSnippet(suite: Suite) extends Snippet

  implicit class MarkupContext(val sc: StringContext) {
    def markup(suites: Suite*): IndexedSeq[Snippet] = {
      val stringIt = sc.parts.iterator
      val suiteIt = suites.iterator
      // There will always be at least one element in stringIt, though
      // it could be empty.
      val buf = new ListBuffer[Snippet]
      val initialString = stringIt.next // don't trim, because \n's are important in markup
      if (!initialString.isEmpty) // But I'm guessing if a suite is the very first thing, we'll get an empty initial string
        buf += MarkupSnippet(initialString)
      // If there's another string, that means there's another
      // suite before it. The last string may again be empty.
      while (stringIt.hasNext) {
        buf += SuiteSnippet(suiteIt.next)
        buf += MarkupSnippet(stringIt.next)
      }
      // buf.toVector
      Vector.empty ++ buf // While supporting 2.9
    }
  }

  protected override def runNestedSuites(args: Args): Status = {
    import args._
    for (snippet <- doc) {
      snippet match {
        case SuiteSnippet(suite) =>
          // Need to of course compose these, but also need the darned 
          // status to make a checkmark. And want the stuff to come out
          // in a nice order in the text output. Hmm. I think this is the
          // sorting thing? Yes, this one started first, so it gets sorted
          // and preferred until it is done. The checkmark could go out as
          // a markup, but trouble is that I kind of want a real checkmark.
          // Like mocha did in the output, and in the HTML.
          suite.run(None, args)
        case MarkupSnippet(text) =>
          reportMarkupProvided(thisSuite, reporter, tracker, None, trimMarkup(stripMargin(text)), 0, None, true)
      }
    }
    SucceededStatus
  }

  val doc: IndexedSeq[Snippet]
}

