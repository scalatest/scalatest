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
package org.scalactic

/*
 Describe need to deal with flickers. Fix them so they pass 100% if you can, but if not, the problem
 is wastes people's time looking at them. Or if they happen too often it is the cry wolf problem, and people
 don't bother looking anymore when the CI server fails. They get jaded and red doesn't mean the same.
 Best thing to do is fix it, but sometimes that's not practical. In such situations, cancel when the 
 problem occurs.

 If just needed one place, just write it in place

try ...
catch {
  case e: DBAccessException if e.getMessage == "500:Internal Server Error" => cancel(e)
}

 But if you're going to use it in multiple places, make an extractor. This is what it would look like written out in full:

object InternalServerError {
  def unapply(e: Throwable): Option[Throwable] = {
    case e: DBAccessException if e.getMessage == "500:Internal Server Error" => Some(e)
    case _ => None
  }
}

  Extractor gives you a factory method for such one-item extractors:

val InternalServerError =
  Extractor[Throwable] { e: DBAccessException =>
    e.getMessage == "500:Internal Server Error"
  }

// Catcher is a convenience subclass for Extractor[Throwable]:

val InternalServerError =
  Catcher { e: DBAccessException =>
    e.getMessage == "500:Internal Server Error"
  }
*/
/**
 * Convenience class for extractors that match and return <code>Throwable</code>s based on a type and <code>Boolean</code> condition.
 *
 * <p>
 * Class <code>Catcher</code> was motivated by the need to catch
 * and handle exceptions based on more than just the exception's type as a strategy for dealing with
 * "flickering" tests&#8212;tests that usually pass, but occasionally fail. The best strategy for dealing with
 * flickers is to fix the test such that they stop flickering, but sometimes that is not practical. In
 * such cases allowing the test to continue flickering can distract the team by requiring them to
 * spend time inspecting failures to determine whether or not they are flickers or real failures that need
 * attention. Worse, with enough flickers, team members can stop checking all failures and not notice real ones.
 * </p>
 *
 * <p>
 * One strategy for dealing with flickers you can't practically fix is to catch exceptions that are causing individual flickers
 * and cancel the test when you detect them. Often this means you will need to insert a catch clause in a particular spot, or a pattern
 * match if in a <code>withFixture</code>, looking for a particular exception with a particular message or other identifying attribute. If
 * the same problem is causing flickers in many places,
 * it is handy to create an extractor to detect the problem. This <code>Catcher</code> class provides
 * a factory method that takes a partial function from <code>Throwable</code> to <code>Boolean</code> and produces such an extractor.
 * Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * val InternalServerError =
 *   Catcher { case e: DBAccessException =&gt;
 *     e.getMessage == "500:Internal Server Error"
 *   }
 * </pre>
 * 
 * <p>
 * Using this <code>Catcher</code> in a ScalaTest <code>withFixture</code> method would look like:
 * </p>
 *
 * <pre class="stHighlight">
 * override def withFixture(test: NoArgTest) = {
 *   super.withFixture(test) match {
 *      case Failed(InternalServerError(ex)) =&gt;
 *        Canceled("Canceled because likely a flicker caused by intermittently flaky DB", ex)
 *      case other =&gt; other
 *   }
 * }
 * </pre>
 *
 * @param partial the partial function that is used by this extractor to determine matches
 */
class Catcher(partial: PartialFunction[Throwable, Boolean]) {
  if (partial == null) throw new NullPointerException("partial was null")

  /**
   * Extractor for <code>Throwable</code> that determines matches using on the partial function
   * passed to the constructor.
   *
   * @param exception the exception on which to match
   */
  def unapply(exception: Throwable): Option[Throwable] = {
    if (partial.isDefinedAt(exception) && partial(exception)) Some(exception) else None
  }
}

/**
 * Companion object for <code>Catcher</code> that provides a factory method for creating <code>Throwable</code> extractors.
 */
object Catcher {

  /**
   * Creates and returns a new <code>Catcher</code> that uses the passed partial function to determine matches.
   *
   * @param partial the partial function that is used by returned extractor to determine matches
   */
  def apply(partial: PartialFunction[Throwable, Boolean]): Catcher = new Catcher(partial)
}

