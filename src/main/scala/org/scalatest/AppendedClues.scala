/*
 * Copyright 2001-2011 Artima, Inc.
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

import exceptions.ModifiableMessage

/**
 * Trait providing an implicit conversion that allows clues to be placed after a block of code.
 *
 * <p>
 * You can use the <code>withClue</code> construct provided by <code>Assertions</code>, which is
 * extended by every style trait in ScalaTest, to add extra information to reports of failed or canceled tests.
 * The <code>withClue</code> from <code>Assertions</code> places the "clue string" at the front, both
 * in the code and in the resulting message:
 *
 * <pre class="stHighlight">
 * withClue("This is a prepended clue;") {
 *   1 + 1 should equal (3)
 * }
 * </pre>
 *
 * <p>
 * The above expression will yield the failure message:
 * </p>
 *
 * <p>
 * <code>This is a prepended clue; 2 did not equal 3</code>
 * </p>
 *
 * <p>
 * If you mix in this trait, or import its members via its companion object, you can alternatively place
 * the clue string at the end, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * { 1 + 1 should equal (3) } withClue "now the clue comes after"
 * </pre>
 *
 * <p>
 * The above expression will yield the failure message:
 * </p>
 *
 * <p>
 * <code>2 did not equal 3 now the clue comes after</code>
 * </p>
 *
 * <p>
 * If no space is already present, either at the beginning of the clue string or at the end
 * of the current message, a space will be placed between the two, unless the clue string
 * starts with one of the punctuation characters: comma (<code>,</code>), period (<code>.</code>),
 * or semicolon (<code>;</code>). For example, the failure message in the above example
 * includes an extra space inserted between <em>3</em> and <em>now</em>.
 * </p>
 *
 * <p>
 * By contrast this code, which has a clue string starting with comma:
 * </p>
 *
 * <pre class="stHighlight">
 * { 1 + 1 should equal (3) } withClue ", now the clue comes after"
 * </pre>
 *
 * <p>
 * Will yield a failure message with no extra inserted space:
 * </p>
 *
 * <p>
 * <code>2 did not equal 3, now the clue comes after</code>
 * </p>
 *
 * <p>
 * The <code>withClue</code> method will only append the clue string to the detail
 * message of exception types that mix in the <code>ModifiableMessage</code> trait.
 * See the documentation for <a href="exceptions/ModifiableMessage.html"><code>ModifiableMessage</code></a> for more
 * information.
 * </p>
 *
 * <p>
 * Note: the reason this functionality is not provided by <code>Assertions</code> directly, like the
 * prepended <code>withClue</code> construct, is because appended clues require an implicit conversion.
 * ScalaTest only gives you one implicit conversion by default in any test class to minimize the
 * potential for conflicts with other implicit conversions you may be using. All other implicit conversions,
 * including the one provided by this trait, you must explicitly invite into your code through inheritance
 * or an import.
 * </p>
 *
 * @author Bill Venners
 */
trait AppendedClues {

  /**
   * Class that provides a <code>withClue</code> method that appends clue strings to any
   * <a href="ModifiableMessage.html"><code>ModifiableMessage</code></a> exception
   * thrown by the passed by-name parameter.
   *
   * @author Bill Venners
   */
  class Clueful[T](fun: => T) {

    /**
     * Executes the block of code passed as the constructor parameter to this <code>Clueful</code>, and, if it
     * completes abruptly with a <code>ModifiableMessage</code> exception,
     * appends the "clue" string passed to this method to the end of the detail message
     * of that thrown exception, then rethrows it. If clue does not begin in a white space
     * character or one of the punctuation characters: comma (<code>,</code>),
     * period (<code>.</code>), or semicolon (<code>;</code>), one space will be added
     * between it and the existing detail message (unless the detail message is
     * not defined).
     *
     * <p>
     * This method allows you to add more information about what went wrong that will be
     * reported when a test fails or cancels. For example, this code:
     * </p>
     *
     * <pre class="stHighlight">
     * { 1 + 1 should equal (3) } withClue ", not even for very large values of 1"
     * </pre>
     *
     * <p>
     * Would yield a <code>TestFailed</code> exception whose message would be:
     * </p>
     *
     * <pre>
     * 2 did not equal 3, not even for very large values of 1
     * </pre>
     *
     * @throws NullPointerException if the passed <code>clue</code> is <code>null</code>
     */
    def withClue(clue: Any): T = {
      if (clue == null)
        throw new NullPointerException("clue was null")
      def append(currentMessage: Option[String]) =
        currentMessage match {
          case Some(msg) =>
            // clue.toString.head is guaranteed to work, because append() only called if clue.toString != ""
            val firstChar = clue.toString.head
            if (firstChar.isWhitespace || firstChar == '.' || firstChar == ',' || firstChar == ';')
              Some(msg + clue.toString)
            else
              Some(msg + " " + clue.toString)
          case None => Some(clue.toString)
        }
      try {
        fun
      }
      catch {
        case e: ModifiableMessage[_] =>
          if (clue.toString != "")
            throw e.modifyMessage(append)
          else
            throw e
      }
    }
  }

  /**
   * Implicit conversion that allows clues to be place after a block of code.
   */
  implicit def convertToClueful[T](fun: => T) = new Clueful(fun)
}

/**
 * Companion object that facilitates the importing of <code>AppendedClues</code> members as 
 * an alternative to mixing it in. One use case is to import <code>AppendedClues</code>
 * members so you can use them in the Scala interpreter.
 */
object AppendedClues extends AppendedClues
