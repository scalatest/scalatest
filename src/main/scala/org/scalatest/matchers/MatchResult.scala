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
package org.scalatest.matchers
 
/**
 * The result of a match operation, such as one performed by a <a href="Matcher.html"><code>Matcher</code></a> or
 * <a href="BeMatcher.html"><code>BeMatcher</code></a>, which 
 * contains one field that indicates whether the match succeeded and four fields that provide
 * failure messages to report under different circumstances.
 * 
 * <p>
 * A <code>MatchResult</code>'s <code>matches</code> field indicates whether a match succeeded. If it succeeded,
 * <code>matches</code> will be <code>true</code>.
 * The other four fields contain failure message strings, one of which will be presented to the user in case of a match failure. If a match succeeds,
 * none of these strings will be used, because no failure message will be reported (<em>i.e.</em>, because there was no failure
 * to report). If a match fails (<code>matches</code> is <code>false</code>), the <code>failureMessage</code> (or
 * <code>midSentenceFailure</code>&#8212;more on that below) will be reported to help the user understand what went wrong.
 * </p>
 *
 * <h2>Understanding <code>negatedFailureMessage</code></h2>
 *
 * <p>
 * The <code>negatedFailureMessage</code> exists so that it can become the <code>failureMessage</code> if the matcher is <em>inverted</em>,
 * which happens, for instance, if it is passed to <code>not</code>. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * val equalSeven = equal (7)
 * val notEqualSeven = not (equalSeven)
 * </pre>
 *
 * <p>
 * The <code>Matcher[Int]</code> that results from passing 7 to <code>equal</code>, which is assigned to the <code>equalSeven</code>
 * variable, will compare <code>Int</code>s passed to its
 * <code>apply</code> method with 7. If 7 is passed, the <code>equalSeven</code> match will succeed. If anything other than 7 is passed, it
 * will fail. By contrast, the <code>notEqualSeven</code> matcher, which results from passing <code>equalSeven</code> to <code>not</code>, does
 * just the opposite. If 7 is passed, the <code>notEqualSeven</code> match will fail. If anything other than 7 is passed, it will succeed.
 * </p>
 *
 * <p>
 * For example, if 8 is passed, <code>equalSeven</code>'s <code>MatchResult</code> will contain:
 * </p>
 *
 * <pre class="stExamples">
 *            expression: equalSeven(8)
 *               matches: false
 *        failureMessage: 8 did not equal 7
 * negatedFailureMessage: 8 equaled 7
 * </pre>
 *
 * <p>
 * Although the <code>negatedFailureMessage</code> is nonsensical, it will not be reported to the user. Only the <code>failureMessage</code>,
 * which does actually explain what caused the failure, will be reported by the user. If you pass 8 to <code>notEqualSeven</code>'s <code>apply</code>
 * method, by contrast, the <code>failureMessage</code> and <code>negatedFailureMessage</code> will be:
 * </p>
 *
 * <pre class="stExamples">
 *            expression: notEqualSeven(8)
 *               matches: true
 *        failureMessage: 8 equaled 7
 * negatedFailureMessage: 8 did not equal 7
 * </pre>
 *
 * <p>
 * Note that the messages are swapped from the <code>equalSeven</code> messages. This swapping was effectively performed by the <code>not</code> matcher,
 * which in addition to swapping the <code>failureMessage</code> and <code>negatedFailureMessage</code>, also inverted the
 * <code>matches</code> value. Thus when you pass the same value to both <code>equalSeven</code> and <code>notEqualSeven</code> the <code>matches</code>
 * field of one <code>MatchResult</code> will be <code>true</code> and the other <code>false</code>. Because the
 * <code>matches</code> field of the <code>MatchResult</code> returned by <code>notEqualSeven(8)</code> is <code>true</code>,
 * the nonsensical <code>failureMessage</code>, "<code>8 equaled 7</code>", will <em>not</em> be reported to the user.
 * </p>
 *
 * <p>
 * If 7 is passed, by contrast, the <code>failureMessage</code> and <code>negatedFailureMessage</code> of <code>equalSeven</code>
 * will be:
 * </p>
 *
 * <pre class="stExamples">
 *            expression: equalSeven(7)
 *               matches: true
 *        failureMessage: 7 did not equal 7
 * negatedFailureMessage: 7 equaled 7
 * </pre>
 *
 * <p>
 * In this case <code>equalSeven</code>'s <code>failureMessage</code> is nonsensical, but because the match succeeded, the nonsensical message will
 * not be reported to the user.
 * If you pass 7 to <code>notEqualSeven</code>'s <code>apply</code>
 * method, you'll get:
 * </p>
 *
 * <pre class="stExamples">
 *            expression: notEqualSeven(7)
 *               matches: false
 *        failureMessage: 7 equaled 7
 * negatedFailureMessage: 7 did not equal 7
 * </pre>
 *
 * <p>
 * Again the messages are swapped from the <code>equalSeven</code> messages, but this time, the <code>failureMessage</code> makes sense
 * and explains what went wrong: the <code>notEqualSeven</code> match failed because the number passed did in fact equal 7. Since
 * the match failed, this failure message, "<code>7 equaled 7</code>", will be reported to the user.
 * </p>
 *
 * <h2>Understanding the "<code>midSentence</code>" messages</h2>
 *
 * <p>
 * When a ScalaTest matcher expression that involves <code>and</code> or <code>or</code> fails, the failure message that
 * results is composed from the failure messages of the left and right matcher operatnds to <code>and</code> or </code>or</code>.
 * For example:
 * </p>
 *
 * <pre class="stExamples">
 * 8 should (equal (7) or equal (9))
 * </pre>
 *
 * <p>
 * This above expression would fail with the following failure message reported to the user:
 * </p>
 *
 * <pre class="stExamples">
 * 8 did not equal 7, and 8 did not equal 9
 * </pre>
 *
 * <p>
 * This works fine, but what if the failure messages being combined begin with a capital letter, such as:
 * </p>
 *
 * <pre class="stExamples">
 * The name property did not equal "Ricky"
 * </pre>
 *
 * <p>
 * A combination of two such failure messages might result in an abomination of English punctuation, such as:
 * </p>
 *
 * <pre class="stExamples">
 * The name property did not equal "Ricky", and The name property did not equal "Bobby"
 * </pre>
 *
 * <p>
 * Because ScalaTest is an internationalized application, taking all of its strings from a property file
 * enabling it to be localized, it isn't a good idea to force the first character to lower case. Besides,
 * it might actually represent a String value which should stay upper case. The <code>midSentenceFailureMessage</code>
 * exists for this situation. If the failure message is used at the beginning of the sentence, <code>failureMessage</code>
 * will be used. But if it appears mid-sentence, or at the end of the sentence, <code>midSentenceFailureMessage</code>
 * will be used. Given these failure message strings:
 * </p>
 *
 * <pre class="stExamples">
 *            failureMessage: The name property did not equal "Bobby"
 * midSentenceFailureMessage: the name property did not equal "Bobby"
 * </pre>
 *
 * <p>
 * The resulting failure of the <code>or</code> expression involving to matchers would make any English teacher proud:
 * </p>
 *
 * <pre class="stExamples">
 * The name property did not equal "Ricky", and the name property did not equal "Bobby"
 * </pre>
 *
 * @param matches indicates whether or not the matcher matched
 * @param failureMessage a failure message to report if a match fails
 * @param negatedFailureMessage a message with a meaning opposite to that of the failure message
 * @param midSentenceFailureMessage a failure message suitable for appearing mid-sentence
 * @param midSentenceNegatedFailureMessage a negated failure message suitable for appearing mid-sentence
 *
 * @author Bill Venners
 */
final case class MatchResult(
  matches: Boolean,
  failureMessage: String,
  negatedFailureMessage: String,
  midSentenceFailureMessage: String,
  midSentenceNegatedFailureMessage: String
) {

  /**
   * Constructs a new <code>MatchResult</code> with passed <code>matches</code>, <code>failureMessage</code>, and
   * <code>negativeFailureMessage</code> fields. The <code>midSentenceFailureMessage</code> will return the same
   * string as <code>failureMessage</code>, and the <code>midSentenceNegatedFailureMessage</code> will return the
   * same string as <code>negatedFailureMessage</code>.
   *
   * @param matches indicates whether or not the matcher matched
   * @param failureMessage a failure message to report if a match fails
   * @param negatedFailureMessage a message with a meaning opposite to that of the failure message
   */
  def this(matches: Boolean, failureMessage: String, negatedFailureMessage: String) =
    this(
      matches,
      failureMessage,
      negatedFailureMessage,
      failureMessage,
      negatedFailureMessage
    )
}

/**
 * Companion object for the <code>MatchResult</code> case class.
 *
 * @author Bill Venners
 */
object MatchResult {

/* Can't seem to redefine this to get the Scaladoc.
  /**
   * Factory method that constructs a new <code>MatchResult</code> with passed <code>matches</code>, <code>failureMessage</code>, 
   * <code>negativeFailureMessage</code>, <code>midSentenceFailureMessage</code>, and
   * <code>midSentenceNegatedFailureMessage</code> fields.
   *
   * @param matches indicates whether or not the matcher matched
   * @param failureMessage a failure message to report if a match fails
   * @param negatedFailureMessage a message with a meaning opposite to that of the failure message
   * @param midSentenceFailureMessage a failure message to report if a match fails
   * @param midSentenceNegatedFailureMessage a message with a meaning opposite to that of the failure message
   */
  def apply(matches: Boolean, failureMessage: String, negatedFailureMessage: String, midSentenceFailureMessage: String,
      midSentenceNegatedFailureMessage: String): MatchResult =
    new MatchResult(matches, failureMessage, negatedFailureMessage, midSentenceFailureMessage, midSentenceNegatedFailureMessage)
*/

  /**
   * Factory method that constructs a new <code>MatchResult</code> with passed <code>matches</code>, <code>failureMessage</code>, and
   * <code>negativeFailureMessage</code> fields. The <code>midSentenceFailureMessage</code> will return the same
   * string as <code>failureMessage</code>, and the <code>midSentenceNegatedFailureMessage</code> will return the
   * same string as <code>negatedFailureMessage</code>.
   *
   * @param matches indicates whether or not the matcher matched
   * @param failureMessage a failure message to report if a match fails
   * @param negatedFailureMessage a message with a meaning opposite to that of the failure message
   */
  def apply(matches: Boolean, failureMessage: String, negatedFailureMessage: String): MatchResult =
    new MatchResult(matches, failureMessage, negatedFailureMessage, failureMessage, negatedFailureMessage)
}

