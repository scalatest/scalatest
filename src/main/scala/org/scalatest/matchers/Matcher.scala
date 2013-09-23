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
package org.scalatest.matchers

import org.scalatest.enablers._
import org.scalatest.MatchersHelper.orMatchersAndApply
import org.scalatest.MatchersHelper.andMatchersAndApply
import org.scalatest.words.MatcherWords
import scala.collection.GenTraversable
import scala.util.matching.Regex
import org.scalautils.Equality
import org.scalautils.TripleEqualsSupport.Spread
import org.scalautils.TripleEqualsSupport.TripleEqualsInvocation
import org.scalautils.Prettifier
import org.scalatest.FailureMessages
import org.scalatest.Resources
import org.scalatest.words.FullyMatchWord
import org.scalatest.words.StartWithWord
import org.scalatest.words.EndWithWord
import org.scalatest.words.IncludeWord
import org.scalatest.words.HaveWord
import org.scalatest.words.BeWord
import org.scalatest.words.NotWord
import org.scalatest.words.ContainWord
import org.scalatest.words.ResultOfLengthWordApplication
import org.scalatest.words.ResultOfSizeWordApplication
import org.scalatest.words.ResultOfMessageWordApplication
import org.scalatest.words.ResultOfLessThanComparison
import org.scalatest.words.ResultOfGreaterThanComparison
import org.scalatest.words.ResultOfLessThanOrEqualToComparison
import org.scalatest.words.ResultOfGreaterThanOrEqualToComparison
import org.scalatest.words.ResultOfAWordToSymbolApplication
import org.scalatest.words.ResultOfAWordToBePropertyMatcherApplication
import org.scalatest.words.ResultOfAWordToAMatcherApplication
import org.scalatest.words.ResultOfAnWordToSymbolApplication
import org.scalatest.words.ResultOfAnWordToBePropertyMatcherApplication
import org.scalatest.words.ResultOfAnWordToAnMatcherApplication
import org.scalatest.words.ResultOfTheSameInstanceAsApplication
import org.scalatest.words.ResultOfRegexWordApplication
import org.scalatest.words.RegexWithGroups
import org.scalatest.words.ResultOfDefinedAt
import org.scalatest.words.ResultOfOneOfApplication
import org.scalatest.words.ResultOfAtLeastOneOfApplication
import org.scalatest.words.ResultOfNoneOfApplication
import org.scalatest.words.ResultOfTheSameElementsAsApplication
import org.scalatest.words.ResultOfTheSameElementsInOrderAsApplication
import org.scalatest.words.ResultOfOnlyApplication
import org.scalatest.words.ResultOfAllOfApplication
import org.scalatest.words.ResultOfInOrderOnlyApplication
import org.scalatest.words.ResultOfInOrderApplication
import org.scalatest.words.ResultOfKeyWordApplication
import org.scalatest.words.ResultOfValueWordApplication
import org.scalatest.words.ResultOfAtMostOneOfApplication
import org.scalatest.words.SortedWord
import org.scalatest.words.ResultOfATypeInvocation
import org.scalatest.words.ResultOfAnTypeInvocation
import org.scalatest.words.ExistWord
import org.scalatest.words.ResultOfNotExist
import org.scalatest.words.ReadableWord
import org.scalatest.words.WritableWord
import org.scalatest.words.EmptyWord
import org.scalatest.words.DefinedWord

/**
 * Trait extended by objects that can match a value of the specified type. The value to match is
 * passed to the matcher's <code>apply</code> method. The result is a <code>MatchResult</code>.
 * A matcher is, therefore, a function from the specified type, <code>T</code>, to a <code>MatchResult</code>.
 * <p></p> <!-- needed otherwise the heading below shows up in the wrong place. dumb scaladoc algo -->
 *
 * <h2>Creating custom matchers</h2>
 * 
 * <p>
 * <em>Note: We are planning on adding some new matchers to ScalaTest in a future release, and would like your feedback.
 * Please let us know if you have felt the need for a matcher ScalaTest doesn't yet provide, whether or
 * not you wrote a custom matcher for it. Please email your feedback to bill AT artima.com.</em>
 * </p>
 *
 * <p>
 * If none of the built-in matcher syntax satisfies a particular need you have, you can create
 * custom <code>Matcher</code>s that allow
 * you to place your own syntax directly after <code>should</code> or <code>must</code>. For example, class <code>java.io.File</code> has a method <code>exists</code>, which
 * indicates whether a file of a certain path and name exists. Because the <code>exists</code> method takes no parameters and returns <code>Boolean</code>,
 * you can call it using <code>be</code> with a symbol or <code>BePropertyMatcher</code>, yielding assertions like:
 * </p>
 * 
 * <pre class="stHighlight">
 * file should be ('exists)  // using a symbol
 * file should be (inExistance)   // using a BePropertyMatcher
 * </pre>
 * 
 * <p>
 * Although these expressions will achieve your goal of throwing a <code>TestFailedException</code> if the file does not exist, they don't produce
 * the most readable code because the English is either incorrect or awkward. In this case, you might want to create a
 * custom <code>Matcher[java.io.File]</code>
 * named <code>exist</code>, which you could then use to write expressions like:
 * </p>
 *
 * <pre class="stHighlight">
 * // using a plain-old Matcher
 * file should exist
 * file should not (exist)
 * file should (exist and have ('name ("temp.txt")))
 * </pre>
 * 
 * <p>
 * One good way to organize custom matchers is to place them inside one or more
 * traits that you can then mix into the suites or specs that need them. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * trait CustomMatchers {
 * 
 *   class FileExistsMatcher extends Matcher[java.io.File] {
 * 
 *     def apply(left: java.io.File) = {
 * 
 *       val fileOrDir = if (left.isFile) "file" else "directory"
 * 
 *       val failureMessageSuffix = 
 *         fileOrDir + " named " + left.getName + " did not exist"
 * 
 *       val negatedFailureMessageSuffix = 
 *         fileOrDir + " named " + left.getName + " existed"
 * 
 *       MatchResult(
 *         left.exists,
 *         "The " + failureMessageSuffix,
 *         "The " + negatedFailureMessageSuffix,
 *         "the " + failureMessageSuffix,
 *         "the " + negatedFailureMessageSuffix
 *       )
 *     }
 *   }
 * 
 *   val exist = new FileExistsMatcher
 * }
 *
 * // Make them easy to import with:
 * // import CustomMatchers._
 * object CustomMatchers extends CustomMatchers
 * </pre>
 * 
 * <p>
 * Note: the <code>CustomMatchers</code> companion object exists to make it easy to bring the
 * matchers defined in this trait into scope via importing, instead of mixing in the trait. The ability
 * to import them is useful, for example, when you want to use the matchers defined in a trait in the Scala interpreter console.
 * </p>
 *
 * <p>
 * This trait contains one matcher class, <code>FileExistsMatcher</code>, and a <code>val</code> named <code>exist</code> that refers to
 * an instance of <code>FileExistsMatcher</code>. Because the class extends <code>Matcher[java.io.File]</code>,
 * the compiler will only allow it be used to match against instances of <code>java.io.File</code>. A matcher must declare an
 * <code>apply</code> method that takes the type decared in <code>Matcher</code>'s type parameter, in this case <code>java.io.File</code>.
 * The apply method will return a <code>MatchResult</code> whose <code>matches</code> field will indicate whether the match succeeded.
 * The <code>failureMessage</code> field will provide a programmer-friendly error message indicating, in the event of a match failure, what caused
 * the match to fail. 
 * </p>
 *
 * <p>
 * The <code>FileExistsMatcher</code> matcher in this example determines success by calling <code>exists</code> on the passed <code>java.io.File</code>. It
 * does this in the first argument passed to the <code>MatchResult</code> factory method:
 * </p>
 *
 * <pre class="stHighlight">
 *         left.exists,
 * </pre>
 *
 * <p>
 * In other words, if the file exists, this matcher matches.
 * The next argument to <code>MatchResult</code>'s factory method produces the failure message string:
 * </p>
 *
 * <pre class="stHighlight">
 *         "The " + failureMessageSuffix,
 * </pre>
 *
 * <p>
 * If the passed <code>java.io.File</code> is a file (not a directory) and has the name <code>temp.txt</code>, for example, the failure
 * message would be:
 * </p>
 *
 * <pre>
 * The file named temp.txt did not exist
 * </pre>
 *
 * <p>
 * For more information on the fields in a <code>MatchResult</code>, including the subsequent three fields that follow the failure message,
 * please see the documentation for <a href="MatchResult.html"><code>MatchResult</code></a>.
 * </p>
 *
 * <p>
 * Given the <code>CustomMatchers</code> trait as defined above, you can use the <code>exist</code> syntax in any suite or spec in
 * which you mix in the trait:
 * </p>
 *
 * <pre class="stHighlight">
 * class ExampleSpec extends Spec with ShouldMatchers with CustomMatchers {
 * 
 *   describe("A temp file") {
 * 
 *     it("should be created and deleted") {
 * 
 *       val tempFile = java.io.File.createTempFile("delete", "me")
 * 
 *       try {
 *         // At this point the temp file should exist
 *         tempFile should exist
 *       }
 *       finally {
 *         tempFile.delete()
 *       }
 * 
 *       // At this point it should not exist
 *       tempFile should not (exist)
 *     }
 *   }
 * }
 * </pre>
 *  
 * <p>
 * Note that when you use custom <code>Matcher</code>s, you will need to put parentheses around the custom matcher when if follows <code>not</code>,
 * as shown in the last assertion above: <code>tempFile should not (exist)</code>.
 * </p>
 *
 * <a name="otherways"></a><h2>Other ways to create matchers</h2>
 *
 * <p>
 * There are other ways to create new matchers besides defining one as shown above. For example, you would normally check to ensure
 * an option is defined like this:
 * </p>
 *
 * <pre class="stHighlight">
 * Some("hi") should be ('defined)
 * </pre>
 *
 * <p>
 * If you wanted to get rid of the tick mark, you could simply define <code>defined</code> like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val defined = 'defined
 * </pre>
 *
 * <p>
 * Now you can check that an option is defined without the tick mark:
 * </p>
 *
 * <pre class="stHighlight">
 * Some("hi") should be (defined)
 * </pre>
 *
 * <p>
 * Perhaps after using that for a while, you realize you're tired of typing the parentheses. You could
 * get rid of them with another one-liner:
 * </p>
 *
 * <pre class="stHighlight">
 * val beDefined = be (defined)
 * </pre>
 *
 * <p>
 * Now you can check that an option is defined without the tick mark or the parentheses:
 * </p>
 *
 * <pre class="stHighlight">
 * Some("hi") should beDefined
 * </pre>
 *
 * <p>
 * You can also use ScalaTest matchers' logical operators to combine existing matchers into new ones, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val beWithinTolerance = be >= 0 and be <= 10
 * </pre>
 *
 * <p>
 * Now you could check that a number is within the tolerance (in this case, between 0 and 10, inclusive), like this:
 * </p>
 *
 * <pre class="stHighlight">
 * num should beWithinTolerance
 * </pre>
 *
 * <p>
 * When defining a full blown matcher, one shorthand is to use one of the factory methods in <code>Matcher</code>'s companion
 * object. For example, instead of writing this:
 * </p>
 *
 * <pre class="stHighlight">
 * val beOdd =
 *   new Matcher[Int] {
 *     def apply(left: Int) =
 *       MatchResult(
 *         left % 2 == 1,
 *         left + " was not odd",
 *         left + " was odd"
 *       )
 *   }
 * </pre>
 *
 * <p>
 * You could alternately write this:
 * </p>
 *
 * <pre class="stHighlight">
 * val beOdd =
 *   Matcher { (left: Int) =>
 *     MatchResult(
 *       left % 2 == 1,
 *       left + " was not odd",
 *       left + " was odd"
 *     )
 *   }
 * </pre>
 *
 * <p>
 * Either way you define the <code>beOdd</code> matcher, you could use it like this:
 * </p>
 *
 * <pre class="stHighlight">
 * 3 should beOdd
 * 4 should not (beOdd)
 * </pre>
 *
 * <p>
 * You can also compose matchers. If for some odd reason, you wanted a <code>Matcher[String]</code> that 
 * checked whether a string, when converted to an <code>Int</code>,
 * was odd, you could make one by composing <code>beOdd</code> with
 * a function that converts a string to an <code>Int</code>, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * val beOddAsInt = beOdd compose { (s: String) => s.toInt }
 * </pre>
 *
 * <p>
 * Now you have a <code>Matcher[String]</code> whose <code>apply</code> method first
 * invokes the converter function to convert the passed string to an <code>Int</code>,
 * then passes the resulting <code>Int</code> to <code>beOdd</code>. Thus, you could use
 * <code>beOddAsInt</code> like this:
 * </p>
 *
 * <pre class="stHighlight">
 * "3" should beOddAsInt
 * "4" should not (beOddAsInt)
 * </pre>
 *
 * <p>
 * You can also define a method that produces a matcher using matcher
 * composition and a passed parameter. For example, here's how you could create a <code>Matcher[File]</code> from a
 * <code>Matcher[String]</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * import java.io.File
 *
 * def endWithExtension(ext: String) = endWith(ext) compose { (f: File) =&gt; f.getPath }
 * </pre>
 *
 * <p>
 * Every time you call the above <code>endWithExtension</code> method, you'll get a new <code>Matcher[File]</code>. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * new File("output.txt") should endWithExtension("txt")
 * </pre>
 * 
 * <h2>Matcher's variance</h2>
 *
 * <p>
 * <code>Matcher</code> is contravariant in its type parameter, <code>T</code>, to make its use more flexible.
 * As an example, consider the hierarchy:
 * </p>
 *
 * <pre class="stHighlight">
 * class Fruit
 * class Orange extends Fruit
 * class ValenciaOrange extends Orange
 * </pre>
 *
 * <p>
 * Given an orange:
 * </p>
 *
 * <pre class="stHighlight">
 * val orange = Orange
 * </pre>
 *
 * <p>
 * The expression "<code>orange should</code>" will, via an implicit conversion in <code>ShouldMatchers</code>,
 * result in an object that has a <code>should</code>
 * method that takes a <code>Matcher[Orange]</code>. If the static type of the matcher being passed to <code>should</code> is
 * <code>Matcher[Valencia]</code> it shouldn't (and won't) compile. The reason it shouldn't compile is that
 * the left value is an <code>Orange</code>, but not necessarily a <code>Valencia</code>, and a
 * <code>Matcher[Valencia]</code> only knows how to match against a <code>Valencia</code>. The reason
 * it won't compile is given that <code>Matcher</code> is contravariant in its type parameter, <code>T</code>, a
 * <code>Matcher[Valencia]</code> is <em>not</em> a subtype of <code>Matcher[Orange]</code>.
 * </p>
 *
 * <p>
 * By contrast, if the static type of the matcher being passed to <code>should</code> is <code>Matcher[Fruit]</code>,
 * it should (and will) compile. The reason it <em>should</em> compile is that given the left value is an <code>Orange</code>,
 * it is also a <code>Fruit</code>, and a <code>Matcher[Fruit]</code> knows how to match against <code>Fruit</code>s.
 * The reason it <em>will</em> compile is that given  that <code>Matcher</code> is contravariant in its type parameter, <code>T</code>, a
 * <code>Matcher[Fruit]</code> is indeed a subtype of <code>Matcher[Orange]</code>.
 * </p>
 *
 * @author Bill Venners
 */
trait Matcher[-T] extends Function1[T, MatchResult] { outerInstance =>

  /**
   * Check to see if the specified object, <code>left</code>, matches, and report the result in
   * the returned <code>MatchResult</code>. The parameter is named <code>left</code>, because it is
   * usually the value to the left of a <code>should</code> or <code>must</code> invocation. For example,
   * in:
   *
   * <pre class="stHighlight">
   * list should equal (List(1, 2, 3))
   * </pre>
   *
   * The <code>equal (List(1, 2, 3))</code> expression results in a matcher that holds a reference to the
   * right value, <code>List(1, 2, 3)</code>. The <code>should</code> method invokes <code>apply</code>
   * on this matcher, passing in <code>list</code>, which is therefore the "<code>left</code>" value. The
   * matcher will compare the <code>list</code> (the <code>left</code> value) with <code>List(1, 2, 3)</code> (the right
   * value), and report the result in the returned <code>MatchResult</code>.
   *
   * @param left the value against which to match
   * @return the <code>MatchResult</code> that represents the result of the match
   */
  def apply(left: T): MatchResult

  /**
   * Compose this matcher with the passed function, returning a new matcher.
   *
   * <p>
   * This method overrides <code>compose</code> on <code>Function1</code> to
   * return a more specific function type of <code>Matcher</code>. For example, given
   * a <code>beOdd</code> matcher defined like this:
   * </p>
   *
   * <pre class="stHighlight">
   * val beOdd =
   *   new Matcher[Int] {
   *     def apply(left: Int) =
   *       MatchResult(
   *         left % 2 == 1,
   *         left + " was not odd",
   *         left + " was odd"
   *       )
   *   }
   * </pre>
   *
   * <p>
   * You could use <code>beOdd</code> like this:
   * </p>
   *
   * <pre class="stHighlight">
   * 3 should beOdd
   * 4 should not (beOdd)
   * </pre>
   *
   * <p>
   * If for some odd reason, you wanted a <code>Matcher[String]</code> that 
   * checked whether a string, when converted to an <code>Int</code>,
   * was odd, you could make one by composing <code>beOdd</code> with
   * a function that converts a string to an <code>Int</code>, like this:
   * </p>
   *
   * <pre class="stHighlight">
   * val beOddAsInt = beOdd compose { (s: String) => s.toInt }
   * </pre>
   *
   * <p>
   * Now you have a <code>Matcher[String]</code> whose <code>apply</code> method first
   * invokes the converter function to convert the passed string to an <code>Int</code>,
   * then passes the resulting <code>Int</code> to <code>beOdd</code>. Thus, you could use
   * <code>beOddAsInt</code> like this:
   * </p>
   *
   * <pre class="stHighlight">
   * "3" should beOddAsInt
   * "4" should not (beOddAsInt)
   * </pre>
   */
  override def compose[U](g: U => T): Matcher[U] =
    new Matcher[U] {
      def apply(u: U) = outerInstance.apply(g(u))
    }

// TODO: mention not short circuited, and the precendence is even between and and or

  /**
   * Returns a matcher whose <code>apply</code> method returns a <code>MatchResult</code>
   * that represents the logical-and of the results of the wrapped and the passed matcher applied to
   * the same value.
   *
   * <p>
   * The reason <code>and</code> has an upper bound on its type parameter is so that the <code>Matcher</code>
   * resulting from an invocation of <code>and</code> will have the correct type parameter. If you call
   * <code>and</code> on a <code>Matcher[Orange]</code>, passing in a <code>Matcher[Valencia]</code>,
   * the result will have type <code>Matcher[Valencia]</code>. This is correct because both a
   * <code>Matcher[Orange]</code> and a <code>Matcher[Valencia]</code> know how to match a
   * <code>Valencia</code> (but a <code>Matcher[Valencia]</code> doesn't know how to
   * match any old <code>Orange</code>).  If you call
   * <code>and</code> on a <code>Matcher[Orange]</code>, passing in a <code>Matcher[Fruit]</code>,
   * the result will have type <code>Matcher[Orange]</code>. This is also correct because both a
   * <code>Matcher[Orange]</code> and a <code>Matcher[Fruit]</code> know how to match an
   * <code>Orange</code> (but a <code>Matcher[Orange]</code> doesn't know how to
   * match any old <code>Fruit</code>).
   * </p>
   *
   * @param the matcher to logical-and with this matcher
   * @return a matcher that performs the logical-and of this and the passed matcher
   */
  def and[U <: T](rightMatcher: Matcher[U]): Matcher[U] =
    new Matcher[U] {
      def apply(left: U): MatchResult = {
        andMatchersAndApply(left, outerInstance, rightMatcher)
      }
      override def toString: String = "(" + Prettifier.default(outerInstance) + ") and (" + Prettifier.default(rightMatcher) + ")"
    }

  /**
   * Returns a <code>MatcherFactory</code> whose <code>matcher</code> method returns a <code>Matcher</code>,
   * which has <code>apply</code> method that returns a <code>MatchResult</code> that represents the logical-and
   * of the results of the wrapped and the passed <code>MatcherFactory</code> applied to the same value.
   *
   * @param rightMatcherFactory1 the <code>MatcherFactory</code> to logical-and with this <code>MatcherFactory</code>
   * @return a <code>MatcherFactory</code> that performs the logical-and of this and the passed <code>MatcherFactory</code>
   */
  def and[U, TC1[_]](rightMatcherFactory1: MatcherFactory1[U, TC1]): MatcherFactory1[T with U, TC1] =
    new MatcherFactory1[T with U, TC1] {
      def matcher[V <: T with U : TC1]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val rightMatcher = rightMatcherFactory1.matcher
            andMatchersAndApply(left, outerInstance, rightMatcher)
          }
        }
      }
      override def toString: String = "(" + Prettifier.default(outerInstance) + ") and (" + Prettifier.default(rightMatcherFactory1) + ")"
    }

  /**
   * Returns a matcher whose <code>apply</code> method returns a <code>MatchResult</code>
   * that represents the logical-or of the results of this and the passed matcher applied to
   * the same value.
   *
   * <p>
   * The reason <code>or</code> has an upper bound on its type parameter is so that the <code>Matcher</code>
   * resulting from an invocation of <code>or</code> will have the correct type parameter. If you call
   * <code>or</code> on a <code>Matcher[Orange]</code>, passing in a <code>Matcher[Valencia]</code>,
   * the result will have type <code>Matcher[Valencia]</code>. This is correct because both a
   * <code>Matcher[Orange]</code> and a <code>Matcher[Valencia]</code> know how to match a
   * <code>Valencia</code> (but a <code>Matcher[Valencia]</code> doesn't know how to
   * match any old <code>Orange</code>).  If you call
   * <code>or</code> on a <code>Matcher[Orange]</code>, passing in a <code>Matcher[Fruit]</code>,
   * the result will have type <code>Matcher[Orange]</code>. This is also correct because both a
   * <code>Matcher[Orange]</code> and a <code>Matcher[Fruit]</code> know how to match an
   * <code>Orange</code> (but a <code>Matcher[Orange]</code> doesn't know how to
   * match any old <code>Fruit</code>).
   * </p>
   *
   * @param rightMatcher the matcher to logical-or with this matcher
   * @return a matcher that performs the logical-or of this and the passed matcher
   */
  def or[U <: T](rightMatcher: Matcher[U]): Matcher[U] =
    new Matcher[U] {
      def apply(left: U): MatchResult = {
        orMatchersAndApply(left, outerInstance, rightMatcher)
      }
      override def toString: String = "(" + Prettifier.default(outerInstance) + ") or (" + Prettifier.default(rightMatcher) + ")"
    }

  /**
   * Returns a <code>MatcherFactory</code> whose <code>matcher</code> method returns a <code>Matcher</code>,
   * which has <code>apply</code> method that returns a <code>MatchResult</code> that represents the logical-or
   * of the results of the wrapped and the passed <code>MatcherFactory</code> applied to the same value.
   *
   * @param rightMatcherFactory1 the <code>MatcherFactory</code> to logical-or with this <code>MatcherFactory</code>
   * @return a <code>MatcherFactory</code> that performs the logical-or of this and the passed <code>MatcherFactory</code>
   */
  def or[U, TC1[_]](rightMatcherFactory1: MatcherFactory1[U, TC1]): MatcherFactory1[T with U, TC1] =
    new MatcherFactory1[T with U, TC1] {
      def matcher[V <: T with U : TC1]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val rightMatcher = rightMatcherFactory1.matcher
            orMatchersAndApply(left, outerInstance, rightMatcher)
          }
          override def toString: String = "(" + Prettifier.default(outerInstance) + ") or (" + Prettifier.default(rightMatcherFactory1) + ")"
        }
      }
      override def toString: String = "(" + Prettifier.default(outerInstance) + ") or (" + Prettifier.default(rightMatcherFactory1) + ")"
    }

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndHaveWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and have length (3 - 1)
     *                   ^
     * </pre>
     */
    def length(expectedLength: Long): MatcherFactory1[T, Length] = and(MatcherWords.have.length(expectedLength))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and have size (3 - 1)
     *                                 ^
     * </pre>
     */
    def size(expectedSize: Long): MatcherFactory1[T, Size] = and(MatcherWords.have.size(expectedSize))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and have message ("A message from Mars")
     *                   ^
     * </pre>
     */
    def message(expectedMessage: String): MatcherFactory1[T, Messaging] = and(MatcherWords.have.message(expectedMessage))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher and have size (3 - 1)
   *          ^
   * </pre>
   */
  def and(haveWord: HaveWord): AndHaveWord = new AndHaveWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndContainWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and contain (3 - 1)
     *                      ^
     * </pre>
     */
    def apply[U](expectedElement: Any): MatcherFactory1[T with U, Containing] = outerInstance.and(MatcherWords.contain(expectedElement))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and contain key ("one")
     *                      ^
     * </pre>
     */
    def key(expectedKey: Any): MatcherFactory1[T, KeyMapping] = outerInstance.and(MatcherWords.contain.key(expectedKey))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and contain value (1)
     *                      ^
     * </pre>
     */
    def value(expectedValue: Any): MatcherFactory1[T, ValueMapping] = outerInstance.and(MatcherWords.contain.value(expectedValue))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and contain theSameElementsAs List(1, 2, 3)
     *                      ^
     * </pre>
     */
    def theSameElementsAs(right: GenTraversable[_]): MatcherFactory1[T, Aggregating] = 
      outerInstance.and(MatcherWords.contain.theSameElementsAs(right))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and contain theSameElementsInOrderAs List(1, 2, 3)
     *                      ^
     * </pre>
     */
    def theSameElementsInOrderAs(right: GenTraversable[_]): MatcherFactory1[T, Sequencing] = 
      outerInstance.and(MatcherWords.contain.theSameElementsInOrderAs(right))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and contain inOrderOnly (1, 2, 3)
     *                      ^
     * </pre>
     */
    def inOrderOnly(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory1[T, Sequencing] =
      outerInstance.and(MatcherWords.contain.inOrderOnly(firstEle, secondEle, remainingEles.toList: _*))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and contain allOf (1, 2, 3)
     *                      ^
     * </pre>
     */
    def allOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory1[T, Aggregating] =
      outerInstance.and(MatcherWords.contain.allOf(firstEle, secondEle, remainingEles.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and contain inOrder (1, 2, 3)
     *                      ^
     * </pre>
     */
    def inOrder(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory1[T, Sequencing] =
      outerInstance.and(MatcherWords.contain.inOrder(firstEle, secondEle, remainingEles.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and contain oneOf (1, 2, 3)
     *                      ^
     * </pre>
     */
    def oneOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory1[T, Containing] =
      outerInstance.and(MatcherWords.contain.oneOf(firstEle, secondEle, remainingEles.toList: _*))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and contain atLeastOneOf (1, 2, 3)
     *                      ^
     * </pre>
     */
    def atLeastOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory1[T, Aggregating] =
      outerInstance.and(MatcherWords.contain.atLeastOneOf(firstEle, secondEle, remainingEles.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and contain only (1, 2, 3)
     *                      ^
     * </pre>
     */
    def only(right: Any*): MatcherFactory1[T, Aggregating] = 
      outerInstance.and(MatcherWords.contain.only(right.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and contain noneOf (1, 2, 3)
     *                      ^
     * </pre>
     */
    def noneOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory1[T, Containing] =
      outerInstance.and(MatcherWords.contain.noneOf(firstEle, secondEle, remainingEles.toList: _*))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and contain atMostOneOf (1, 2, 3)
     *                      ^
     * </pre>
     */
    def atMostOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory1[T, Aggregating] =
      outerInstance.and(MatcherWords.contain.atMostOneOf(firstEle, secondEle, remainingEles.toList: _*))
  }
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher and contain key ("one")
   *          ^
   * </pre>
   */
  def and(containWord: ContainWord): AndContainWord = new AndContainWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndBeWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and be a ('file)
     *                 ^
     * </pre>
     */
    def a(symbol: Symbol): Matcher[T with AnyRef] = and(MatcherWords.be.a(symbol))

    /**
     * This method enables the following syntax, where <code>file</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher and be a (file)
     *                 ^
     * </pre>
     */
    def a[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = and(MatcherWords.be.a(bePropertyMatcher))

    /**
     * This method enables the following syntax, where <code>positiveNumber</code> and <code>validNumber</code> are <a href="AMatcher.html"><code>AMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher and be a (validNumber)
     *                 ^
     * </pre>
     */
    def a[U](aMatcher: AMatcher[U]): Matcher[T with U] = and(MatcherWords.be.a(aMatcher))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and be an ('apple)
     *                 ^
     * </pre>
     */
    def an(symbol: Symbol): Matcher[T with AnyRef] = and(MatcherWords.be.an(symbol))

    /**
     * This method enables the following syntax, where <code>apple</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher and be an (apple)
     *                 ^
     * </pre>
     */
    def an[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = and(MatcherWords.be.an(bePropertyMatcher))
    
    /**
     * This method enables the following syntax, where <code>integerNumber</code> is an <a href="AnMatcher.html"><code>AnMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher and be an (integerNumber)
     *                 ^
     * </pre>
     */
    def an[U](anMatcher: AnMatcher[U]): Matcher[T with U] = and(MatcherWords.be.an(anMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and be theSameInstanceAs (string)
     *                 ^
     * </pre>
     */
    def theSameInstanceAs(anyRef: AnyRef): Matcher[T with AnyRef] = and(MatcherWords.be.theSameInstanceAs(anyRef))
    
    /**
     * This method enables the following syntax, where <code>fraction</code> refers to a <code>PartialFunction</code>:
     *
     * <pre class="stHighlight">
     * aMatcher and be definedAt (8)
     *                 ^
     * </pre>
     */
    def definedAt[A, U <: PartialFunction[A, _]](right: A): Matcher[T with U] = and(MatcherWords.be.definedAt(right))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher and be a ('file)
   *          ^
   * </pre>
   */
  def and(beWord: BeWord): AndBeWord = new AndBeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndFullyMatchWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and fullyMatch regex (decimal)
     *                         ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = and(MatcherWords.fullyMatch.regex(regexString))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and fullyMatch regex ("a(b*)c" withGroup "bb")
     *                         ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups): Matcher[T with String] = and(MatcherWords.fullyMatch.regex(regexWithGroups))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and fullyMatch regex (decimalRegex)
     *                         ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = and(MatcherWords.fullyMatch.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher and fullyMatch regex (decimalRegex)
   *          ^
   * </pre>
   */
  def and(fullyMatchWord: FullyMatchWord): AndFullyMatchWord = new AndFullyMatchWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndIncludeWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and include regex (decimal)
     *                      ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = and(MatcherWords.include.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and include regex ("a(b*)c" withGroup "bb")
     *                      ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups): Matcher[T with String] = and(MatcherWords.include.regex(regexWithGroups))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and include regex (decimalRegex)
     *                      ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = and(MatcherWords.include.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher and include regex ("wor.d")
   *          ^
   * </pre>
   */
  def and(includeWord: IncludeWord): AndIncludeWord = new AndIncludeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndStartWithWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and startWith regex (decimal)
     *                        ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = and(MatcherWords.startWith.regex(regexString))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and startWith regex ("a(b*)c" withGroup "bb")
     *                        ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups): Matcher[T with String] = and(MatcherWords.startWith.regex(regexWithGroups))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and startWith regex (decimalRegex)
     *                        ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = and(MatcherWords.startWith.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher and startWith regex ("1.7")
   *          ^
   * </pre>
   */
  def and(startWithWord: StartWithWord): AndStartWithWord = new AndStartWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndEndWithWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and endWith regex (decimal)
     *                      ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = and(MatcherWords.endWith.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and endWith regex ("a(b*)c" withGroup "bb")
     *                      ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups): Matcher[T with String] = and(MatcherWords.endWith.regex(regexWithGroups))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and endWith regex (decimalRegex)
     *                      ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = and(MatcherWords.endWith.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher and endWith regex (decimalRegex)
   *          ^
   * </pre>
   */
  def and(endWithWord: EndWithWord): AndEndWithWord = new AndEndWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndNotWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not equal (3 - 1)
     *                  ^
     * </pre>
     */
    def equal(any: Any): MatcherFactory1[T, Equality] =
      outerInstance.and(MatcherWords.not.apply(MatcherWords.equal(any)))

    /**
     * This method enables the following syntax, for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * aMatcher and not equal (17.0 +- 0.2)
     *                  ^
     * </pre>
     */
    def equal[U](spread: Spread[U]): Matcher[T with U] = outerInstance.and(MatcherWords.not.equal(spread))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not equal (null)
     *                  ^
     * </pre>
     */
    def equal(o: Null): Matcher[T] = {
      outerInstance and {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              left != null,
              Resources("equaledNull"),
              Resources("didNotEqualNull"),
              Resources("midSentenceEqualedNull"),
              Resources("didNotEqualNull"), 
              Vector.empty, 
              Vector(left)
            )
          }
          override def toString: String = "not equal null"
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be (3 - 1)
     *                  ^
     * </pre>
     */
    def be(any: Any): Matcher[T] =
      outerInstance.and(MatcherWords.not.apply(MatcherWords.be(any)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not have length (3)
     *                  ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication): MatcherFactory1[T, Length] =
      outerInstance.and(MatcherWords.not.apply(MatcherWords.have.length(resultOfLengthWordApplication.expectedLength)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not have size (3)
     *                  ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): MatcherFactory1[T, Size] =
      outerInstance.and(MatcherWords.not.apply(MatcherWords.have.size(resultOfSizeWordApplication.expectedSize)))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not have message ("Message from Mars!")
     *                  ^
     * </pre>
     */
    def have(resultOfMessageWordApplication: ResultOfMessageWordApplication): MatcherFactory1[T, Messaging] =
      outerInstance.and(MatcherWords.not.apply(MatcherWords.have.message(resultOfMessageWordApplication.expectedMessage)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not have (author ("Melville"))
     *                  ^
     * </pre>
     */
    def have[U](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*): Matcher[T with U] =
      outerInstance.and(MatcherWords.not.apply(MatcherWords.have(firstPropertyMatcher, propertyMatchers: _*)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be &lt; (6)
     *                  ^
     * </pre>
     */
    def be[U](resultOfLessThanComparison: ResultOfLessThanComparison[U]): Matcher[T with U] =
      outerInstance.and(MatcherWords.not.be(resultOfLessThanComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be (null)
     *                  ^
     * </pre>
     */
    def be(o: Null): Matcher[T with AnyRef] = outerInstance.and(MatcherWords.not.be(o))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be &gt; (6)
     *                  ^
     * </pre>
     */
    def be[U](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[U]): Matcher[T with U] =
      outerInstance.and(MatcherWords.not.be(resultOfGreaterThanComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be &lt;= (2)
     *                  ^
     * </pre>
     */
    def be[U](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[U]): Matcher[T with U] =
      outerInstance.and(MatcherWords.not.be(resultOfLessThanOrEqualToComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be &gt;= (6)
     *                  ^
     * </pre>
     */
    def be[U](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[U]): Matcher[T with U] =
      outerInstance.and(MatcherWords.not.be(resultOfGreaterThanOrEqualToComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be === (6)
     *                  ^
     * </pre>
     */
    def be(tripleEqualsInvocation: TripleEqualsInvocation[_]): Matcher[T] =
      outerInstance.and(MatcherWords.not.be(tripleEqualsInvocation))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be ('empty)
     *                  ^
     * </pre>
     */
    def be(symbol: Symbol): Matcher[T with AnyRef] = outerInstance.and(MatcherWords.not.be(symbol))

    /**
     * This method enables the following syntax, where <code>odd</code> is a <a href="BeMatcher.html"><code>BeMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher and not be (odd)
     *                  ^
     * </pre>
     */
    def be[U](beMatcher: BeMatcher[U]): Matcher[T with U] = outerInstance.and(MatcherWords.not.be(beMatcher))

    /**
     * This method enables the following syntax, where <code>directory</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher and not be (directory)
     *                  ^
     * </pre>
     */
    def be[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = outerInstance.and(MatcherWords.not.be(bePropertyMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be a ('file)
     *                  ^
     * </pre>
     */
    def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication): Matcher[T with AnyRef] = outerInstance.and(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax, where <code>validMarks</code> is an <a href="AMatcher.html"><code>AMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher and not be a (validMarks)
     *                  ^
     * </pre>
     */
    def be[U](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[U]): Matcher[T with U] = outerInstance.and(MatcherWords.not.be(resultOfAWordApplication))
    
    /**
     * This method enables the following syntax, where <code>directory</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher and not be a (directory)
     *                  ^
     * </pre>
     */
    def be[U <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]): Matcher[T with U] = outerInstance.and(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be an ('apple)
     *                  ^
     * </pre>
     */
    def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication): Matcher[T with AnyRef] = outerInstance.and(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax, where <code>directory</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher and not be an (directory)
     *                  ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[T]) = outerInstance.and(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax, where <code>invalidMarks</code> is an <a href="AnMatcher.html"><code>AnMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher and not be an (invalidMarks)
     *                  ^
     * </pre>
     */
    def be[U](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[U]): Matcher[T with U] = outerInstance.and(MatcherWords.not.be(resultOfAnWordApplication))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be a [Book]
     *                  ^
     * </pre>
     */
    def be(aType: ResultOfATypeInvocation[_]): Matcher[T] =
      outerInstance.and(MatcherWords.not.apply(MatcherWords.be(aType)))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be an [Apple]
     *                  ^
     * </pre>
     */
    def be(anType: ResultOfAnTypeInvocation[_]): Matcher[T] =
      outerInstance.and(MatcherWords.not.apply(MatcherWords.be(anType)))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be theSameInstanceAs (otherString)
     *                  ^
     * </pre>
     */
    def be(resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): Matcher[T with AnyRef] = outerInstance.and(MatcherWords.not.be(resultOfTheSameInstanceAsApplication))

    /**
     * This method enables the following syntax, for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * aMatcher and not be (17.0 +- 0.2)
     *                  ^
     * </pre>
     */
    def be[U](spread: Spread[U]): Matcher[T with U] = outerInstance.and(MatcherWords.not.be(spread))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be definedAt (8)
     *                  ^
     * </pre>
     */
    def be[A, U <: PartialFunction[A, _]](resultOfDefinedAt: ResultOfDefinedAt[A]): Matcher[T with U] =
      outerInstance.and(MatcherWords.not.be(resultOfDefinedAt))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be sorted
     *                  ^
     * </pre>
     */
    def be(sortedWord: SortedWord) = 
      outerInstance.and(MatcherWords.not.be(sortedWord))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be readable
     *                  ^
     * </pre>
     */
    def be(readableWord: ReadableWord) = 
      outerInstance.and(MatcherWords.not.be(readableWord))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be writable
     *                  ^
     * </pre>
     */
    def be(writableWord: WritableWord) = 
      outerInstance.and(MatcherWords.not.be(writableWord))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be empty
     *                  ^
     * </pre>
     */
    def be(emptyWord: EmptyWord) = 
      outerInstance.and(MatcherWords.not.be(emptyWord))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not be defined
     *                  ^
     * </pre>
     */
    def be(definedWord: DefinedWord) = 
      outerInstance.and(MatcherWords.not.be(definedWord))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not fullyMatch regex (decimal)
     *                  ^
     * </pre>
     */
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      outerInstance.and(MatcherWords.not.fullyMatch(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not include regex (decimal)
     *                  ^
     * </pre>
     */
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      outerInstance.and(MatcherWords.not.include(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not include ("1.7")
     *                  ^
     * </pre>
     */
    def include(expectedSubstring: String): Matcher[T with String] =
      outerInstance.and(MatcherWords.not.include(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not startWith regex (decimal)
     *                  ^
     * </pre>
     */
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      outerInstance.and(MatcherWords.not.startWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not startWith ("1.7")
     *                  ^
     * </pre>
     */
    def startWith(expectedSubstring: String): Matcher[T with String] =
      outerInstance.and(MatcherWords.not.startWith(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not endWith regex (decimal)
     *                  ^
     * </pre>
     */
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      outerInstance.and(MatcherWords.not.endWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not endWith ("1.7")
     *                  ^
     * </pre>
     */
    def endWith(expectedSubstring: String): Matcher[T with String] =
      outerInstance.and(MatcherWords.not.endWith(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not contain (3)
     *                  ^
     * </pre>
     */
    def contain[U](expectedElement: U): MatcherFactory1[T, Containing] =
      outerInstance.and(MatcherWords.not.contain(expectedElement))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not contain oneOf (List(8, 1, 2))
     *                  ^
     * </pre>
     */
    def contain(right: ResultOfOneOfApplication): MatcherFactory1[T with Any, Containing] =
      outerInstance.and(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not contain atLeastOneOf (List(8, 1, 2))
     *                  ^
     * </pre>
     */
    def contain(right: ResultOfAtLeastOneOfApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.and(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not contain noneOf (List(8, 1, 2))
     *                  ^
     * </pre>
     */
    def contain(right: ResultOfNoneOfApplication): MatcherFactory1[T with Any, Containing] =
      outerInstance.and(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not contain theSameElementsAs (List(8, 1, 2))
     *                  ^
     * </pre>
     */
    def contain(right: ResultOfTheSameElementsAsApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.and(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not contain theSameElementsInOrderAs (List(8, 1, 2))
     *                  ^
     * </pre>
     */
    def contain(right: ResultOfTheSameElementsInOrderAsApplication): MatcherFactory1[T with Any, Sequencing] =
      outerInstance.and(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not contain only (List(8, 1, 2))
     *                  ^
     * </pre>
     */
    def contain(right: ResultOfOnlyApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.and(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not contain inOrderOnly (8, 1, 2)
     *                  ^
     * </pre>
     */
    def contain(right: ResultOfInOrderOnlyApplication): MatcherFactory1[T with Any, Sequencing] =
      outerInstance.and(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not contain allOf (8, 1, 2)
     *                  ^
     * </pre>
     */
    def contain(right: ResultOfAllOfApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.and(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not contain inOrder (8, 1, 2)
     *                  ^
     * </pre>
     */
    def contain(right: ResultOfInOrderApplication): MatcherFactory1[T, Sequencing] =
      outerInstance.and(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not contain atMostOneOf (8, 1, 2)
     *                          ^
     * </pre>
     */
    def contain(right: ResultOfAtMostOneOfApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.and(MatcherWords.not.contain(right))

// TODO: Write tests and impl for contain ResultOfKey/ValueWordApplication
    /**
     * This method enables the following syntax given a <code>Matcher</code>:
     *
     * <pre class="stHighlight">
     * aMatcher and not contain key ("three")
     *                  ^
     * </pre>
     */
    def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication): MatcherFactory1[T, KeyMapping] =
      outerInstance.and(MatcherWords.not.contain(resultOfKeyWordApplication))

    /**
     * This method enables the following syntax given a <code>Matcher</code>:
     *
     * <pre class="stHighlight">
     * aMatcher and not contain value (3)
     *                  ^
     * </pre>
     */
    def contain(resultOfValueWordApplication: ResultOfValueWordApplication): MatcherFactory1[T, ValueMapping] =
      outerInstance.and(MatcherWords.not.contain(resultOfValueWordApplication))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher and not contain value (3)
   *          ^
   * </pre>
   */
  def and(notWord: NotWord): AndNotWord = new AndNotWord
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher and exist
   *          ^
   * </pre>
   */
  def and(existWord: ExistWord): MatcherFactory1[T, Existence] = 
    outerInstance.and(MatcherWords.exist.matcherFactory)
    
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher and not (exist)
   *          ^
   * </pre>
   */
  def and(notExist: ResultOfNotExist): MatcherFactory1[T, Existence] = 
    outerInstance.and(MatcherWords.not.exist)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrHaveWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or have length (3 - 1)
     *                  ^
     * </pre>
     */
    def length(expectedLength: Long): MatcherFactory1[T, Length] = or(MatcherWords.have.length(expectedLength))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or have size (3 - 1)
     *                  ^
     * </pre>
     */
    def size(expectedSize: Long): MatcherFactory1[T, Size] = or(MatcherWords.have.size(expectedSize))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or have message ("Message from Mars!")
     *                  ^
     * </pre>
     */
    def message(expectedMessage: String): MatcherFactory1[T, Messaging] = or(MatcherWords.have.message(expectedMessage))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher or have size (3 - 1)
   *          ^
   * </pre>
   */
  def or(haveWord: HaveWord): OrHaveWord = new OrHaveWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrContainWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or contain (3 - 1)
     *                     ^
     * </pre>
     */
    def apply[U](expectedElement: Any): MatcherFactory1[T with U, Containing] = outerInstance.or(MatcherWords.contain(expectedElement))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or contain key ("one")
     *                     ^
     * </pre>
     */
    def key(expectedKey: Any): MatcherFactory1[T, KeyMapping] = outerInstance.or(MatcherWords.contain.key(expectedKey))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or contain value (1)
     *                     ^
     * </pre>
     */
    def value(expectedValue: Any): MatcherFactory1[T, ValueMapping] = outerInstance.or(MatcherWords.contain.value(expectedValue))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or contain theSameElementsAs List(1, 2, 3)
     *                     ^
     * </pre>
     */
    def theSameElementsAs(right: GenTraversable[_]): MatcherFactory1[T, Aggregating] = 
      outerInstance.or(MatcherWords.contain.theSameElementsAs(right))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or contain theSameElementsInOrderAs List(1, 2, 3)
     *                     ^
     * </pre>
     */
    def theSameElementsInOrderAs(right: GenTraversable[_]): MatcherFactory1[T, Sequencing] = 
      outerInstance.or(MatcherWords.contain.theSameElementsInOrderAs(right))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or contain allOf (1, 2, 3)
     *                     ^
     * </pre>
     */
    def allOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory1[T, Aggregating] =
      outerInstance.or(MatcherWords.contain.allOf(firstEle, secondEle, remainingEles.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or contain inOrder (1, 2, 3)
     *                     ^
     * </pre>
     */
    def inOrder(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory1[T, Sequencing] =
      outerInstance.or(MatcherWords.contain.inOrder(firstEle, secondEle, remainingEles.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or contain oneOf (1, 2, 3)
     *                     ^
     * </pre>
     */
    def oneOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory1[T, Containing] =
      outerInstance.or(MatcherWords.contain.oneOf(firstEle, secondEle, remainingEles.toList: _*))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or contain atLeastOneOf (1, 2, 3)
     *                     ^
     * </pre>
     */
    def atLeastOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory1[T, Aggregating] =
      outerInstance.or(MatcherWords.contain.atLeastOneOf(firstEle, secondEle, remainingEles.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or contain only (1, 2, 3)
     *                     ^
     * </pre>
     */
    def only(right: Any*): MatcherFactory1[T, Aggregating] = 
      outerInstance.or(MatcherWords.contain.only(right.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or contain inOrderOnly (1, 2, 3)
     *                     ^
     * </pre>
     */
    def inOrderOnly(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory1[T, Sequencing] =
      outerInstance.or(MatcherWords.contain.inOrderOnly(firstEle, secondEle, remainingEles.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or contain noneOf (1, 2, 3)
     *                     ^
     * </pre>
     */
    def noneOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory1[T, Containing] =
      outerInstance.or(MatcherWords.contain.noneOf(firstEle, secondEle, remainingEles.toList: _*))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or contain atMostOneOf (1, 2, 3)
     *                     ^
     * </pre>
     */
    def atMostOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory1[T, Aggregating] =
      outerInstance.or(MatcherWords.contain.atMostOneOf(firstEle, secondEle, remainingEles.toList: _*))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher or contain value (1)
   *          ^
   * </pre>
   */
  def or(containWord: ContainWord): OrContainWord = new OrContainWord
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrBeWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or be a ('directory)
     *                ^
     * </pre>
     */
    def a(symbol: Symbol): Matcher[T with AnyRef] = or(MatcherWords.be.a(symbol))

    /**
     * This method enables the following syntax, where <code>directory</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher or be a (directory)
     *                ^
     * </pre>
     */
    def a[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = or(MatcherWords.be.a(bePropertyMatcher))
    
    /**
     * This method enables the following syntax, where <code>positiveNumber</code> and <code>validNumber</code> are <a href="AMatcher.html"><code>AMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher or be a (validNumber)
     *                ^
     * </pre>
     */
    def a[U](aMatcher: AMatcher[U]): Matcher[T with U] = or(MatcherWords.be.a(aMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or be an ('apple)
     *                ^
     * </pre>
     */
    def an(symbol: Symbol): Matcher[T with AnyRef] = or(MatcherWords.be.an(symbol))

    /**
     * This method enables the following syntax, where <code>orange</code> and <code>apple</code> are <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher or be an (apple)
     *                ^
     * </pre>
     */
    def an[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = or(MatcherWords.be.an(bePropertyMatcher))

    /**
     * This method enables the following syntax, where <code>oddNumber</code> and <code>integerNumber</code> are <a href="AnMatcher.html"><code>AnMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher or be an (integerNumber)
     *                ^
     * </pre>
     */
    def an[U](anMatcher: AnMatcher[U]): Matcher[T with U] = or(MatcherWords.be.an(anMatcher))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or be theSameInstanceAs (otherString)
     *                ^
     * </pre>
     */
    def theSameInstanceAs(anyRef: AnyRef): Matcher[T with AnyRef] = or(MatcherWords.be.theSameInstanceAs(anyRef))
    
    /**
     * This method enables the following syntax, where <code>fraction</code> refers to a <code>PartialFunction</code>:
     *
     * <pre class="stHighlight">
     * aMatcher or be definedAt (8)
     *                ^
     * </pre>
     */
    def definedAt[A, U <: PartialFunction[A, _]](right: A): Matcher[T with U] = or(MatcherWords.be.definedAt(right))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher or be a ('directory)
   *          ^
   * </pre>
   */
  def or(beWord: BeWord): OrBeWord = new OrBeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrFullyMatchWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or fullyMatch regex (decimal)
     *                        ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = or(MatcherWords.fullyMatch.regex(regexString))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or fullyMatch regex ("a(b*)c" withGroup "bb")
     *                        ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups): Matcher[T with String] = or(MatcherWords.fullyMatch.regex(regexWithGroups))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or fullyMatch regex (decimal)
     *                        ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = or(MatcherWords.fullyMatch.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher or fullyMatch regex (decimal)
   *          ^
   * </pre>
   */
  def or(fullyMatchWord: FullyMatchWord): OrFullyMatchWord = new OrFullyMatchWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrIncludeWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or include regex (decimal)
     *                     ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = or(MatcherWords.include.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or include regex ("a(b*)c" withGroup "bb")
     *                     ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups): Matcher[T with String] = or(MatcherWords.include.regex(regexWithGroups))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or include regex (decimal)
     *                     ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = or(MatcherWords.include.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher or include regex ("1.7")
   *          ^
   * </pre>
   */
  def or(includeWord: IncludeWord): OrIncludeWord = new OrIncludeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrStartWithWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or startWith regex (decimal)
     *                       ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = or(MatcherWords.startWith.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or startWith regex ("a(b*)c" withGroup "bb")
     *                       ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups): Matcher[T with String] = or(MatcherWords.startWith.regex(regexWithGroups))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or startWith regex (decimal)
     *                       ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = or(MatcherWords.startWith.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher or startWith regex ("1.7")
   *          ^
   * </pre>
   */
  def or(startWithWord: StartWithWord): OrStartWithWord = new OrStartWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrEndWithWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or endWith regex (decimal)
     *                     ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = or(MatcherWords.endWith.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or endWith regex ("d(e*)f" withGroup "ee")
     *                     ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups): Matcher[T with String] = or(MatcherWords.endWith.regex(regexWithGroups))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or endWith regex (decimal)
     *                     ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = or(MatcherWords.endWith.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher or endWith regex ("7b")
   *          ^
   * </pre>
   */
  def or(endWithWord: EndWithWord): OrEndWithWord = new OrEndWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrNotWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not equal (2)
     *                 ^
     * </pre>
     */
    def equal(any: Any): MatcherFactory1[T, Equality] =
      outerInstance.or(MatcherWords.not.apply(MatcherWords.equal(any)))

    /**
     * This method enables the following syntax for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * aMatcher or not equal (17.0 +- 0.2)
     *                 ^
     * </pre>
     */
    def equal[U](spread: Spread[U]): Matcher[T with U] = outerInstance.or(MatcherWords.not.equal(spread))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not equal (null)
     *                 ^
     * </pre>
     */
    def equal(o: Null): Matcher[T] = {
      outerInstance or {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              left != null,
              Resources("equaledNull"),
              Resources("didNotEqualNull"),
              Resources("midSentenceEqualedNull"),
              Resources("didNotEqualNull"), 
              Vector.empty, 
              Vector(left)
            )
          }
          override def toString: String = "not equal null"
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be (2)
     *                 ^
     * </pre>
     */
    def be(any: Any): Matcher[T] =
      outerInstance.or(MatcherWords.not.apply(MatcherWords.be(any)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not have length (3)
     *                 ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication): MatcherFactory1[T, Length] =
      outerInstance.or(MatcherWords.not.apply(MatcherWords.have.length(resultOfLengthWordApplication.expectedLength)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not have size (3)
     *                 ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): MatcherFactory1[T, Size] =
      outerInstance.or(MatcherWords.not.apply(MatcherWords.have.size(resultOfSizeWordApplication.expectedSize)))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not have message ("Message from Mars!")
     *                 ^
     * </pre>
     */
    def have(resultOfMessageWordApplication: ResultOfMessageWordApplication): MatcherFactory1[T, Messaging] =
      outerInstance.or(MatcherWords.not.apply(MatcherWords.have.message(resultOfMessageWordApplication.expectedMessage)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not have (author ("Melville"))
     *                 ^
     * </pre>
     */
    def have[U](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*): Matcher[T with U] =
      outerInstance.or(MatcherWords.not.apply(MatcherWords.have(firstPropertyMatcher, propertyMatchers: _*)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be (null)
     *                 ^
     * </pre>
     */
    def be(o: Null): Matcher[T with AnyRef] = outerInstance.or(MatcherWords.not.be(o))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be &lt; (8)
     *                 ^
     * </pre>
     */
    def be[U](resultOfLessThanComparison: ResultOfLessThanComparison[U]): Matcher[T with U] =
      outerInstance.or(MatcherWords.not.be(resultOfLessThanComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be &gt; (6)
     *                 ^
     * </pre>
     */
    def be[U](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[U]): Matcher[T with U] =
      outerInstance.or(MatcherWords.not.be(resultOfGreaterThanComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be &lt;= (2)
     *                 ^
     * </pre>
     */
    def be[U](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[U]): Matcher[T with U] =
      outerInstance.or(MatcherWords.not.be(resultOfLessThanOrEqualToComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be &gt;= (6)
     *                 ^
     * </pre>
     */
    def be[U](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[U]): Matcher[T with U] =
      outerInstance.or(MatcherWords.not.be(resultOfGreaterThanOrEqualToComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be === (8)
     *                 ^
     * </pre>
     */
    def be(tripleEqualsInvocation: TripleEqualsInvocation[_]): Matcher[T] =
      outerInstance.or(MatcherWords.not.be(tripleEqualsInvocation))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be ('empty)
     *                 ^
     * </pre>
     */
    def be(symbol: Symbol): Matcher[T with AnyRef] = outerInstance.or(MatcherWords.not.be(symbol))

    /**
     * This method enables the following syntax, where <code>odd</code> is a <a href="BeMatcher.html"><code>BeMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher or not be (odd)
     *                 ^
     * </pre>
     */
    def be[U](beMatcher: BeMatcher[U]): Matcher[T with U] = outerInstance.or(MatcherWords.not.be(beMatcher))

    /**
     * This method enables the following syntax, where <code>file</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher or not be (file)
     *                 ^
     * </pre>
     */
    def be[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = outerInstance.or(MatcherWords.not.be(bePropertyMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be a ('file)
     *                 ^
     * </pre>
     */
    def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication): Matcher[T with AnyRef] = outerInstance.or(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax, where <code>validMarks</code> is an <a href="AMatcher.html"><code>AMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher or not be a (validMarks)
     *                 ^
     * </pre>
     */
    def be[U](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[U]): Matcher[T with U] = outerInstance.or(MatcherWords.not.be(resultOfAWordApplication))
    
    /**
     * This method enables the following syntax, where <code>file</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher or not be a (file)
     *                 ^
     * </pre>
     */
    def be[U <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]): Matcher[T with U] = outerInstance.or(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be an ('apple)
     *                    ^
     * </pre>
     */
    def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication): Matcher[T with AnyRef] = outerInstance.or(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax, where <code>apple</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher or not be an (apple)
     *                 ^
     * </pre>
     */
    def be[U <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U]): Matcher[T with U] = outerInstance.or(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax, where <code>invalidMarks</code> is an <a href="AnMatcher.html"><code>AnMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcher and not be an (invalidMarks)
     *                  ^
     * </pre>
     */
    def be[U](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[U]): Matcher[T with U] = outerInstance.or(MatcherWords.not.be(resultOfAnWordApplication))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be a [Book]
     *                 ^
     * </pre>
     */
    def be(aType: ResultOfATypeInvocation[_]): Matcher[T] = outerInstance.or(MatcherWords.not.apply(MatcherWords.be(aType)))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be an [Book]
     *                 ^
     * </pre>
     */
    def be(anType: ResultOfAnTypeInvocation[_]): Matcher[T] = outerInstance.or(MatcherWords.not.apply(MatcherWords.be(anType)))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be theSameInstanceAs (string)
     *                    ^
     * </pre>
     */
    def be(resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): Matcher[T with AnyRef] = outerInstance.or(MatcherWords.not.be(resultOfTheSameInstanceAsApplication))

    /**
     * This method enables the following syntax for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * aMatcher or not be (17.0 +- 0.2)
     *                 ^
     * </pre>
     */
    def be[U](spread: Spread[U]): Matcher[T with U] = outerInstance.or(MatcherWords.not.be(spread))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be definedAt (8)
     *                 ^
     * </pre>
     */
    def be[A, U <: PartialFunction[A, _]](resultOfDefinedAt: ResultOfDefinedAt[A]): Matcher[T with U] =
      outerInstance.or(MatcherWords.not.be(resultOfDefinedAt))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be sorted
     *                 ^
     * </pre>
     */
    def be(sortedWord: SortedWord) = 
      outerInstance.or(MatcherWords.not.be(sortedWord))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be readable
     *                 ^
     * </pre>
     */
    def be(readableWord: ReadableWord) = 
      outerInstance.or(MatcherWords.not.be(readableWord))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be empty
     *                 ^
     * </pre>
     */
    def be(emptyWord: EmptyWord) = 
      outerInstance.or(MatcherWords.not.be(emptyWord))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be writable
     *                 ^
     * </pre>
     */
    def be(writableWord: WritableWord) = 
      outerInstance.or(MatcherWords.not.be(writableWord))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not be defined
     *                 ^
     * </pre>
     */
    def be(definedWord: DefinedWord) = 
      outerInstance.or(MatcherWords.not.be(definedWord))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not fullyMatch regex (decimal)
     *                 ^
     * </pre>
     */
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      outerInstance.or(MatcherWords.not.fullyMatch(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not include regex (decimal)
     *                 ^
     * </pre>
     */
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      outerInstance.or(MatcherWords.not.include(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not include ("1.7")
     *                 ^
     * </pre>
     */
    def include(expectedSubstring: String): Matcher[T with String] =
      outerInstance.or(MatcherWords.not.include(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not startWith regex (decimal)
     *                 ^
     * </pre>
     */
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      outerInstance.or(MatcherWords.not.startWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not startWith ("1.7")
     *                 ^
     * </pre>
     */
    def startWith(expectedSubstring: String): Matcher[T with String] =
      outerInstance.or(MatcherWords.not.startWith(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not endWith regex (decimal)
     *                 ^
     * </pre>
     */
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      outerInstance.or(MatcherWords.not.endWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not endWith ("1.7")
     *                 ^
     * </pre>
     */
    def endWith(expectedSubstring: String): Matcher[T with String] =
      outerInstance.or(MatcherWords.not.endWith(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not contain (3)
     *                 ^
     * </pre>
     */
    def contain[U](expectedElement: U): MatcherFactory1[T, Containing] =
      outerInstance.or(MatcherWords.not.contain(expectedElement))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (not contain key ("two") or not contain key ("three"))
     *                                                                    ^
     * </pre>
     */
/*
    def contain[U](resultOfKeyWordApplication: ResultOfKeyWordApplication): MatcherFactory1[T, KeyMapping] =
      outerInstance.or(MatcherWords.not.contain(resultOfKeyWordApplication))
*/
// TODO: Tests and implementation

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (not contain value (2) or not contain value (3))
     *                                                                  ^
     * </pre>
     */
/*
    def contain[U](resultOfValueWordApplication: ResultOfValueWordApplication[U]): MatcherFactory1[T, ValueMapping] =
      outerInstance.or(MatcherWords.not.contain(resultOfValueWordApplication))
*/
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not contain oneOf (8, 1, 2)
     *                 ^
     * </pre>
     */
    def contain(right: ResultOfOneOfApplication): MatcherFactory1[T, Containing] =
      outerInstance.or(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not contain atLeastOneOf (8, 1, 2)
     *                 ^
     * </pre>
     */
    def contain(right: ResultOfAtLeastOneOfApplication): MatcherFactory1[T, Aggregating] =
      outerInstance.or(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not contain noneOf (8, 1, 2)
     *                 ^
     * </pre>
     */
    def contain(right: ResultOfNoneOfApplication): MatcherFactory1[T, Containing] =
      outerInstance.or(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not contain theSameElementsAs (8, 1, 2)
     *                 ^
     * </pre>
     */
    def contain(right: ResultOfTheSameElementsAsApplication): MatcherFactory1[T, Aggregating] =
      outerInstance.or(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not contain theSameElementsInOrderAs (8, 1, 2)
     *                 ^
     * </pre>
     */
    def contain(right: ResultOfTheSameElementsInOrderAsApplication): MatcherFactory1[T, Sequencing] =
      outerInstance.or(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not contain inOrderOnly (8, 1, 2)
     *                 ^
     * </pre>
     */
    def contain(right: ResultOfInOrderOnlyApplication): MatcherFactory1[T, Sequencing] =
      outerInstance.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not contain only (8, 1, 2)
     *                 ^
     * </pre>
     */
    def contain(right: ResultOfOnlyApplication): MatcherFactory1[T, Aggregating] =
      outerInstance.or(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not contain allOf (8, 1, 2)
     *                 ^
     * </pre>
     */
    def contain(right: ResultOfAllOfApplication): MatcherFactory1[T, Aggregating] =
      outerInstance.or(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not contain inOrder (8, 1, 2)
     *                 ^
     * </pre>
     */
    def contain(right: ResultOfInOrderApplication): MatcherFactory1[T, Sequencing] =
      outerInstance.or(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher not contain atMostOneOf (8, 1, 2)
     *              ^
     * </pre>
     */
    def contain(right: ResultOfAtMostOneOfApplication): MatcherFactory1[T, Aggregating] =
      outerInstance.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>Matcher</code>:
     *
     * <pre class="stHighlight">
     * aMatcher or not contain key ("three")
     *                 ^
     * </pre>
     */
    def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication): MatcherFactory1[T, KeyMapping] =
      outerInstance.or(MatcherWords.not.contain(resultOfKeyWordApplication))

    /**
     * This method enables the following syntax given a <code>Matcher</code>:
     *
     * <pre class="stHighlight">
     * aMatcher or not contain value (3)
     *                 ^
     * </pre>
     */
    def contain(resultOfValueWordApplication: ResultOfValueWordApplication): MatcherFactory1[T, ValueMapping] =
      outerInstance.or(MatcherWords.not.contain(resultOfValueWordApplication))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher or not contain value (3)
   *          ^
   * </pre>
   */
  def or(notWord: NotWord): OrNotWord = new OrNotWord
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher or exist
   *          ^
   * </pre>
   */
  def or(existWord: ExistWord): MatcherFactory1[T, Existence] = 
    outerInstance.or(MatcherWords.exist.matcherFactory)
    
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcher or not (exist)
   *          ^
   * </pre>
   */
  def or(notExist: ResultOfNotExist): MatcherFactory1[T, Existence] = 
    outerInstance.or(MatcherWords.not.exist)

  /**
   * Create a new <code>Matcher</code> that will produce <code>MatchResult</code> by applying original <code>MatchResult</code>
   * produced by this <code>Matcher</code> to the passed <code>m2m</code> function.  In other words, the <code>MatchResult</code>
   * produced by this <code>Matcher</code> will be passed to <code>m2m</code> to produce the final <code>MatchResult</code>
   *
   * @param m2m a function to apply to the original <code>MatchResult</code> produced by this <code>Matcher</code>
   * @return a new <code>Matcher</code> that will produce <code>MatchResult</code> by applying original <code>MatchResult</code>
   *         produced by this <code>Matcher</code> to the passed <code>m2m</code> function
   */
  def mapResult(m2m: MatchResult => MatchResult): Matcher[T] =
    new Matcher[T] {
      def apply(o: T): MatchResult = m2m(outerInstance(o))
    }

  /**
   * Create a new <code>Matcher</code> that will produce <code>MatchResult</code> that contains error messages constructed
   * using arguments that are transformed by the passed <code>a2s</code> function.  In other words, the <code>MatchResult</code>
   * produced by this <code>Matcher</code> will use arguments transformed by <code>a2s</code> function to construct the final
   * error messages.
   *
   * @param a2s a function to transform arguments of error messages.
   * @return a new <code>Matcher</code> that will produce <code>MatchResult</code> that contains error messages constructed
   *         using arguments that are transformed by the passed <code>a2s</code> function.
   */
  def mapArgs(a2s: Any => String): Matcher[T] =
    new Matcher[T] {
      def apply(o: T): MatchResult = {
        val mr = outerInstance(o)
        mr.copy(
          failureMessageArgs = mr.failureMessageArgs.map((LazyArg(_) { a2s })),
          negatedFailureMessageArgs = mr.negatedFailureMessageArgs.map((LazyArg(_) { a2s })),
          midSentenceFailureMessageArgs = mr.midSentenceFailureMessageArgs.map((LazyArg(_) { a2s })),
          midSentenceNegatedFailureMessageArgs = mr.midSentenceNegatedFailureMessageArgs.map((LazyArg(_) { a2s }))
        )
      }
    }
}

/**
 * Companion object for trait <code>Matcher</code> that provides a
 * factory method that creates a <code>Matcher[T]</code> from a
 * passed function of type <code>(T =&gt; MatchResult)</code>.
 *
 * @author Bill Venners
 */
object Matcher {

  /**
   * Factory method that creates a <code>Matcher[T]</code> from a
   * passed function of type <code>(T =&gt; MatchResult)</code>.
   *
   * @author Bill Venners
   */
  def apply[T](fun: T => MatchResult)(implicit ev: Manifest[T]): Matcher[T] =
    new Matcher[T] {
      def apply(left: T) = fun(left)
      override def toString: String = "Matcher[" + ev.erasure.getName + "](" + ev.erasure.getName + " => MatchResult)"
    }
}

