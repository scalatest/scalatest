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
import org.scalautils.Interval
import org.scalautils.TripleEqualsInvocation
import org.scalatest.FailureMessages
import org.scalatest.words.FullyMatchWord
import org.scalatest.words.StartWithWord
import org.scalatest.words.EndWithWord
import org.scalatest.words.IncludeWord
import org.scalatest.words.HaveWord
import org.scalatest.words.BeWord
import org.scalatest.words.NotWord
import org.scalatest.words.ContainWord
import org.scalatest.words.NewContainWord
import org.scalatest.words.ResultOfLengthWordApplication
import org.scalatest.words.ResultOfSizeWordApplication
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
import org.scalatest.words.ResultOfKeyWordApplication
import org.scalatest.words.ResultOfValueWordApplication
import org.scalatest.words.RegexWithGroups
import org.scalatest.words.ResultOfDefinedAt
import org.scalatest.words.ResultOfNewOneOfApplication
import org.scalatest.words.ResultOfAtLeastOneOfApplication
import org.scalatest.words.ResultOfNewNoneOfApplication
import org.scalatest.words.ResultOfNewTheSameElementsAsApplication
import org.scalatest.words.ResultOfNewTheSameElementsInOrderAsApplication
import org.scalatest.words.ResultOfNewOnlyApplication
import org.scalatest.words.ResultOfNewAllOfApplication
import org.scalatest.words.ResultOfNewInOrderOnlyApplication
import org.scalatest.words.ResultOfNewInOrderApplication

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
    }

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
   * @param the matcher to logical-or with this matcher
   * @return a matcher that performs the logical-or of this and the passed matcher
   */
  def or[U <: T](rightMatcher: Matcher[U]): Matcher[U] =
    new Matcher[U] {
      def apply(left: U): MatchResult = {
        orMatchersAndApply(left, outerInstance, rightMatcher)
      }
    }

  def or[U, TC1[_]](rightMatcherFactory1: MatcherFactory1[U, TC1]): MatcherFactory1[T with U, TC1] =
    new MatcherFactory1[T with U, TC1] {
      def matcher[V <: T with U : TC1]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val rightMatcher = rightMatcherFactory1.matcher
            orMatchersAndApply(left, outerInstance, rightMatcher)
          }
        }
      }
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
     * Array(1, 2) should (have length (2) and have length (3 - 1))
     *                                              ^
     * </pre>
     */
    def length(expectedLength: Long): MatcherFactory1[T, Length] = and(MatcherWords.have.length(expectedLength))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (have size (2) and have size (3 - 1))
     *                                            ^ 
     * </pre>
     */
    def size(expectedSize: Long): MatcherFactory1[T, Size] = and(MatcherWords.have.size(expectedSize))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (have size (2) and have size (3 - 1))
   *                                   ^ 
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
     * Array(1, 2) should (contain (2) and contain (3 - 1))
     *                                             ^
     * </pre>
     */
    def apply[U](expectedElement: Any): MatcherFactory1[T with U, Containing] = outerInstance.and(MatcherWords.contain(expectedElement))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (contain key ("two") and contain key ("one"))
     *                                                                     ^
     * </pre>
     */
    def key[U](expectedElement: U): Matcher[T with scala.collection.GenMap[U, Any]] = outerInstance.and(MatcherWords.contain.key(expectedElement))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (contain value (2) and contain value (1))
     *                                                                   ^
     * </pre>
     */
    def value[U](expectedValue: U): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] = outerInstance.and(MatcherWords.contain.value(expectedValue))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain theSameElementsAs List(1, 2, 3))
     *                                                                           ^
     * </pre>
     */
    def oldTheSameElementsAs[E](right: GenTraversable[E])(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      outerInstance.and(MatcherWords.contain.oldTheSameElementsAs(right)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain theSameElementsInOrderAs List(1, 2, 3))
     *                                                                           ^
     * </pre>
     */
    def oldTheSameElementsInOrderAs[E](right: GenTraversable[E])(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      outerInstance.and(MatcherWords.contain.oldTheSameElementsInOrderAs(right)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain allOf (1, 2, 3))
     *                                                                           ^
     * </pre>
     */
    def oldAllOf[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      outerInstance.and(MatcherWords.contain.oldAllOf(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain inOrder (1, 2, 3))
     *                                                                           ^
     * </pre>
     */
    def oldInOrder[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      outerInstance.and(MatcherWords.contain.oldInOrder(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain oneOf (1, 3, 3))
     *                                                                           ^
     * </pre>
     */
    def oldOneOf[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      outerInstance.and(MatcherWords.contain.oldOneOf(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain only (3, 1))
     *                                                                           ^
     * </pre>
     */
    def oldOnly[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      outerInstance.and(MatcherWords.contain.oldOnly(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain inOrderOnly (1, 3))
     *                                                                           ^
     * </pre>
     */
    def oldInOrderOnly[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      outerInstance.and(MatcherWords.contain.oldInOrderOnly(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain noneOf (7, 8, 9))
     *                                                                           ^
     * </pre>
     */
    def oldNoneOf[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      outerInstance.and(MatcherWords.contain.oldNoneOf(right.toList: _*)(equality))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (contain a (positiveNumber) and contain a (validNumber))
     *                                                       ^
     * </pre>
     */
    def a[E](aMatcher: AMatcher[E]): Matcher[T with GenTraversable[E]] = 
      and(MatcherWords.contain.a(aMatcher))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (contain a (positiveNumber) and contain an (invalidNumber))
     *                                                       ^
     * </pre>
     */
    def an[E](anMatcher: AnMatcher[E]): Matcher[T with GenTraversable[E]] = 
      and(MatcherWords.contain.an(anMatcher))
  }
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Map("one" -&gt; 1, "two" -&gt; 2) should (contain key ("two") and contain key ("one"))
   *                                                               ^ 
   * </pre>
   */
  def and(containWord: ContainWord): AndContainWord = new AndContainWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndNewContainWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (contain (2) and contain (3 - 1))
     *                                             ^
     * </pre>
     */
    //def apply[U](expectedElement: Any): MatcherFactory1[T with U, Containing] = outerInstance.and(MatcherWords.contain(expectedElement))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (contain key ("two") and contain key ("one"))
     *                                                                     ^
     * </pre>
     */
    //def newKey[U](expectedElement: U): Matcher[T with scala.collection.GenMap[U, Any]] = outerInstance.and(MatcherWords.contain.key(expectedElement))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (contain value (2) and contain value (1))
     *                                                                   ^
     * </pre>
     */
    //def newValue[U](expectedValue: U): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] = outerInstance.and(MatcherWords.contain.value(expectedValue))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain theSameElementsAs (1, 3, 3))
     *                                                                           ^
     * </pre>
     */
    def newTheSameElementsAs(right: GenTraversable[_]): MatcherFactory1[T with Any, Aggregating] = 
      outerInstance.and(MatcherWords.newContain.newTheSameElementsAs(right))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain theSameElementsInOrderAs (1, 3, 3))
     *                                                                           ^
     * </pre>
     */
    def newTheSameElementsInOrderAs(right: GenTraversable[_]): MatcherFactory1[T with Any, Aggregating] = 
      outerInstance.and(MatcherWords.newContain.newTheSameElementsInOrderAs(right))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and contain inOrderOnly (1, 2, 3)
     *                      ^
     * </pre>
     */
    def newInOrderOnly(right: Any*): MatcherFactory1[T with Any, Aggregating] = 
      outerInstance.and(MatcherWords.newContain.newInOrderOnly(right.toList: _*))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain allOf (1, 3, 3))
     *                                                                           ^
     * </pre>
     */
    def newAllOf(right: Any*): MatcherFactory1[T with Any, Aggregating] = 
      outerInstance.and(MatcherWords.newContain.newAllOf(right.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain inOrder (1, 3, 3))
     *                                                                           ^
     * </pre>
     */
    def newInOrder(right: Any*): MatcherFactory1[T with Any, Aggregating] = 
      outerInstance.and(MatcherWords.newContain.newInOrder(right.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain oneOf (1, 3, 3))
     *                                                                           ^
     * </pre>
     */
    def newOneOf(right: Any*): MatcherFactory1[T with Any, Containing] = 
      outerInstance.and(MatcherWords.newContain.newOneOf(right.toList: _*))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain atLeastOneOf (1, 3, 3))
     *                                                                           ^
     * </pre>
     */
    def atLeastOneOf(right: Any*): MatcherFactory1[T with Any, Aggregating] = 
      outerInstance.and(MatcherWords.newContain.atLeastOneOf(right.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain atLeastOneOf (1, 3, 3))
     *                                                                           ^
     * </pre>
     */
    def newOnly(right: Any*): MatcherFactory1[T with Any, Aggregating] = 
      outerInstance.and(MatcherWords.newContain.newOnly(right.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain inOrderOnly (1, 3))
     *                                                                           ^
     * </pre>
     */
    //def newInOrderOnly[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      //outerInstance.and(MatcherWords.contain.inOrderOnly(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain noneOf (7, 8, 9))
     *                                                                           ^
     * </pre>
     */
    //def newNoneOf[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      //outerInstance.and(MatcherWords.contain.noneOf(right.toList: _*)(equality))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) and contain noneOf (1, 3, 3))
     *                                                                           ^
     * </pre>
     */
    def newNoneOf(right: Any*): MatcherFactory1[T with Any, Containing] = 
      outerInstance.and(MatcherWords.newContain.newNoneOf(right.toList: _*))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (contain a (positiveNumber) and contain a (validNumber))
     *                                                       ^
     * </pre>
     */
    //def newA[E](aMatcher: AMatcher[E]): Matcher[T with GenTraversable[E]] = 
      //and(MatcherWords.contain.a(aMatcher))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (contain a (positiveNumber) and contain an (invalidNumber))
     *                                                       ^
     * </pre>
     */
    //def newAn[E](anMatcher: AnMatcher[E]): Matcher[T with GenTraversable[E]] = 
      //and(MatcherWords.contain.an(anMatcher))
  }
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Map("one" -&gt; 1, "two" -&gt; 2) should (contain key ("two") and contain key ("one"))
   *                                                               ^ 
   * </pre>
   */
  def and(containWord: NewContainWord): AndNewContainWord = new AndNewContainWord
  
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
     * isFileMock should (be a ('file) and be a ('file))
     *                                        ^
     * </pre>
     */
    def a(symbol: Symbol): Matcher[T with AnyRef] = and(MatcherWords.be.a(symbol))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * myFile should (be a (file) and be a (file))
     *                                   ^
     * </pre>
     */
    def a[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = and(MatcherWords.be.a(bePropertyMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (be a (positiveNumber) and be a (validNumber))
     *                                             ^
     * </pre>
     */
    def a[U](aMatcher: AMatcher[U]): Matcher[T with U] = and(MatcherWords.be.a(aMatcher))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * isAppleMock should (be an ('apple) and be an ('apple))
     *                                           ^
     * </pre>
     */
    def an(symbol: Symbol): Matcher[T with AnyRef] = and(MatcherWords.be.an(symbol))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * isAppleMock should (be an (apple) and be an (apple))
     *                                          ^
     * </pre>
     */
    def an[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = and(MatcherWords.be.an(bePropertyMatcher))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (be an (oddNumber) and be an (integerNumber))
     *                                         ^
     * </pre>
     */
    def an[U](anMatcher: AnMatcher[U]): Matcher[T with U] = and(MatcherWords.be.an(anMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should (be theSameInstanceAs (string) and be theSameInstanceAs (string))
     *                                                  ^
     * </pre>
     */
    def theSameInstanceAs(anyRef: AnyRef): Matcher[T with AnyRef] = and(MatcherWords.be.theSameInstanceAs(anyRef))
    
    /**
     * This method enables the following syntax, where <code>fraction</code> refers to a <code>PartialFunction</code>:
     *
     * <pre class="stHighlight">
     * fraction should (be definedAt (6) and be definedAt (8))
     *                                          ^
     * </pre>
     */
    def definedAt[A, U <: PartialFunction[A, _]](right: A): Matcher[T with U] = and(MatcherWords.be.definedAt(right))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * isFileMock should (be a ('file) and be a ('file))
   *                                 ^
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
     * "1.7" should (fullyMatch regex (decimal) and fullyMatch regex (decimal))
     *                                                         ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = and(MatcherWords.fullyMatch.regex(regexString))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "abbc" should (fullyMatch regex ("a(b*)c" withGroup "bb") and fullyMatch regex ("a(b*)c" withGroup "bb"))
     *                                                                          ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups): Matcher[T with String] = and(MatcherWords.fullyMatch.regex(regexWithGroups))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (fullyMatch regex (decimalRegex) and fullyMatch regex (decimalRegex))
     *                                                              ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = and(MatcherWords.fullyMatch.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "1.7" should (fullyMatch regex (decimalRegex) and fullyMatch regex (decimalRegex))
   *                                               ^
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
     * "1.7" should (include regex (decimal) and include regex (decimal))
     *                                                   ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = and(MatcherWords.include.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "abbc" should (include regex ("a(b*)c" withGroup "bb") and include regex ("a(b*)c" withGroup "bb"))
     *                                                                    ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups): Matcher[T with String] = and(MatcherWords.include.regex(regexWithGroups))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (include regex (decimalRegex) and include regex (decimalRegex))
     *                                                        ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = and(MatcherWords.include.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "hello, world" should (include regex ("hel*o") and include regex ("wor.d"))
   *                                                ^
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
     * "1.7" should (startWith regex (decimal) and startWith regex (decimal))
     *                                                       ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = and(MatcherWords.startWith.regex(regexString))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "abbcdef" should (startWith regex ("a(b*)c" withGroup "bb") and startWith regex ("a(b*)c" withGroup "bb"))
     *                                                                           ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups): Matcher[T with String] = and(MatcherWords.startWith.regex(regexWithGroups))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (startWith regex (decimalRegex) and startWith regex (decimalRegex))
     *                                                            ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = and(MatcherWords.startWith.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "1.78" should (have length (4) and startWith regex ("1.7"))
   *                                ^
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
     * "1.7" should (endWith regex (decimal) and endWith regex (decimal))
     *                                                   ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = and(MatcherWords.endWith.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "abcdeef" should (endWith regex ("a(b*)c" withGroup "bb") and endWith regex ("a(b*)c" withGroup "bb"))
     *                                                                       ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups): Matcher[T with String] = and(MatcherWords.endWith.regex(regexWithGroups))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (endWith regex (decimalRegex) and endWith regex (decimalRegex))
     *                                                        ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = and(MatcherWords.endWith.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "1.7" should (endWith regex (decimalRegex) and endWith regex (decimalRegex))
   *                                            ^
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
     * 1 should (not equal (2) and not equal (3 - 1))
     *                                 ^
     * </pre>
     */
    def equal(any: Any): MatcherFactory1[T, Equality] =
      outerInstance.and(MatcherWords.not.apply(MatcherWords.equal(any)))

    /**
     * This method enables the following syntax, for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * sevenDotOh should (not equal (17.0 +- 0.2) and not equal (17.0 +- 0.2))
     *                                                    ^
     * </pre>
     */
    def equal[U](interval: Interval[U]): Matcher[T with U] = outerInstance.and(MatcherWords.not.equal(interval))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aNullRef should (not equal ("hi") and not equal (null))
     *                                   ^
     * </pre>
     */
    def equal(o: Null): Matcher[T] = {
      outerInstance and {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              left != null,
              FailureMessages("equaledNull"),
              FailureMessages("didNotEqualNull", left),
              FailureMessages("midSentenceEqualedNull"),
              FailureMessages("didNotEqualNull", left)
            )
          }
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 1 should (not be (2) and not be (3 - 1))
     *                              ^
     * </pre>
     */
    def be(any: Any): Matcher[T] =
      outerInstance.and(MatcherWords.not.apply(MatcherWords.be(any)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not have size (5) and not have length (3))
     *                                               ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication): MatcherFactory1[T, Length] =
      outerInstance.and(MatcherWords.not.apply(MatcherWords.have.length(resultOfLengthWordApplication.expectedLength)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not have size (5) and not have size (3))
     *                                               ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): MatcherFactory1[T, Size] =
      outerInstance.and(MatcherWords.not.apply(MatcherWords.have.size(resultOfSizeWordApplication.expectedSize)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * book should (not have (title ("Moby Dick")) and not have (author ("Melville")))
     *                                                     ^
     * </pre>
     */
    def have[U](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*): Matcher[T with U] =
      outerInstance.and(MatcherWords.not.apply(MatcherWords.have(firstPropertyMatcher, propertyMatchers: _*)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 5 should (not be &lt; (2) and not be &lt; (6))
     *                                ^
     * </pre>
     */
    def be[U](resultOfLessThanComparison: ResultOfLessThanComparison[U]): Matcher[T with U] =
      outerInstance.and(MatcherWords.not.be(resultOfLessThanComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should (contain key (7) and not be (null))
     *                                     ^
     * </pre>
     */
    def be(o: Null): Matcher[T with AnyRef] = outerInstance.and(MatcherWords.not.be(o))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 7 should (not be &gt; (8) and not be &gt; (6))
     *                                ^
     * </pre>
     */
    def be[U](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[U]): Matcher[T with U] =
      outerInstance.and(MatcherWords.not.be(resultOfGreaterThanComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 2 should (not be &lt;= (1) and not be &lt;= (2))
     *                                 ^
     * </pre>
     */
    def be[U](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[U]): Matcher[T with U] =
      outerInstance.and(MatcherWords.not.be(resultOfLessThanOrEqualToComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 7 should (not be &gt;= (8) and not be &gt;= (6))
     *                                 ^
     * </pre>
     */
    def be[U](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[U]): Matcher[T with U] =
      outerInstance.and(MatcherWords.not.be(resultOfGreaterThanOrEqualToComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 5 should (not be === (2) and not be === (6))
     *                                  ^
     * </pre>
     */
    def be(tripleEqualsInvocation: TripleEqualsInvocation[_]): Matcher[T] =
      outerInstance.and(MatcherWords.not.be(tripleEqualsInvocation))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * notEmptyMock should (not be ('empty) and not be ('empty))
     *                                              ^
     * </pre>
     */
    def be(symbol: Symbol): Matcher[T with AnyRef] = outerInstance.and(MatcherWords.not.be(symbol))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 2 should (not be (odd) and not be (odd))
     *                                ^
     * </pre>
     */
    def be[U](beMatcher: BeMatcher[U]): Matcher[T with U] = outerInstance.and(MatcherWords.not.be(beMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * myFile should (not be (directory) and not be (directory))
     *                                              ^
     * </pre>
     */
    def be[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = outerInstance.and(MatcherWords.not.be(bePropertyMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * isNotFileMock should (not be a ('file) and not be a ('file))
     *                                                ^
     * </pre>
     */
    def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication): Matcher[T with AnyRef] = outerInstance.and(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not be a (passedMarks) and not be a (validMarks))
     *                                               ^
     * </pre>
     */
    def be[U](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[U]): Matcher[T with U] = outerInstance.and(MatcherWords.not.be(resultOfAWordApplication))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * myFile should (not be a (directory) and not be a (directory))
     *                                             ^
     * </pre>
     */
    def be[U <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]): Matcher[T with U] = outerInstance.and(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not be a negativeNumber and not be a primeNumber)
     *                                                ^
     * </pre>
     */
    def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication): Matcher[T with AnyRef] = outerInstance.and(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * myFile should (not be an (directory) and not be an (directory))
     *                                              ^
     * </pre>
     */
    def be[T <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[T]) = outerInstance.and(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not be an (oddMarks) and not be an (invalidMarks))
     *                                                ^
     * </pre>
     */
    def be[U](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[U]): Matcher[T with U] = outerInstance.and(MatcherWords.not.be(resultOfAnWordApplication))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should (not be theSameInstanceAs (otherString) and not be theSameInstanceAs (otherString))
     *                                                            ^
     * </pre>
     */
    def be(resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): Matcher[T with AnyRef] = outerInstance.and(MatcherWords.not.be(resultOfTheSameInstanceAsApplication))

    /**
     * This method enables the following syntax, for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * sevenDotOh should (not be (17.0 +- 0.2) and not be (17.0 +- 0.2))
     *                                                 ^
     * </pre>
     */
    def be[U](interval: Interval[U]): Matcher[T with U] = outerInstance.and(MatcherWords.not.be(interval))
    
    /**
     * This method enables the following syntax, where <code>fraction</code> is a <code>PartialFunction</code>:
     *
     * <pre class="stHighlight">
     * fraction should (not be definedAt (0) and not be definedAt (8))
     *                                               ^
     * </pre>
     */
    def be[A, U <: PartialFunction[A, _]](resultOfDefinedAt: ResultOfDefinedAt[A]): Matcher[T with U] =
      outerInstance.and(MatcherWords.not.be(resultOfDefinedAt))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not fullyMatch regex ("bob") and not fullyMatch regex (decimal))
     *                                                     ^
     * </pre>
     */
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      outerInstance.and(MatcherWords.not.fullyMatch(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not include regex ("bob") and not include regex (decimal))
     *                                                     ^
     * </pre>
     */
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      outerInstance.and(MatcherWords.not.include(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not include ("bob") and not include ("1.7"))
     *                                            ^
     * </pre>
     */
    def include(expectedSubstring: String): Matcher[T with String] =
      outerInstance.and(MatcherWords.not.include(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not startWith regex ("bob") and not startWith regex (decimal))
     *                                                    ^
     * </pre>
     */
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      outerInstance.and(MatcherWords.not.startWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not startWith ("red") and not startWith ("1.7"))
     *                                              ^
     * </pre>
     */
    def startWith(expectedSubstring: String): Matcher[T with String] =
      outerInstance.and(MatcherWords.not.startWith(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not endWith regex ("bob") and not endWith regex (decimal))
     *                                                  ^
     * </pre>
     */
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      outerInstance.and(MatcherWords.not.endWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not endWith ("fre") and not endWith ("1.7"))
     *                                            ^
     * </pre>
     */
    def endWith(expectedSubstring: String): Matcher[T with String] =
      outerInstance.and(MatcherWords.not.endWith(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not contain (5) and not contain (3))
     *                                                     ^
     * </pre>
     */
    def contain[U](expectedElement: U): MatcherFactory1[T, Containing] =
      outerInstance.and(MatcherWords.not.contain(expectedElement))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (not contain key ("five") and not contain key ("three"))
     *                                                                      ^
     * </pre>
     */
    def contain[U](resultOfKeyWordApplication: ResultOfKeyWordApplication[U]): Matcher[T with scala.collection.GenMap[U, Any]] =
      outerInstance.and(MatcherWords.not.contain(resultOfKeyWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (not contain value (5) and not contain value (3))
     *                                                                   ^
     * </pre>
     */
    def contain[U](resultOfValueWordApplication: ResultOfValueWordApplication[U]): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] =
      outerInstance.and(MatcherWords.not.contain(resultOfValueWordApplication))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) and not contain theSameElementsAs (List(8, 1, 2))) 
     *                                                                      ^
     * </pre>
     */
    def contain[U](right: ContainMatcher[U]): Matcher[T with GenTraversable[U]] =
      outerInstance.and(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) and not contain oneOf (List(8, 1, 2))) 
     *                                                                      ^
     * </pre>
     */
    def newContain(right: ResultOfNewOneOfApplication): MatcherFactory1[T with Any, Containing] =
      outerInstance.and(MatcherWords.not.newContain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) and not contain atLeastOneOf (List(8, 1, 2))) 
     *                                                                      ^
     * </pre>
     */
    def newContain(right: ResultOfAtLeastOneOfApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.and(MatcherWords.not.newContain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) and not contain noneOf (List(8, 1, 2))) 
     *                                                                      ^
     * </pre>
     */
    def newContain(right: ResultOfNewNoneOfApplication): MatcherFactory1[T with Any, Containing] =
      outerInstance.and(MatcherWords.not.newContain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) and not contain theSameElementsAs (List(8, 1, 2))) 
     *                                                                      ^
     * </pre>
     */
    def newContain(right: ResultOfNewTheSameElementsAsApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.and(MatcherWords.not.newContain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) and not contain theSameElementsInOrderAs (List(8, 1, 2))) 
     *                                                                      ^
     * </pre>
     */
    def newContain(right: ResultOfNewTheSameElementsInOrderAsApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.and(MatcherWords.not.newContain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) and not contain only (List(8, 1, 2))) 
     *                                                                      ^
     * </pre>
     */
    def newContain(right: ResultOfNewOnlyApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.and(MatcherWords.not.newContain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher and not contain inOrderOnly (8, 1, 2)
     *                  ^
     * </pre>
     */
    def newContain(right: ResultOfNewInOrderOnlyApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.and(MatcherWords.not.newContain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) and not contain allOf (List(8, 1, 2))) 
     *                                                                      ^
     * </pre>
     */
    def newContain(right: ResultOfNewAllOfApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.and(MatcherWords.not.newContain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) and not contain inOrder (List(8, 1, 2))) 
     *                                                                      ^
     * </pre>
     */
    def newContain(right: ResultOfNewInOrderApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.and(MatcherWords.not.newContain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not contain a negativeNumber and not contain a primeNumber)
     *                                                     ^
     * </pre>
     */
    def contain[U](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[U]): Matcher[T with GenTraversable[U]] = 
      outerInstance.and(MatcherWords.not.contain(resultOfAWordApplication))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not contain an oddNumber and not contain an invalidNumber)
     *                                                 ^
     * </pre>
     */
    def contain[U](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[U]): Matcher[T with GenTraversable[U]] = 
      outerInstance.and(MatcherWords.not.contain(resultOfAnWordApplication))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Map("one" -&gt; 1, "two" -&gt; 2) should (not contain value (5) and not contain value (3))
   *                                                           ^
   * </pre>
   */
  def and(notWord: NotWord): AndNotWord = new AndNotWord

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
     * Array(1, 2) should (have length (2) or have length (3 - 1))
     *                                             ^
     * </pre>
     */
    def length(expectedLength: Long): MatcherFactory1[T, Length] = or(MatcherWords.have.length(expectedLength))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (have size (2) or have size (3 - 1))
     *                                      ^
     * </pre>
     */
    def size(expectedSize: Long): MatcherFactory1[T, Size] = or(MatcherWords.have.size(expectedSize))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (have size (2) or have size (3 - 1))
   *                                   ^
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
     * Array(1, 2) should (contain (2) or contain (3 - 1))
     *                                            ^
     * </pre>
     */
    def apply[U](expectedElement: Any): MatcherFactory1[T with U, Containing] = outerInstance.or(MatcherWords.contain(expectedElement))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (contain key ("cat") or contain key ("one"))
     *                                                                    ^
     * </pre>
     */
    def key[U](expectedKey: U): Matcher[T with scala.collection.GenMap[U, Any]] = outerInstance.or(MatcherWords.contain.key(expectedKey))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (contain value (7) or contain value (1))
     *                                                                  ^
     * </pre>
     */
    def value[U](expectedValue: U): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] = outerInstance.or(MatcherWords.contain.value(expectedValue))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain theSameElementsAs List(1, 2, 3))
     *                                                                          ^
     * </pre>
     */
    def oldTheSameElementsAs[E](right: GenTraversable[E])(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      outerInstance.or(MatcherWords.contain.oldTheSameElementsAs(right)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain theSameElementsInOrderAs List(1, 2, 3))
     *                                                                          ^
     * </pre>
     */
    def oldTheSameElementsInOrderAs[E](right: GenTraversable[E])(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      outerInstance.or(MatcherWords.contain.oldTheSameElementsInOrderAs(right)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain allOf (1, 2, 3))
     *                                                                          ^
     * </pre>
     */
    def oldAllOf[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      outerInstance.or(MatcherWords.contain.oldAllOf(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain inOrder (1, 2, 3))
     *                                                                          ^
     * </pre>
     */
    def oldInOrder[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      outerInstance.or(MatcherWords.contain.oldInOrder(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain oneOf (1, 3, 3))
     *                                                                          ^
     * </pre>
     */
    def oldOneOf[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      outerInstance.or(MatcherWords.contain.oldOneOf(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain only (3, 1))
     *                                                                          ^
     * </pre>
     */
    def oldOnly[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      outerInstance.or(MatcherWords.contain.oldOnly(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain inOrderOnly (1, 3))
     *                                                                          ^
     * </pre>
     */
    def oldInOrderOnly[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      outerInstance.or(MatcherWords.contain.oldInOrderOnly(right.toList: _*)(equality))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain noneOf (7, 8, 9))
     *                                                                          ^
     * </pre>
     */
    def oldNoneOf[E](right: E*)(implicit equality: Equality[E]): Matcher[T with GenTraversable[E]] = 
      outerInstance.or(MatcherWords.contain.oldNoneOf(right.toList: _*)(equality))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (contain a (positiveNumber) or contain a (validNumber))
     *                                                      ^
     * </pre>
     */
    def a[E](aMatcher: AMatcher[E]): Matcher[T with GenTraversable[E]] = 
      or(MatcherWords.contain.a(aMatcher))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (contain a (positiveNumber) or contain an (invalidNumber))
     *                                                      ^
     * </pre>
     */
    def an[E](anMatcher: AnMatcher[E]): Matcher[T with GenTraversable[E]] = 
      or(MatcherWords.contain.an(anMatcher))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Map("one" -&gt; 1, "two" -&gt; 2) should (contain value (7) or contain value (1))
   *                                                       ^
   * </pre>
   */
  def or(containWord: ContainWord): OrContainWord = new OrContainWord
  
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrNewContainWord {

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (contain (2) or contain (3 - 1))
     *                                            ^
     * </pre>
     */
    //def apply[U](expectedElement: Any): MatcherFactory1[T with U, Containing] = outerInstance.or(MatcherWords.contain(expectedElement))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (contain key ("cat") or contain key ("one"))
     *                                                                    ^
     * </pre>
     */
    //def key[U](expectedKey: U): Matcher[T with scala.collection.GenMap[U, Any]] = outerInstance.or(MatcherWords.contain.key(expectedKey))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (contain value (7) or contain value (1))
     *                                                                  ^
     * </pre>
     */
    //def value[U](expectedValue: U): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] = outerInstance.or(MatcherWords.contain.value(expectedValue))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain theSameElementsAs List(1, 2, 3))
     *                                                                          ^
     * </pre>
     */
    def newTheSameElementsAs(right: GenTraversable[_]): MatcherFactory1[T with Any, Aggregating] = 
      outerInstance.or(MatcherWords.newContain.newTheSameElementsAs(right))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain theSameElementsInOrderAs List(1, 2, 3))
     *                                                                          ^
     * </pre>
     */
    def newTheSameElementsInOrderAs(right: GenTraversable[_]): MatcherFactory1[T with Any, Aggregating] = 
      outerInstance.or(MatcherWords.newContain.newTheSameElementsInOrderAs(right))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain allOf (1, 3, 3))
     *                                                                          ^
     * </pre>
     */
    def newAllOf(right: Any*): MatcherFactory1[T with Any, Aggregating] = 
      outerInstance.or(MatcherWords.newContain.newAllOf(right.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain inOrder (1, 3, 3))
     *                                                                          ^
     * </pre>
     */
    def newInOrder(right: Any*): MatcherFactory1[T with Any, Aggregating] = 
      outerInstance.or(MatcherWords.newContain.newInOrder(right.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain oneOf (1, 3, 3))
     *                                                                          ^
     * </pre>
     */
    def newOneOf(right: Any*): MatcherFactory1[T with Any, Containing] = 
      outerInstance.or(MatcherWords.newContain.newOneOf(right.toList: _*))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain oneOf (1, 3, 3))
     *                                                                          ^
     * </pre>
     */
    def atLeastOneOf(right: Any*): MatcherFactory1[T with Any, Aggregating] = 
      outerInstance.or(MatcherWords.newContain.atLeastOneOf(right.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain only (1, 3, 3))
     *                                                                          ^
     * </pre>
     */
    def newOnly(right: Any*): MatcherFactory1[T with Any, Aggregating] = 
      outerInstance.or(MatcherWords.newContain.newOnly(right.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or contain inOrderOnly (1, 2, 3)
     *                     ^
     * </pre>
     */
    def newInOrderOnly(right: Any*): MatcherFactory1[T with Any, Aggregating] = 
      outerInstance.or(MatcherWords.newContain.newInOrderOnly(right.toList: _*))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2, 3) should (contain theSameElementAs List(3, 2, 1) or contain noneOf (1, 3, 3))
     *                                                                          ^
     * </pre>
     */
    def newNoneOf(right: Any*): MatcherFactory1[T with Any, Containing] = 
      outerInstance.or(MatcherWords.newContain.newNoneOf(right.toList: _*))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (contain a (positiveNumber) or contain a (validNumber))
     *                                                      ^
     * </pre>
     */
    //def a[E](aMatcher: AMatcher[E]): Matcher[T with GenTraversable[E]] = 
      //or(MatcherWords.contain.a(aMatcher))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (contain a (positiveNumber) or contain an (invalidNumber))
     *                                                      ^
     * </pre>
     */
    //def an[E](anMatcher: AnMatcher[E]): Matcher[T with GenTraversable[E]] = 
      //or(MatcherWords.contain.an(anMatcher))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Map("one" -&gt; 1, "two" -&gt; 2) should (contain value (7) or contain value (1))
   *                                                       ^
   * </pre>
   */
  def or(containWord: NewContainWord): OrNewContainWord = new OrNewContainWord

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
     * isFileMock should (be a ('file) or be a ('directory))
     *                                       ^
     * </pre>
     */
    def a(symbol: Symbol): Matcher[T with AnyRef] = or(MatcherWords.be.a(symbol))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * isFileMock should (be a (file) or be a (directory))
     *                                      ^
     * </pre>
     */
    def a[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = or(MatcherWords.be.a(bePropertyMatcher))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (be a (positiveNumber) or be a (validNumber))
     *                                            ^
     * </pre>
     */
    def a[U](aMatcher: AMatcher[U]): Matcher[T with U] = or(MatcherWords.be.a(aMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * appleMock should (be an ('orange) or be an ('apple))
     *                                         ^
     * </pre>
     */
    def an(symbol: Symbol): Matcher[T with AnyRef] = or(MatcherWords.be.an(symbol))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * appleMock should (be an (orange) or be an (apple))
     *                                        ^
     * </pre>
     */
    def an[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = or(MatcherWords.be.an(bePropertyMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (be an (oddNumber) or be an (integerNumber))
     *                                        ^
     * </pre>
     */
    def an[U](anMatcher: AnMatcher[U]): Matcher[T with U] = or(MatcherWords.be.an(anMatcher))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should (be theSameInstanceAs (string) or be theSameInstanceAs (otherString))
     *                                                 ^
     * </pre>
     */
    def theSameInstanceAs(anyRef: AnyRef): Matcher[T with AnyRef] = or(MatcherWords.be.theSameInstanceAs(anyRef))
    
    /**
     * This method enables the following syntax, where <code>fraction</code> refers to a <code>PartialFunction</code>:
     *
     * <pre class="stHighlight">
     * fraction should (be definedAt (6) or be definedAt (8))
     *                                         ^
     * </pre>
     */
    def definedAt[A, U <: PartialFunction[A, _]](right: A): Matcher[T with U] = or(MatcherWords.be.definedAt(right))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * isFileMock should (be a ('file) or be a ('directory))
   *                                 ^
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
     * "1.7" should (fullyMatch regex ("hello") or fullyMatch regex (decimal))
     *                                                        ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = or(MatcherWords.fullyMatch.regex(regexString))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "abbc" should (fullyMatch regex ("a(b*)c" withGroup "bbb") or fullyMatch regex ("a(b*)c" withGroup "bb"))
     *                                                                          ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups): Matcher[T with String] = or(MatcherWords.fullyMatch.regex(regexWithGroups))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (fullyMatch regex ("hello") or fullyMatch regex (decimal))
     *                                                        ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = or(MatcherWords.fullyMatch.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "1.7" should (fullyMatch regex ("hello") or fullyMatch regex (decimal))
   *                                          ^
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
     * "1.7" should (include regex ("hello") or include regex (decimal))
     *                                                  ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = or(MatcherWords.include.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "abbc" should (include regex ("a(b*)c" withGroup "bbb") or include regex ("a(b*)c" withGroup "bb"))
     *                                                                    ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups): Matcher[T with String] = or(MatcherWords.include.regex(regexWithGroups))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (include regex ("hello") or include regex (decimal))
     *                                                  ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = or(MatcherWords.include.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "a1.7b" should (include regex ("1.7") or include regex ("1.7"))
   *                                          ^
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
     * "1.7" should (startWith regex ("hello") or startWith regex (decimal))
     *                                                      ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = or(MatcherWords.startWith.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "abbcdef" should (startWith regex ("a(b*)c" withGroup "bb") or startWith regex ("a(b*)c" withGroup "bb"))
     *                                                                          ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups): Matcher[T with String] = or(MatcherWords.startWith.regex(regexWithGroups))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (startWith regex ("hello") or startWith regex (decimal))
     *                                                      ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = or(MatcherWords.startWith.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "1.7" should (startWith regex ("hello") or startWith regex ("1.7"))
   *                                            ^
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
     * "1.7" should (endWith regex ("hello") or endWith regex (decimal))
     *                                                  ^
     * </pre>
     */
    def regex(regexString: String): Matcher[T with String] = or(MatcherWords.endWith.regex(regexString))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "abcdeef" should (endWith regex ("d(e*)f" withGroup "ee") or endWith regex ("d(e*)f" withGroup "ee"))
     *                                                                      ^
     * </pre>
     */
    def regex(regexWithGroups: RegexWithGroups): Matcher[T with String] = or(MatcherWords.endWith.regex(regexWithGroups))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "1.7" should (endWith regex ("hello") or endWith regex (decimal))
     *                                                  ^
     * </pre>
     */
    def regex(regex: Regex): Matcher[T with String] = or(MatcherWords.endWith.regex(regex))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "1.7b" should (endWith regex ("hello") or endWith regex ("7b"))
   *                                        ^
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
     * 1 should (not equal (1) or not equal (2))
     *                                ^
     * </pre>
     */
    def equal(any: Any): Matcher[T] =
      outerInstance.or(MatcherWords.not.apply(MatcherWords.legacyEqual(any)))

    /**
     * This method enables the following syntax for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * sevenDotOh should (not equal (17.0 +- 0.2) or not equal (17.0 +- 0.2))
     *                                                   ^
     * </pre>
     */
    def equal[U](interval: Interval[U]): Matcher[T with U] = outerInstance.or(MatcherWords.not.equal(interval))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aNullRef should (not equal (null) or not equal (null))
     *                                          ^
     * </pre>
     */
    def equal(o: Null): Matcher[T] = {
      outerInstance or {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              left != null,
              FailureMessages("equaledNull"),
              FailureMessages("didNotEqualNull", left),
              FailureMessages("midSentenceEqualedNull"),
              FailureMessages("didNotEqualNull", left)
            )
          }
        }
      }
    }

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 1 should (not be (1) or not be (2))
     *                             ^
     * </pre>
     */
    def be(any: Any): Matcher[T] =
      outerInstance.or(MatcherWords.not.apply(MatcherWords.be(any)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not have length (2) or not have length (3))
     *                                                ^
     * </pre>
     */
    def have(resultOfLengthWordApplication: ResultOfLengthWordApplication): MatcherFactory1[T, Length] =
      outerInstance.or(MatcherWords.not.apply(MatcherWords.have.length(resultOfLengthWordApplication.expectedLength)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not have size (2) or not have size (3))
     *                                              ^
     * </pre>
     */
    def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): MatcherFactory1[T, Size] =
      outerInstance.or(MatcherWords.not.apply(MatcherWords.have.size(resultOfSizeWordApplication.expectedSize)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * book should (not have (title ("Moby Dick")) or not have (author ("Melville")))
     *                                                    ^
     * </pre>
     */
    def have[U](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*): Matcher[T with U] =
      outerInstance.or(MatcherWords.not.apply(MatcherWords.have(firstPropertyMatcher, propertyMatchers: _*)))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * map should (contain key (7) or not be (null))
     *                                    ^
     * </pre>
     */
    def be(o: Null): Matcher[T with AnyRef] = outerInstance.or(MatcherWords.not.be(o))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 5 should (not be &lt; (7) or not be &lt; (8))
     *                               ^
     * </pre>
     */
    def be[U](resultOfLessThanComparison: ResultOfLessThanComparison[U]): Matcher[T with U] =
      outerInstance.or(MatcherWords.not.be(resultOfLessThanComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 7 should (not be &gt; (5) or not be &gt; (6))
     *                               ^
     * </pre>
     */
    def be[U](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[U]): Matcher[T with U] =
      outerInstance.or(MatcherWords.not.be(resultOfGreaterThanComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 2 should (not be &lt;= (3) or not be &lt;= (2))
     *                                ^
     * </pre>
     */
    def be[U](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[U]): Matcher[T with U] =
      outerInstance.or(MatcherWords.not.be(resultOfLessThanOrEqualToComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 8 should (not be &gt;= (7) or not be &gt;= (6))
     *                                ^
     * </pre>
     */
    def be[U](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[U]): Matcher[T with U] =
      outerInstance.or(MatcherWords.not.be(resultOfGreaterThanOrEqualToComparison))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 5 should (not be === (7) or not be === (8))
     *                                 ^
     * </pre>
     */
    def be(tripleEqualsInvocation: TripleEqualsInvocation[_]): Matcher[T] =
      outerInstance.or(MatcherWords.not.be(tripleEqualsInvocation))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * notEmptyMock should (not be ('full) or not be ('empty))
     *                                            ^
     * </pre>
     */
    def be(symbol: Symbol): Matcher[T with AnyRef] = outerInstance.or(MatcherWords.not.be(symbol))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * 2 should (not be (even) or not be (odd))
     *                                ^
     * </pre>
     */
    def be[U](beMatcher: BeMatcher[U]): Matcher[T with U] = outerInstance.or(MatcherWords.not.be(beMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * myFile should (not be (directory) or not be (file))
     *                                          ^
     * </pre>
     */
    def be[U](bePropertyMatcher: BePropertyMatcher[U]): Matcher[T with AnyRef with U] = outerInstance.or(MatcherWords.not.be(bePropertyMatcher))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * isNotFileMock should (not be a ('directory) or not be a ('file))
     *                                                    ^
     * </pre>
     */
    def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication): Matcher[T with AnyRef] = outerInstance.or(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not be a (passedMarks) or not be a (validMarks))
     *                                                 ^
     * </pre>
     */
    def be[U](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[U]): Matcher[T with U] = outerInstance.or(MatcherWords.not.be(resultOfAWordApplication))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * myFile should (not be a (directory) or not be a (file))
     *                                            ^
     * </pre>
     */
    def be[U <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]): Matcher[T with U] = outerInstance.or(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * notAppleMock should (not be an ('apple) or not be an ('apple))
     *                                                ^
     * </pre>
     */
    def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication): Matcher[T with AnyRef] = outerInstance.or(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * myFile should (not be an (directory) or not be an (file))
     *                                             ^
     * </pre>
     */
    def be[U <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U]): Matcher[T with U] = outerInstance.or(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not be an (oddMarks) and not be an (invalidMarks))
     *                                                ^
     * </pre>
     */
    def be[U](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[U]): Matcher[T with U] = outerInstance.or(MatcherWords.not.be(resultOfAnWordApplication))
    
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * obj should (not be theSameInstanceAs (otherString) or not be theSameInstanceAs (string))
     *                                                           ^
     * </pre>
     */
    def be(resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): Matcher[T with AnyRef] = outerInstance.or(MatcherWords.not.be(resultOfTheSameInstanceAsApplication))

    /**
     * This method enables the following syntax for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * sevenDotOh should (not be (17.0 +- 0.2) or not be (17.0 +- 0.2))
     *                                                ^
     * </pre>
     */
    def be[U](interval: Interval[U]): Matcher[T with U] = outerInstance.or(MatcherWords.not.be(interval))
    
    /**
     * This method enables the following syntax, where <code>fraction</code> is a <code>PartialFunction</code>:
     *
     * <pre class="stHighlight">
     * fraction should (not be definedAt (0) or not be definedAt (8))
     *                                              ^
     * </pre>
     */
    def be[A, U <: PartialFunction[A, _]](resultOfDefinedAt: ResultOfDefinedAt[A]): Matcher[T with U] =
      outerInstance.or(MatcherWords.not.be(resultOfDefinedAt))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not fullyMatch regex ("fred") or not fullyMatch regex (decimal))
     *                                                     ^
     * </pre>
     */
    def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      outerInstance.or(MatcherWords.not.fullyMatch(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not include regex ("fred") or not include regex (decimal))
     *                                                  ^
     * </pre>
     */
    def include(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      outerInstance.or(MatcherWords.not.include(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not include ("bob") or not include ("1.7"))
     *                                           ^
     * </pre>
     */
    def include(expectedSubstring: String): Matcher[T with String] =
      outerInstance.or(MatcherWords.not.include(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not startWith regex ("bob") or not startWith regex (decimal))
     *                                                   ^
     * </pre>
     */
    def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      outerInstance.or(MatcherWords.not.startWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not startWith ("fred") or not startWith ("1.7"))
     *                                              ^
     * </pre>
     */
    def startWith(expectedSubstring: String): Matcher[T with String] =
      outerInstance.or(MatcherWords.not.startWith(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not endWith regex ("bob") or not endWith regex (decimal))
     *                                                 ^
     * </pre>
     */
    def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): Matcher[T with String] =
      outerInstance.or(MatcherWords.not.endWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * "fred" should (not endWith ("fred") or not endWith ("1.7"))
     *                                            ^
     * </pre>
     */
    def endWith(expectedSubstring: String): Matcher[T with String] =
      outerInstance.or(MatcherWords.not.endWith(expectedSubstring))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Array(1, 2) should (not contain (1) or not contain (3))
     *                                            ^
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
    def contain[U](resultOfKeyWordApplication: ResultOfKeyWordApplication[U]): Matcher[T with scala.collection.GenMap[U, Any]] =
      outerInstance.or(MatcherWords.not.contain(resultOfKeyWordApplication))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * Map("one" -&gt; 1, "two" -&gt; 2) should (not contain value (2) or not contain value (3))
     *                                                                  ^
     * </pre>
     */
    def contain[U](resultOfValueWordApplication: ResultOfValueWordApplication[U]): Matcher[T with scala.collection.GenMap[K, U] forSome { type K }] =
      outerInstance.or(MatcherWords.not.contain(resultOfValueWordApplication))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) or not contain theSameElementsAs (List(8, 1, 2))) 
     *                                                                     ^
     * </pre>
     */
    def contain[U](right: ContainMatcher[U]): Matcher[T with GenTraversable[U]] =
      outerInstance.or(MatcherWords.not.contain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) or not contain oneOf (List(8, 1, 2))) 
     *                                                                     ^
     * </pre>
     */
    def newContain(right: ResultOfNewOneOfApplication): MatcherFactory1[T with Any, Containing] =
      outerInstance.or(MatcherWords.not.newContain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) or not contain atLeastOneOf (List(8, 1, 2))) 
     *                                                                     ^
     * </pre>
     */
    def newContain(right: ResultOfAtLeastOneOfApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.or(MatcherWords.not.newContain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) or not contain noneOf (List(8, 1, 2))) 
     *                                                                     ^
     * </pre>
     */
    def newContain(right: ResultOfNewNoneOfApplication): MatcherFactory1[T with Any, Containing] =
      outerInstance.or(MatcherWords.not.newContain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) or not contain theSameElementsAs (List(8, 1, 2))) 
     *                                                                     ^
     * </pre>
     */
    def newContain(right: ResultOfNewTheSameElementsAsApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.or(MatcherWords.not.newContain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) or not contain theSameElementsInOrderAs (List(8, 1, 2))) 
     *                                                                     ^
     * </pre>
     */
    def newContain(right: ResultOfNewTheSameElementsInOrderAsApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.or(MatcherWords.not.newContain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcher or not contain only (List(8, 1, 2))) 
     *                 ^
     * </pre>
     */
    def newContain(right: ResultOfNewInOrderOnlyApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.or(MatcherWords.not.newContain(right))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) or not contain only (List(8, 1, 2))) 
     *                                                                     ^
     * </pre>
     */
    def newContain(right: ResultOfNewOnlyApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.or(MatcherWords.not.newContain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) or not contain allOf (List(8, 1, 2))) 
     *                                                                     ^
     * </pre>
     */
    def newContain(right: ResultOfNewAllOfApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.or(MatcherWords.not.newContain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * collection should (contain theSameElementsAs (List(1, 2, 3)) or not contain inOrder (List(8, 1, 2))) 
     *                                                                     ^
     * </pre>
     */
    def newContain(right: ResultOfNewInOrderApplication): MatcherFactory1[T with Any, Aggregating] =
      outerInstance.or(MatcherWords.not.newContain(right))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not contain a negativeNumber or not contain a primeNumber)
     *                                                    ^
     * </pre>
     */
    def contain[U](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[U]): Matcher[T with GenTraversable[U]] = 
      outerInstance.or(MatcherWords.not.contain(resultOfAWordApplication))
      
    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * result should (not contain an oddNumber or not contain an invalidNumber)
     *                                                ^
     * </pre>
     */
    def contain[U](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[U]): Matcher[T with GenTraversable[U]] = 
      outerInstance.or(MatcherWords.not.contain(resultOfAnWordApplication))
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Map("one" -&gt; 1, "two" -&gt; 2) should (not contain value (2) or not contain value (3))
   *                                                           ^
   * </pre>
   */
  def or(notWord: NotWord): OrNotWord = new OrNotWord
}

/**
 * Companion object for trait <code>Matcher</code> that provides a
 * factory method that creates a <code>Matcher[T]</code> from a
 * passed function of type <code>(T => MatchResult)</code>.
 *
 * @author Bill Venners
 */
object Matcher {

  /**
   * Factory method that creates a <code>Matcher[T]</code> from a
   * passed function of type <code>(T => MatchResult)</code>.
   *
   * @author Bill Venners
   */
  def apply[T](fun: T => MatchResult): Matcher[T] =
    new Matcher[T] {
      def apply(left: T) = fun(left)
    }
}

