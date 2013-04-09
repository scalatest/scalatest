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
package org.scalatest.words

import org.scalatest.matchers._
import scala.collection.GenTraversable
import org.scalautils._
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString
import org.scalautils.Equality
import org.scalatest.enablers.Holder

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class ContainWord {

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (contain (2) and contain (1))
   *                             ^
   * </pre>
   */
  def apply(expectedElement: Any): MatcherFactory1[Any, Holder] =
    new MatcherFactory1[Any, Holder] {
      def matcher[U <: Any : Holder]: Matcher[U] = 
        new Matcher[U] {
          def apply(left: U): MatchResult = {
            val holder = implicitly[Holder[U]]
            MatchResult(
              holder.containsElement(left, expectedElement),
              FailureMessages("didNotContainExpectedElement", left, expectedElement),
              FailureMessages("containedExpectedElement", left, expectedElement)
            )
          }
        }
    }
/*
  def apply[T](expectedElement: T): Matcher[GenTraversable[T]] =
    new Matcher[GenTraversable[T]] {
      def apply(left: GenTraversable[T]): MatchResult =
        MatchResult(
          left.exists(_ == expectedElement), 
          FailureMessages("didNotContainExpectedElement", left, expectedElement),
          FailureMessages("containedExpectedElement", left, expectedElement)
        )
    }
*/
  
  /**
   * This method enables the following syntax, where <code>num</code> is, for example, of type <code>Int</code> and
   * <code>odd</code> refers to a <code>ContainMatcher[Int]</code>:
   *
   * <pre class="stHighlight">
   * num should contain (odd)
   *            ^
   * </pre>
   */
  def apply[T](right: ContainMatcher[T]): Matcher[GenTraversable[T]] = 
    new Matcher[GenTraversable[T]] {
      def apply(left: GenTraversable[T]): MatchResult = {
        val result = right(left)
        MatchResult(result.matches, result.failureMessage, result.negatedFailureMessage)
      }
    }
  
  //
  // This key method is called when "contain" is used in a logical expression, such as:
  // map should { contain key 1 and equal (Map(1 -> "Howdy")) }. It results in a matcher
  // that remembers the key value. By making the value type Any, it causes overloaded shoulds
  // to work, because for example a Matcher[GenMap[Int, Any]] is a subtype of Matcher[GenMap[Int, String]],
  // given Map is covariant in its V (the value type stored in the map) parameter and Matcher is
  // contravariant in its lone type parameter. Thus, the type of the Matcher resulting from contain key 1
  // is a subtype of the map type that has a known value type parameter because its that of the map
  // to the left of should. This means the should method that takes a map will be selected by Scala's
  // method overloading rules.
  //
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * map should (contain key ("fifty five") or contain key ("twenty two"))
   *                     ^
   * </pre>
   *
   * The map's value type parameter cannot be inferred because only a key type is provided in
   * an expression like <code>(contain key ("fifty five"))</code>. The matcher returned
   * by this method matches <code>scala.collection.Map</code>s with the inferred key type and value type <code>Any</code>. Given
   * <code>Map</code> is covariant in its value type, and <code>Matcher</code> is contravariant in
   * its type parameter, a <code>Matcher[Map[Int, Any]]</code>, for example, is a subtype of <code>Matcher[Map[Int, String]]</code>.
   * This will enable the matcher returned by this method to be used against any <code>Map</code> that has
   * the inferred key type.
   */
  def key[K](expectedKey: K): Matcher[scala.collection.GenMap[K, Any]] =
    new Matcher[scala.collection.GenMap[K, Any]] {
      def apply(left: scala.collection.GenMap[K, Any]): MatchResult =
        MatchResult(
          left.exists(_._1 == expectedKey),
          FailureMessages("didNotContainKey", left, expectedKey),
          FailureMessages("containedKey", left, expectedKey)
        )
    }

  // Holy smokes I'm starting to scare myself. I fixed the problem of the compiler not being
  // able to infer the value type in contain value 1 and ... like expressions, because the
  // value type is there, with an existential type. Since I don't know what K is, I decided to
  // try just saying that with an existential type, and it compiled and ran. Pretty darned
  // amazing compiler. The problem could not be fixed like I fixed the key method above, because
  // Maps are nonvariant in their key type parameter, whereas they are covariant in their value
  // type parameter, so the same trick wouldn't work. But this existential type trick seems to
  // work like a charm.
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Map("one" -> 1, "two" -> 2) should (not contain value (5) and not contain value (3))
   *                                                 ^
   * </pre>
   *
   * The map's key type parameter cannot be inferred because only a value type is provided in
   * an expression like <code>(contain value (5))</code>. The matcher returned
   * by this method matches <code>scala.collection.Map</code>s with the inferred value type and the existential key
   * type <code>[K] forSome { type K }</code>. Even though <code>Matcher</code> is contravariant in its type parameter, because
   * <code>Map</code> is nonvariant in its key type, 
   * a <code>Matcher[Map[Any, Int]]</code>, for example, is <em>not</em> a subtype of <code>Matcher[Map[String, Int]]</code>,
   * so the key type parameter of the <code>Map</code> returned by this method cannot be <code>Any</code>. By making it
   * an existential type, the Scala compiler will not infer it to anything more specific.
   * This will enable the matcher returned by this method to be used against any <code>Map</code> that has
   * the inferred value type.
   *
   */
  def value[V](expectedValue: V): Matcher[scala.collection.GenMap[K, V] forSome { type K }] =
    new Matcher[scala.collection.GenMap[K, V] forSome { type K }] {
      def apply(left: scala.collection.GenMap[K, V] forSome { type K }): MatchResult =
        MatchResult(
          // left.values.contains(expectedValue), CHANGING FOR 2.8.0 RC1
          left.exists(expectedValue == _._2),
          FailureMessages("didNotContainValue", left, expectedValue),
          FailureMessages("containedValue", left, expectedValue)
        )
    }
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2, 3) should (contain theSameElementsAs List(1, 2, 3) and contain theSameIteratedElementsAs List(3, 2, 1))
   *                                ^
   * </pre>
   */
  def theSameElementsAs[E](right: GenTraversable[E])(implicit equality: Equality[E]): Matcher[GenTraversable[E]] = 
    new Matcher[GenTraversable[E]] {
      def apply(left: GenTraversable[E]): MatchResult = {
        val result = new TheSameElementsAsContainMatcher(right, equality).apply(left)
        MatchResult(result.matches, result.failureMessage, result.negatedFailureMessage)
      }
    }
    
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2, 3) should (contain theSameIteratedElementsAs List(1, 2, 3) and contain theSameElementsAs List(1, 2, 3))
   *                                ^
   * </pre>
   */
  def theSameIteratedElementsAs[E](right: GenTraversable[E])(implicit equality: Equality[E]): Matcher[GenTraversable[E]] = 
    new Matcher[GenTraversable[E]] {
      def apply(left: GenTraversable[E]): MatchResult = {
        val result = new TheSameIteratedElementsAsContainMatcher(right, equality).apply(left)
        MatchResult(result.matches, result.failureMessage, result.negatedFailureMessage)
      }
    }
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2, 3) should (contain allOf (3, 2, 1) and contain theSameElementsAs List(1, 2, 3))
   *                                ^
   * </pre>
   */
  def allOf[E](right: E*)(implicit equality: Equality[E]): Matcher[GenTraversable[E]] = 
    new Matcher[GenTraversable[E]] {
      def apply(left: GenTraversable[E]): MatchResult = {
        val result = new AllOfContainMatcher(right, equality).apply(left)
        MatchResult(result.matches, result.failureMessage, result.negatedFailureMessage)
      }
    }
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2, 3) should (contain inOrder (1, 2, 3) and contain theSameElementsAs List(1, 2, 3))
   *                                ^
   * </pre>
   */
  def inOrder[E](right: E*)(implicit equality: Equality[E]): Matcher[GenTraversable[E]] = 
    new Matcher[GenTraversable[E]] {
      def apply(left: GenTraversable[E]): MatchResult = {
        val result = new InOrderContainMatcher(right, equality).apply(left)
        MatchResult(result.matches, result.failureMessage, result.negatedFailureMessage)
      }
    }
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2, 3) should (contain oneOf (1, 3, 5) and contain theSameElementsAs List(1, 2, 3))
   *                                ^
   * </pre>
   */
  def oneOf[E](right: E*)(implicit equality: Equality[E]): Matcher[GenTraversable[E]] = 
    new Matcher[GenTraversable[E]] {
      def apply(left: GenTraversable[E]): MatchResult = {
        val result = new OneOfContainMatcher(right, equality).apply(left)
        MatchResult(result.matches, result.failureMessage, result.negatedFailureMessage)
      }
    }
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2, 3) should (contain only (3, 1, 2) and contain theSameElementsAs List(1, 2, 3))
   *                                ^
   * </pre>
   */
  def only[E](right: E*)(implicit equality: Equality[E]): Matcher[GenTraversable[E]] = 
    new Matcher[GenTraversable[E]] {
      def apply(left: GenTraversable[E]): MatchResult = {
        val result = new OnlyContainMatcher(right, equality).apply(left)
        MatchResult(result.matches, result.failureMessage, result.negatedFailureMessage)
      }
    }
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2, 3) should (contain inOrderOnly (1, 2, 3) and contain theSameElementsAs List(1, 2, 3))
   *                                ^
   * </pre>
   */
  def inOrderOnly[E](right: E*)(implicit equality: Equality[E]): Matcher[GenTraversable[E]] = 
    new Matcher[GenTraversable[E]] {
      def apply(left: GenTraversable[E]): MatchResult = {
        val result = new InOrderOnlyContainMatcher(right, equality).apply(left)
        MatchResult(result.matches, result.failureMessage, result.negatedFailureMessage)
      }
    }
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2, 3) should (contain noneOf (7, 8, 9) and contain theSameElementsAs List(1, 2, 3))
   *                                ^
   * </pre>
   */
  def noneOf[E](right: E*)(implicit equality: Equality[E]): Matcher[GenTraversable[E]] = 
    new Matcher[GenTraversable[E]] {
      def apply(left: GenTraversable[E]): MatchResult = {
        val result = new NoneOfContainMatcher(right, equality).apply(left)
        MatchResult(result.matches, result.failureMessage, result.negatedFailureMessage)
      }
    }
  
  /**
   * This method enables the following syntax, where <code>positiveNumber</code> and <code>validNumber</code> are, for example, of type <code>AMatcher</code>:
   *
   * <pre class="stHighlight">
   * Array(1, 2, 3) should (contain a positiveNumber and contain a validNumber)
   *                                ^
   * </pre>
   */
  def a[T](aMatcher: AMatcher[T]): Matcher[GenTraversable[T]] = 
    new Matcher[GenTraversable[T]] {
      def apply(left: GenTraversable[T]): MatchResult = {
        val matched = left.find(aMatcher(_).matches)
        MatchResult(
          matched.isDefined, 
          FailureMessages("didNotContainA", left, UnquotedString(aMatcher.nounName)),
          FailureMessages("containedA", left, UnquotedString(aMatcher.nounName), UnquotedString(if (matched.isDefined) aMatcher(matched.get).negatedFailureMessage else "-"))
        )
      }
    }
  
  /**
   * This method enables the following syntax, where <code>oddNumber</code> and <code>invalidNumber</code> are, for example, of type <code>AnMatcher</code>:
   *
   * <pre class="stHighlight">
   * Array(1, 2, 3) should (contain an oddNumber and contain an invalidNumber)
   *                                ^
   * </pre>
   */
  def an[T](anMatcher: AnMatcher[T]): Matcher[GenTraversable[T]] = 
    new Matcher[GenTraversable[T]] {
      def apply(left: GenTraversable[T]): MatchResult = {
        val matched = left.find(anMatcher(_).matches)
        MatchResult(
          matched.isDefined, 
          FailureMessages("didNotContainAn", left, UnquotedString(anMatcher.nounName)),
          FailureMessages("containedAn", left, UnquotedString(anMatcher.nounName), UnquotedString(if (matched.isDefined) anMatcher(matched.get).negatedFailureMessage else "-"))
        )
      }
    }
}
