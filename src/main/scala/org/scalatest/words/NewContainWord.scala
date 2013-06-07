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

import scala.collection.GenTraversable
import org.scalatest.enablers.Containing
import org.scalatest.enablers.Aggregating
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatcherFactory1
import org.scalatest.matchers.MatchResult
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString
import org.scalatest.matchers.ContainMatcher
import org.scalatest.matchers.AMatcher
import org.scalatest.matchers.AnMatcher

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class NewContainWord {

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (contain (2) and contain (1))
   *                             ^
   * </pre>
   */
  def apply(expectedElement: Any): MatcherFactory1[Any, Containing] =
    new MatcherFactory1[Any, Containing] {
      def matcher[U <: Any : Containing]: Matcher[U] = 
        new Matcher[U] {
          def apply(left: U): MatchResult = {
            val holder = implicitly[Containing[U]]
            MatchResult(
              holder.contains(left, expectedElement),
              FailureMessages("didNotContainExpectedElement", left, expectedElement),
              FailureMessages("containedExpectedElement", left, expectedElement)
            )
          }
        }
    }
  
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
   * Array(1, 2, 3) should (contain oneOf (1, 3, 5) and contain theSameElementsAs List(1, 2, 3))
   *                                ^
   * </pre>
   */
  def oneOf(right: Any*): MatcherFactory1[Any, Containing] = {
    new MatcherFactory1[Any, Containing] {
      def matcher[T](implicit containing: Containing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              containing.containsOneOf(left, right),
              FailureMessages("didNotContainOneOfElements", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              FailureMessages("containedOneOfElements", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
            )
          }
        }
      }
    }
  }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2, 3) should (contain atLeastOneOf (1, 3, 5) and contain theSameElementsAs List(1, 2, 3))
   *                                ^
   * </pre>
   */
  def atLeastOneOf(right: Any*): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              aggregating.containsAtLeastOneOf(left, right),
              FailureMessages("didNotContainAtLeastOneOf", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              FailureMessages("containedAtLeastOneOf", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
            )
          }
        }
      }
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
  def noneOf(right: Any*): MatcherFactory1[Any, Containing] = {
    new MatcherFactory1[Any, Containing] {
      def matcher[T](implicit containing: Containing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              containing.containsNoneOf(left, right),
              FailureMessages("containedOneOfElements", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              FailureMessages("didNotContainOneOfElements", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
            )
          }
        }
      }
    }
  }
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2, 3) should (contain theSameElementsAs List(1, 2, 3) and contain theSameElementsInOrderAs List(3, 2, 1))
   *                                ^
   * </pre>
   */
  def theSameElementsAs(right: GenTraversable[Any]): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              aggregating.containsTheSameElementsAs(left, right),
              FailureMessages("didNotContainSameElements", left, right),
              FailureMessages("containedSameElements", left, right)
            )
          }
        }
      }
    }
  }
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2, 3) should (contain theSameElementsInOrderAs List(1, 2, 3) and contain theSameElementsAs List(1, 2, 3))
   *                                ^
   * </pre>
   */
  def theSameElementsInOrderAs(right: GenTraversable[Any]): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              aggregating.containsTheSameElementsInOrderAs(left, right),
              FailureMessages("didNotContainSameElementsInOrder", left, right),
              FailureMessages("containedSameElementsInOrder", left, right)
            )
          }
        }
      }
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
  def only(right: Any*): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              aggregating.containsOnly(left, right),
              FailureMessages("didNotContainOnlyElements", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              FailureMessages("containedOnlyElements", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
            )
          }
        }
      }
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
  def inOrderOnly(right: Any*): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              aggregating.containsInOrderOnly(left, right),
              FailureMessages("didNotContainInOrderOnlyElements", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              FailureMessages("containedInOrderOnlyElements", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
            )
          }
        }
      }
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
  def allOf(right: Any*): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              aggregating.containsAllOf(left, right),
              FailureMessages("didNotContainAllOfElements", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              FailureMessages("containedAllOfElements", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
            )
          }
        }
      }
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
  def inOrder(right: Any*): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              aggregating.containsInOrder(left, right),
              FailureMessages("didNotContainAllOfElementsInOrder", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              FailureMessages("containedAllOfElementsInOrder", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
            )
          }
        }
      }
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

