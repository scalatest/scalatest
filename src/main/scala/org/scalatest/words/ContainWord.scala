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
import org.scalatest.enablers.Containing
import org.scalatest.enablers.Aggregating
import org.scalatest.enablers.Sequencing
import org.scalatest.enablers.KeyMapping
import org.scalatest.enablers.ValueMapping

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
  def apply(expectedElement: Any): MatcherFactory1[Any, Containing] =
    new MatcherFactory1[Any, Containing] {
      def matcher[U <: Any : Containing]: Matcher[U] = 
        new Matcher[U] {
          def apply(left: U): MatchResult = {
            val containing = implicitly[Containing[U]]
            MatchResult(
              containing.contains(left, expectedElement),
              FailureMessages("didNotContainExpectedElement", left, expectedElement),
              FailureMessages("containedExpectedElement", left, expectedElement)
            )
          }
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
  def oldKey[K](expectedKey: K): Matcher[scala.collection.GenMap[K, Any]] =
    new Matcher[scala.collection.GenMap[K, Any]] {
      def apply(left: scala.collection.GenMap[K, Any]): MatchResult =
        MatchResult(
          left.exists(_._1 == expectedKey),
          FailureMessages("didNotContainKey", left, expectedKey),
          FailureMessages("containedKey", left, expectedKey)
        )
    }
  def key[K](expectedKey: Any): MatcherFactory1[Any, KeyMapping] =
    new MatcherFactory1[Any, KeyMapping] {
      def matcher[U <: Any : KeyMapping]: Matcher[U] = 
        new Matcher[U] {
          def apply(left: U): MatchResult = {
            val keyMapping = implicitly[KeyMapping[U]]
            MatchResult(
              keyMapping.containsKey(left, expectedKey),
              FailureMessages("didNotContainKey", left, expectedKey),
              FailureMessages("containedKey", left, expectedKey)
            )
          }
        }
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
   * Map("one" -&gt; 1, "two" -&gt; 2) should (not contain value (5) and not contain value (3))
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
  def oldValue[V](expectedValue: V): Matcher[scala.collection.GenMap[K, V] forSome { type K }] =
    new Matcher[scala.collection.GenMap[K, V] forSome { type K }] {
      def apply(left: scala.collection.GenMap[K, V] forSome { type K }): MatchResult =
        MatchResult(
          // left.values.contains(expectedValue), CHANGING FOR 2.8.0 RC1
          left.exists(expectedValue == _._2),
          FailureMessages("didNotContainValue", left, expectedValue),
          FailureMessages("containedValue", left, expectedValue)
        )
    }
  def value[K](expectedValue: Any): MatcherFactory1[Any, ValueMapping] =
    new MatcherFactory1[Any, ValueMapping] {
      def matcher[U <: Any : ValueMapping]: Matcher[U] = 
        new Matcher[U] {
          def apply(left: U): MatchResult = {
            val valueMapping = implicitly[ValueMapping[U]]
            MatchResult(
              valueMapping.containsValue(left, expectedValue),
              FailureMessages("didNotContainValue", left, expectedValue),
              FailureMessages("containedValue", left, expectedValue)
            )
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
  
  def theSameElementsInOrderAs(right: GenTraversable[Any]): MatcherFactory1[Any, Sequencing] = {
    new MatcherFactory1[Any, Sequencing] {
      def matcher[T](implicit sequencing: Sequencing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              sequencing.containsTheSameElementsInOrderAs(left, right),
              FailureMessages("didNotContainSameElementsInOrder", left, right),
              FailureMessages("containedSameElementsInOrder", left, right)
            )
          }
        }
      }
    }
  }
  
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

  def inOrderOnly(right: Any*): MatcherFactory1[Any, Sequencing] = {
    new MatcherFactory1[Any, Sequencing] {
      def matcher[T](implicit sequencing: Sequencing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              sequencing.containsInOrderOnly(left, right),
              FailureMessages("didNotContainInOrderOnlyElements", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              FailureMessages("containedInOrderOnlyElements", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
            )
          }
        }
      }
    }
  }
  
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
  
  def inOrder(right: Any*): MatcherFactory1[Any, Sequencing] = {
    new MatcherFactory1[Any, Sequencing] {
      def matcher[T](implicit sequencing: Sequencing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              sequencing.containsInOrder(left, right),
              FailureMessages("didNotContainAllOfElementsInOrder", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              FailureMessages("containedAllOfElementsInOrder", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
            )
          }
        }
      }
    }
  }
}
