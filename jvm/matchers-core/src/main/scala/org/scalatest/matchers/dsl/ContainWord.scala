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
package org.scalatest.matchers.dsl

import org.scalatest.matchers._
import org.scalactic.ColCompatHelper.Iterable
import org.scalactic._
import org.scalatest.FailureMessages
import org.scalatest.Resources
import org.scalatest.UnquotedString
import org.scalactic.{Equality, Every}
import org.scalatest.enablers.Containing
import org.scalatest.enablers.Aggregating
import org.scalatest.enablers.Sequencing
import org.scalatest.enablers.KeyMapping
import org.scalatest.enablers.ValueMapping
import org.scalatest.exceptions.NotAllowedException

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
   * list should contain (null)
   *                     ^
   * </pre>
   **/
  def apply(nullValue: Null): MatcherFactory1[Any, Containing] =
    new MatcherFactory1[Any, Containing] {
      def matcher[U <: Any : Containing]: Matcher[U] =
        new Matcher[U] {
          def apply(left: U): MatchResult = {
            val containing = implicitly[Containing[U]]
            MatchResult(
              containing.contains(left, null),
              Resources.rawDidNotContainNull,
              Resources.rawContainedNull,
              Vector(left)
            )
          }
          override def toString: String = "contain (null)"
        }
      override def toString: String = "contain (null)"
    }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * Array(1, 2) should (contain (2) and contain (1))
   *                             ^
   * </pre>
   **/ 
  def apply(expectedElement: Any): MatcherFactory1[Any, Containing] =
    new MatcherFactory1[Any, Containing] {
      def matcher[U <: Any : Containing]: Matcher[U] = 
        new Matcher[U] {
          def apply(left: U): MatchResult = {
            val containing = implicitly[Containing[U]]
            new ContainingStringMatchResult(
              containing.contains(left, expectedElement),
              Resources.rawDidNotContainExpectedElement,
              Resources.rawContainedExpectedElement,
              Vector(left, expectedElement), 
              Vector(left, expectedElement)
            )
          }
          override def toString: String = "contain (" + Prettifier.default(expectedElement) + ")"
        }
      override def toString: String = "contain (" + Prettifier.default(expectedElement) + ")"
    }

  def apply(expectedElement: String): MatcherFactory1[Any, Containing] =
    new MatcherFactory1[Any, Containing] {
      def matcher[U <: Any : Containing]: Matcher[U] = 
        new Matcher[U] {
          def apply(left: U): MatchResult = {
            val containing = implicitly[Containing[U]]
            new ContainingStringMatchResult(
              containing.contains(left, expectedElement),
              Resources.rawDidNotContainExpectedElement,
              Resources.rawContainedExpectedElement,
              Vector(left, expectedElement), 
              Vector(left, expectedElement)
            )
          }
          override def toString: String = "contain (" + Prettifier.default(expectedElement) + ")"
        }
      override def toString: String = "contain (" + Prettifier.default(expectedElement) + ")"
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
  //DOTTY-ONLY infix def key[K](expectedKey: Any): MatcherFactory1[Any, KeyMapping] = 
  // SKIP-DOTTY-START 
  def key[K](expectedKey: Any): MatcherFactory1[Any, KeyMapping] =
  // SKIP-DOTTY-END
    new MatcherFactory1[Any, KeyMapping] {
      def matcher[U <: Any : KeyMapping]: Matcher[U] = 
        new Matcher[U] {
          def apply(left: U): MatchResult = {
            val keyMapping = implicitly[KeyMapping[U]]
            new ContainingStringMatchResult(
              keyMapping.containsKey(left, expectedKey),
              Resources.rawDidNotContainKey,
              Resources.rawContainedKey,
              Vector(left, expectedKey), 
              Vector(left, expectedKey)
            )
          }
          override def toString: String = "contain key " + Prettifier.default(expectedKey)
        }
      override def toString: String = "contain key " + Prettifier.default(expectedKey)
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
  //DOTTY-ONLY infix def value[K](expectedValue: Any): MatcherFactory1[Any, ValueMapping] =
  // SKIP-DOTTY-START 
  def value[K](expectedValue: Any): MatcherFactory1[Any, ValueMapping] =
  // SKIP-DOTTY-END
    new MatcherFactory1[Any, ValueMapping] {
      def matcher[U <: Any : ValueMapping]: Matcher[U] = 
        new Matcher[U] {
          def apply(left: U): MatchResult = {
            val valueMapping = implicitly[ValueMapping[U]]
            MatchResult(
              valueMapping.containsValue(left, expectedValue),
              Resources.rawDidNotContainValue,
              Resources.rawContainedValue,
              Vector(left, expectedValue)
            )
          }
          override def toString: String = "contain value " + Prettifier.default(expectedValue)
        }
      override def toString: String = "contain value " + Prettifier.default(expectedValue)
    }
  
  /**
   * This method enables the following syntax, where <code>positiveNumber</code> and <code>validNumber</code> are, for example, of type <code>AMatcher</code>:
   *
   * <pre class="stHighlight">
   * Array(1, 2, 3) should (contain a positiveNumber and contain a validNumber)
   *                                ^
   * </pre>
   **/
  //DOTTY-ONLY infix private[scalatest] def a[T](aMatcher: AMatcher[T]): Matcher[Iterable[T]] =
  // SKIP-DOTTY-START 
  private[scalatest] def a[T](aMatcher: AMatcher[T]): Matcher[Iterable[T]] =
  // SKIP-DOTTY-END
    new Matcher[Iterable[T]] {
      def apply(left: Iterable[T]): MatchResult = {
        val matched = left.find(aMatcher(_).matches)
        MatchResult(
          matched.isDefined, 
          Resources.rawDidNotContainA,
          Resources.rawContainedA,
          Vector(left, UnquotedString(aMatcher.nounName)), 
          Vector(left, UnquotedString(aMatcher.nounName), UnquotedString(if (matched.isDefined) aMatcher(matched.get).negatedFailureMessage(Prettifier.default) else "-"))
        )
      }
      override def toString: String = "contain a " + Prettifier.default(aMatcher)
    }
  
  /**
   * This method enables the following syntax, where <code>oddNumber</code> and <code>invalidNumber</code> are, for example, of type <code>AnMatcher</code>:
   *
   * <pre class="stHighlight">
   * Array(1, 2, 3) should (contain an oddNumber and contain an invalidNumber)
   *                                ^
   * </pre>
   **/
  //DOTTY-ONLY infix private[scalatest] def an[T](anMatcher: AnMatcher[T]): Matcher[Iterable[T]] =
  // SKIP-DOTTY-START 
  private[scalatest] def an[T](anMatcher: AnMatcher[T]): Matcher[Iterable[T]] =
  // SKIP-DOTTY-END
    new Matcher[Iterable[T]] {
      def apply(left: Iterable[T]): MatchResult = {
        val matched = left.find(anMatcher(_).matches)
        MatchResult(
          matched.isDefined, 
          Resources.rawDidNotContainAn,
          Resources.rawContainedAn,
          Vector(left, UnquotedString(anMatcher.nounName)), 
          Vector(left, UnquotedString(anMatcher.nounName), UnquotedString(if (matched.isDefined) anMatcher(matched.get).negatedFailureMessage(Prettifier.default) else "-"))
        )
      }
      override def toString: String = "contain an " + Prettifier.default(anMatcher)
    }

  //DOTTY-ONLY infix def oneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Containing] = {
  // SKIP-DOTTY-START
  def oneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Containing] = {
  // SKIP-DOTTY-END  
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.oneOfDuplicate, pos)
    new MatcherFactory1[Any, Containing] {
      def matcher[T](implicit containing: Containing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            new ContainingStringMatchResult(
              containing.containsOneOf(left, right),
              Resources.rawDidNotContainOneOfElements,
              Resources.rawContainedOneOfElements,
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))), 
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            )
          }
          override def toString: String = "contain oneOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
        }
      }
      override def toString: String = "contain oneOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
    }
  }

  //DOTTY-ONLY infix def oneElementOf(elements: Iterable[Any]): MatcherFactory1[Any, Containing] = {
  // SKIP-DOTTY-START
  def oneElementOf(elements: Iterable[Any]): MatcherFactory1[Any, Containing] = {
  // SKIP-DOTTY-END  
    val right = elements.toList
    new MatcherFactory1[Any, Containing] {
      def matcher[T](implicit containing: Containing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              containing.containsOneOf(left, right.distinct),
              Resources.rawDidNotContainOneElementOf,
              Resources.rawContainedOneElementOf,
              Vector(left, right)
            )
          }
          override def toString: String = "contain oneElementOf " + Prettifier.default(right)
        }
      }
      override def toString: String = "contain oneElementOf " + Prettifier.default(right)
    }
  }

  //DOTTY-ONLY infix def atLeastOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Aggregating] = {
  // SKIP-DOTTY-START
  def atLeastOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Aggregating] = {
  // SKIP-DOTTY-END  
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.atLeastOneOfDuplicate, pos)
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            new ContainingStringMatchResult(
              aggregating.containsAtLeastOneOf(left, right),
              Resources.rawDidNotContainAtLeastOneOf,
              Resources.rawContainedAtLeastOneOf,
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))), 
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            )
          }
          override def toString: String = "contain atLeastOneOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
        }
      }
      override def toString: String = "contain atLeastOneOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
    }
  }

  //DOTTY-ONLY infix def atLeastOneElementOf(elements: Iterable[Any]): MatcherFactory1[Any, Aggregating] = {
  // SKIP-DOTTY-START
  def atLeastOneElementOf(elements: Iterable[Any]): MatcherFactory1[Any, Aggregating] = {
  // SKIP-DOTTY-END  
    val right = elements.toList
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            new ContainingStringMatchResult(
              aggregating.containsAtLeastOneOf(left, right),
              Resources.rawDidNotContainAtLeastOneElementOf,
              Resources.rawContainedAtLeastOneElementOf,
              Vector(left, right), 
              Vector(left, right)
            )
          }
          override def toString: String = "contain atLeastOneElementOf " + Prettifier.default(right)
        }
      }
      override def toString: String = "contain atLeastOneElementOf " + Prettifier.default(right)
    }
  }
  
  //DOTTY-ONLY infix def noneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Containing] = {
  // SKIP-DOTTY-START
  def noneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Containing] = {
  // SKIP-DOTTY-END  
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.noneOfDuplicate, pos)
    new MatcherFactory1[Any, Containing] {
      def matcher[T](implicit containing: Containing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            new ContainingStringMatchResult(
              containing.containsNoneOf(left, right),
              Resources.rawContainedAtLeastOneOf,
              Resources.rawDidNotContainAtLeastOneOf,
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))), 
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            )
          }
          override def toString: String = "contain noneOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
        }
      }
      override def toString: String = "contain noneOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
    }
  }

  //DOTTY-ONLY infix def noElementsOf(elements: Iterable[Any]): MatcherFactory1[Any, Containing] = {
  // SKIP-DOTTY-START
  def noElementsOf(elements: Iterable[Any]): MatcherFactory1[Any, Containing] = {
  // SKIP-DOTTY-END  
    val right = elements.toList
    new MatcherFactory1[Any, Containing] {
      def matcher[T](implicit containing: Containing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            new ContainingStringMatchResult(
              containing.containsNoneOf(left, right.distinct),
              Resources.rawContainedAtLeastOneElementOf,
              Resources.rawDidNotContainAtLeastOneElementOf,
              Vector(left, right), 
              Vector(left, right)
            )
          }
          override def toString: String = "contain noElementsOf (" + Prettifier.default(right) + ")"
        }
      }
      override def toString: String = "contain noElementsOf (" + Prettifier.default(right) + ")"
    }
  }
  
  //DOTTY-ONLY infix def theSameElementsAs(right: Iterable[Any]): MatcherFactory1[Any, Aggregating] = {
  // SKIP-DOTTY-START
  def theSameElementsAs(right: Iterable[Any]): MatcherFactory1[Any, Aggregating] = {
  // SKIP-DOTTY-END  
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              aggregating.containsTheSameElementsAs(left, right),
              Resources.rawDidNotContainSameElements,
              Resources.rawContainedSameElements,
              Vector(left, right)
            )
          }
          override def toString: String = "contain theSameElementsAs " + Prettifier.default(right)
        }
      }
      override def toString: String = "contain theSameElementsAs " + Prettifier.default(right)
    }
  }
  
  //DOTTY-ONLY infix def theSameElementsInOrderAs(right: Iterable[Any]): MatcherFactory1[Any, Sequencing] = {
  // SKIP-DOTTY-START
  def theSameElementsInOrderAs(right: Iterable[Any]): MatcherFactory1[Any, Sequencing] = {
  // SKIP-DOTTY-END  
    new MatcherFactory1[Any, Sequencing] {
      def matcher[T](implicit sequencing: Sequencing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              sequencing.containsTheSameElementsInOrderAs(left, right),
              Resources.rawDidNotContainSameElementsInOrder,
              Resources.rawContainedSameElementsInOrder,
              Vector(left, right)
            )
          }
          override def toString: String = "contain theSameElementsInOrderAs " + Prettifier.default(right)
        }
      }
      override def toString: String = "contain theSameElementsInOrderAs " + Prettifier.default(right)
    }
  }
  
  //DOTTY-ONLY infix def only(right: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Aggregating] = {
  // SKIP-DOTTY-START
  def only(right: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Aggregating] = {
  // SKIP-DOTTY-END  
    if (right.isEmpty)
      throw new NotAllowedException(FailureMessages.onlyEmpty, pos)
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.onlyDuplicate, pos)
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            val withFriendlyReminder = right.size == 1 && (right(0).isInstanceOf[Iterable[_]] || right(0).isInstanceOf[Every[_]])
            MatchResult(
              aggregating.containsOnly(left, right),
              if (withFriendlyReminder) Resources.rawDidNotContainOnlyElementsWithFriendlyReminder else Resources.rawDidNotContainOnlyElements,
              if (withFriendlyReminder) Resources.rawContainedOnlyElementsWithFriendlyReminder else Resources.rawContainedOnlyElements,
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            )
          }
          override def toString: String = "contain only (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
        }
      }
      override def toString: String = "contain only (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
    }
  }

  //DOTTY-ONLY infix def inOrderOnly(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Sequencing] = {
  // SKIP-DOTTY-START
  def inOrderOnly(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Sequencing] = {
  // SKIP-DOTTY-END  
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.inOrderOnlyDuplicate, pos)
    new MatcherFactory1[Any, Sequencing] {
      def matcher[T](implicit sequencing: Sequencing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              sequencing.containsInOrderOnly(left, right),
              Resources.rawDidNotContainInOrderOnlyElements,
              Resources.rawContainedInOrderOnlyElements,
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            )
          }
          override def toString: String = "contain inOrderOnly (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
        }
      }
      override def toString: String = "contain inOrderOnly (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
    }
  }
  
  //DOTTY-ONLY infix def allOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Aggregating] = {
  // SKIP-DOTTY-START
  def allOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Aggregating] = {
  // SKIP-DOTTY-END
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.allOfDuplicate, pos)
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            new ContainingStringMatchResult(
              aggregating.containsAllOf(left, right),
              Resources.rawDidNotContainAllOfElements,
              Resources.rawContainedAllOfElements,
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))), 
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            )
          }
          override def toString: String = "contain allOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
        }
      }
      override def toString: String = "contain allOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
    }
  }

  //DOTTY-ONLY infix def allElementsOf(elements: Iterable[Any]): MatcherFactory1[Any, Aggregating] = {
  // SKIP-DOTTY-START
  def allElementsOf(elements: Iterable[Any]): MatcherFactory1[Any, Aggregating] = {
  // SKIP-DOTTY-END  
    val right = elements.toList
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            new ContainingStringMatchResult(
              aggregating.containsAllOf(left, right.distinct),
              Resources.rawDidNotContainAllElementsOf,
              Resources.rawContainedAllElementsOf,
              Vector(left, right), 
              Vector(left, right)
            )
          }
          override def toString: String = "contain allElementsOf " + Prettifier.default(right)
        }
      }
      override def toString: String = "contain allElementsOf " + Prettifier.default(right)
    }
  }
  
  //DOTTY-ONLY infix def inOrder(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Sequencing] = {
  // SKIP-DOTTY-START
  def inOrder(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Sequencing] = {
  // SKIP-DOTTY-END  
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.inOrderDuplicate, pos)
    new MatcherFactory1[Any, Sequencing] {
      def matcher[T](implicit sequencing: Sequencing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            new ContainingStringMatchResult(
              sequencing.containsInOrder(left, right),
              Resources.rawDidNotContainAllOfElementsInOrder,
              Resources.rawContainedAllOfElementsInOrder,
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))), 
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            )
          }
          override def toString: String = "contain inOrder (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
        }
      }
      override def toString: String = "contain inOrder (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
    }
  }

  //DOTTY-ONLY infix def inOrderElementsOf(elements: Iterable[Any]): MatcherFactory1[Any, Sequencing] = {
  // SKIP-DOTTY-START
  def inOrderElementsOf(elements: Iterable[Any]): MatcherFactory1[Any, Sequencing] = {
  // SKIP-DOTTY-END  
    val right = elements.toList
    new MatcherFactory1[Any, Sequencing] {
      def matcher[T](implicit sequencing: Sequencing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            new ContainingStringMatchResult(
              sequencing.containsInOrder(left, right.distinct),
              Resources.rawDidNotContainAllElementsOfInOrder,
              Resources.rawContainedAllElementsOfInOrder,
              Vector(left, right), 
              Vector(left, right)
            )
          }
          override def toString: String = "contain inOrderElementsOf (" + Prettifier.default(right) + ")"
        }
      }
      override def toString: String = "contain inOrderElementsOf (" + Prettifier.default(right) + ")"
    }
  }
  
  //DOTTY-ONLY infix def atMostOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Aggregating] = {
  // SKIP-DOTTY-START
  def atMostOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Aggregating] = {
  // SKIP-DOTTY-END  
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.atMostOneOfDuplicate, pos)
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            new ContainingStringMatchResult(
              aggregating.containsAtMostOneOf(left, right),
              Resources.rawDidNotContainAtMostOneOf,
              Resources.rawContainedAtMostOneOf,
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", "))), 
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            )
          }
          override def toString: String = "contain atMostOneOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
        }
      }
      override def toString: String = "contain atMostOneOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
    }
  }

  //DOTTY-ONLY infix def atMostOneElementOf(elements: Iterable[Any]): MatcherFactory1[Any, Aggregating] = {
  // SKIP-DOTTY-START
  def atMostOneElementOf(elements: Iterable[Any]): MatcherFactory1[Any, Aggregating] = {
  // SKIP-DOTTY-END  
    val right = elements.toList
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            new ContainingStringMatchResult(
              aggregating.containsAtMostOneOf(left, right.distinct),
              Resources.rawDidNotContainAtMostOneElementOf,
              Resources.rawContainedAtMostOneElementOf,
              Vector(left, right), 
              Vector(left, right)
            )
          }
          override def toString: String = "contain atMostOneElementOf (" + Prettifier.default(right) + ")"
        }
      }
      override def toString: String = "contain atMostOneElementOf (" + Prettifier.default(right) + ")"
    }
  }
  
  /**
   * Overrides toString to return "contain"
   */
  override def toString: String = "contain"
}
