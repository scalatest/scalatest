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
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun

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
            MatchResult(
              containing.contains(left, expectedElement),
              Resources.rawDidNotContainExpectedElement,
              Resources.rawContainedExpectedElement,
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
  def key[K](expectedKey: Any): MatcherFactory1[Any, KeyMapping] =
    new MatcherFactory1[Any, KeyMapping] {
      def matcher[U <: Any : KeyMapping]: Matcher[U] = 
        new Matcher[U] {
          def apply(left: U): MatchResult = {
            val keyMapping = implicitly[KeyMapping[U]]
            MatchResult(
              keyMapping.containsKey(left, expectedKey),
              Resources.rawDidNotContainKey,
              Resources.rawContainedKey,
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
  def value[K](expectedValue: Any): MatcherFactory1[Any, ValueMapping] =
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
  private[scalatest] def a[T](aMatcher: AMatcher[T]): Matcher[GenTraversable[T]] =
    new Matcher[GenTraversable[T]] {
      def apply(left: GenTraversable[T]): MatchResult = {
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
  private[scalatest] def an[T](anMatcher: AnMatcher[T]): Matcher[GenTraversable[T]] =
    new Matcher[GenTraversable[T]] {
      def apply(left: GenTraversable[T]): MatchResult = {
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

  def oneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Containing] = {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.oneOfDuplicate, Some(pos), getStackDepthFun(pos))
    new MatcherFactory1[Any, Containing] {
      def matcher[T](implicit containing: Containing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              containing.containsOneOf(left, right),
              Resources.rawDidNotContainOneOfElements,
              Resources.rawContainedOneOfElements,
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            )
          }
          override def toString: String = "contain oneOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
        }
      }
      override def toString: String = "contain oneOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
    }
  }

  def oneElementOf(elements: GenTraversable[Any]): MatcherFactory1[Any, Containing] = {
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

  def atLeastOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Aggregating] = {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.atLeastOneOfDuplicate, Some(pos), getStackDepthFun(pos))
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              aggregating.containsAtLeastOneOf(left, right),
              Resources.rawDidNotContainAtLeastOneOf,
              Resources.rawContainedAtLeastOneOf,
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            )
          }
          override def toString: String = "contain atLeastOneOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
        }
      }
      override def toString: String = "contain atLeastOneOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
    }
  }

  def atLeastOneElementOf(elements: GenTraversable[Any]): MatcherFactory1[Any, Aggregating] = {
    val right = elements.toList
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              aggregating.containsAtLeastOneOf(left, right),
              Resources.rawDidNotContainAtLeastOneElementOf,
              Resources.rawContainedAtLeastOneElementOf,
              Vector(left, right)
            )
          }
          override def toString: String = "contain atLeastOneElementOf " + Prettifier.default(right)
        }
      }
      override def toString: String = "contain atLeastOneElementOf " + Prettifier.default(right)
    }
  }
  
  def noneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Containing] = {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.noneOfDuplicate, Some(pos), getStackDepthFun(pos))
    new MatcherFactory1[Any, Containing] {
      def matcher[T](implicit containing: Containing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              containing.containsNoneOf(left, right),
              Resources.rawContainedAtLeastOneOf,
              Resources.rawDidNotContainAtLeastOneOf,
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            )
          }
          override def toString: String = "contain noneOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
        }
      }
      override def toString: String = "contain noneOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
    }
  }

  def noElementsOf(elements: GenTraversable[Any]): MatcherFactory1[Any, Containing] = {
    val right = elements.toList
    new MatcherFactory1[Any, Containing] {
      def matcher[T](implicit containing: Containing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              containing.containsNoneOf(left, right.distinct),
              Resources.rawContainedAtLeastOneElementOf,
              Resources.rawDidNotContainAtLeastOneElementOf,
              Vector(left, right)
            )
          }
          override def toString: String = "contain noElementsOf (" + Prettifier.default(right) + ")"
        }
      }
      override def toString: String = "contain noElementsOf (" + Prettifier.default(right) + ")"
    }
  }
  
  def theSameElementsAs(right: GenTraversable[Any]): MatcherFactory1[Any, Aggregating] = {
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
  
  def theSameElementsInOrderAs(right: GenTraversable[Any]): MatcherFactory1[Any, Sequencing] = {
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
  
  def only(right: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Aggregating] = {
    if (right.isEmpty)
      throw new NotAllowedException(FailureMessages.onlyEmpty, Some(pos), getStackDepthFun(pos))
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.onlyDuplicate, Some(pos), getStackDepthFun(pos))
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            val withFriendlyReminder = right.size == 1 && (right(0).isInstanceOf[scala.collection.GenTraversable[_]] || right(0).isInstanceOf[Every[_]])
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

  def inOrderOnly(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Sequencing] = {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.inOrderOnlyDuplicate, Some(pos), getStackDepthFun(pos))
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
  
  def allOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Aggregating] = {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.allOfDuplicate, Some(pos), getStackDepthFun(pos))
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              aggregating.containsAllOf(left, right),
              Resources.rawDidNotContainAllOfElements,
              Resources.rawContainedAllOfElements,
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            )
          }
          override def toString: String = "contain allOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
        }
      }
      override def toString: String = "contain allOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
    }
  }

  def allElementsOf(elements: GenTraversable[Any]): MatcherFactory1[Any, Aggregating] = {
    val right = elements.toList
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              aggregating.containsAllOf(left, right.distinct),
              Resources.rawDidNotContainAllElementsOf,
              Resources.rawContainedAllElementsOf,
              Vector(left, right)
            )
          }
          override def toString: String = "contain allElementsOf " + Prettifier.default(right)
        }
      }
      override def toString: String = "contain allElementsOf " + Prettifier.default(right)
    }
  }
  
  def inOrder(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Sequencing] = {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.inOrderDuplicate, Some(pos), getStackDepthFun(pos))
    new MatcherFactory1[Any, Sequencing] {
      def matcher[T](implicit sequencing: Sequencing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              sequencing.containsInOrder(left, right),
              Resources.rawDidNotContainAllOfElementsInOrder,
              Resources.rawContainedAllOfElementsInOrder,
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            )
          }
          override def toString: String = "contain inOrder (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
        }
      }
      override def toString: String = "contain inOrder (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
    }
  }

  def inOrderElementsOf(elements: GenTraversable[Any]): MatcherFactory1[Any, Sequencing] = {
    val right = elements.toList
    new MatcherFactory1[Any, Sequencing] {
      def matcher[T](implicit sequencing: Sequencing[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              sequencing.containsInOrder(left, right.distinct),
              Resources.rawDidNotContainAllElementsOfInOrder,
              Resources.rawContainedAllElementsOfInOrder,
              Vector(left, right)
            )
          }
          override def toString: String = "contain inOrderElementsOf (" + Prettifier.default(right) + ")"
        }
      }
      override def toString: String = "contain inOrderElementsOf (" + Prettifier.default(right) + ")"
    }
  }
  
  def atMostOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit prettifier: Prettifier, pos: source.Position): MatcherFactory1[Any, Aggregating] = {
    val right = firstEle :: secondEle :: remainingEles.toList
    if (right.distinct.size != right.size)
      throw new NotAllowedException(FailureMessages.atMostOneOfDuplicate, Some(pos), getStackDepthFun(pos))
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              aggregating.containsAtMostOneOf(left, right),
              Resources.rawDidNotContainAtMostOneOf,
              Resources.rawContainedAtMostOneOf,
              Vector(left, UnquotedString(right.map(r => FailureMessages.decorateToStringValue(prettifier, r)).mkString(", ")))
            )
          }
          override def toString: String = "contain atMostOneOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
        }
      }
      override def toString: String = "contain atMostOneOf (" + right.map(Prettifier.default(_)).mkString(", ") + ")"
    }
  }

  def atMostOneElementOf(elements: GenTraversable[Any]): MatcherFactory1[Any, Aggregating] = {
    val right = elements.toList
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              aggregating.containsAtMostOneOf(left, right.distinct),
              Resources.rawDidNotContainAtMostOneElementOf,
              Resources.rawContainedAtMostOneElementOf,
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
