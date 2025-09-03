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
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import java.util.Calendar
import scala.collection.JavaConversions._

object GenFactoriesDotty {

  val generatorSource = new File("GenFactories.scala")

  val topPart = """
package org.scalatest.matchers.dsl

import org.scalatest.enablers._
import org.scalatest.matchers.MatchersHelper.andMatchersAndApply
import org.scalatest.matchers.MatchersHelper.orMatchersAndApply
import org.scalactic.ColCompatHelper.Iterable
import scala.util.matching.Regex
import org.scalactic._
import TripleEqualsSupport.Spread
import TripleEqualsSupport.TripleEqualsInvocation
import org.scalatest.FailureMessages
import org.scalatest.Resources
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.BeMatcher
import org.scalatest.matchers.BePropertyMatcher
import org.scalatest.matchers.HavePropertyMatcher
import org.scalatest.matchers.AMatcher
import org.scalatest.matchers.AnMatcher
import org.scalatest.matchers.MatchPatternMacro
import org.scalatest.matchers.TypeMatcherMacro
import org.scalatest.matchers.MatchPatternHelper

import scala.language.higherKinds

/**
 * A matcher factory that can produce a matcher given $nTypeclassInstances$.
 *
 * <p>
 * In the type parameters for this class, "<code>SC</code>" means <em>superclass</em>; "<code>TC</code>"
 * (in <code>TC1</code>, <code>TC2</code>, <em>etc.</em>) means <em>typeclass</em>.
 * This class's <code>matcher</code> factory method will produce a <code>Matcher[T]</code>, where <code>T</code> is a subtype of (or the same type
 * as) <code>SC</code>, given a typeclass instance for each <code>TC<em>n</em></code>
 * implicit parameter (for example, a <code>TC1[T]</code>, <code>TC2[T]</code>, <em>etc.</em>).
 * </p>
 *
 * @author Bill Venners
 */
// Add a TYPECLASSN for each N
abstract class MatcherFactory$arity$[-SC, $typeConstructors$] { thisMatcherFactory =>

  /**
   * Factory method that will produce a <code>Matcher[T]</code>, where <code>T</code> is a subtype of (or the same type
   * as) <code>SC</code>, given a typeclass instance for each <code>TC<em>n</em></code>
   * implicit parameter (for example, a <code>TC1[T]</code>, <code>TC2[T]</code>, <em>etc.</em>).
   */
  def matcher[T <: SC : $colonSeparatedTCNs$]: Matcher[T]

$if (arityIsOne)$
  /**
   * Enables the <a href="../../org/scalactic/"><code>Explicitly</code></a> DSL to be used directly
   * on a <code>MatcherFactory1</code>, without invoking the <code>matcher</code> factory method.
   *
   * <p>
   * Here's an example of the kind of syntax this <code>apply</code> method can enable:
   * </p>
   *
   * <pre>
   * result should equal (1) (decided by defaultEquality)
   * </pre>
   */
  def apply[T <: SC](explicit: TC1[T]): Matcher[T] = {
    given TC1[T] = explicit
    matcher[T]
  }

$endif$

  /**
   * Ands this matcher factory with the passed matcher.
   */
  infix def and[U <: SC](rightMatcher: Matcher[U]): MatcherFactory$arity$[U, $commaSeparatedTCNs$] =
    new MatcherFactory$arity$[U, $commaSeparatedTCNs$] {
      def matcher[V <: U : $colonSeparatedTCNs$]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherFactory.matcher
            andMatchersAndApply(left, leftMatcher, rightMatcher)
          }
          override def toString: String = "(" + Prettifier.default(thisMatcherFactory) + ") and (" + Prettifier.default(rightMatcher) + ")"
        }
      }
      override def toString: String = "(" + Prettifier.default(thisMatcherFactory) + ") and (" + Prettifier.default(rightMatcher) + ")"
    }

  /**
   * Ors this matcher factory with the passed matcher.
   */
  infix def or[U <: SC](rightMatcher: Matcher[U]): MatcherFactory$arity$[U, $commaSeparatedTCNs$] =
    new MatcherFactory$arity$[U, $commaSeparatedTCNs$] {
      def matcher[V <: U : $colonSeparatedTCNs$]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherFactory.matcher
            orMatchersAndApply(left, leftMatcher, rightMatcher)
          }
          override def toString: String = "(" + Prettifier.default(thisMatcherFactory) + ") or (" + Prettifier.default(rightMatcher) + ")"
        }
      }
      override def toString: String = "(" + Prettifier.default(thisMatcherFactory) + ") or (" + Prettifier.default(rightMatcher) + ")"
    }

  /**
   * Ands this matcher factory with the passed <code>MatcherFactory1</code> that has the same final typeclass as this one.
   */
  infix def and[U <: SC](rightMatcherFactory: MatcherFactory1[U, TC$arity$]): MatcherFactory$arity$[U, $commaSeparatedTCNs$] =
    new MatcherFactory$arity$[U, $commaSeparatedTCNs$] {
      def matcher[V <: U : $colonSeparatedTCNs$]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherFactory.matcher
            val rightMatcher = rightMatcherFactory.matcher
            andMatchersAndApply(left, leftMatcher, rightMatcher)
          }
          override def toString: String = "(" + Prettifier.default(thisMatcherFactory) + ") and (" + Prettifier.default(rightMatcherFactory) + ")"
        }
      }
      override def toString: String = "(" + Prettifier.default(thisMatcherFactory) + ") and (" + Prettifier.default(rightMatcherFactory) + ")"
    }

  /**
   * Ors this matcher factory with the passed <code>MatcherFactory1</code> that has the same final typeclass as this one.
   */
  infix def or[U <: SC](rightMatcherFactory: MatcherFactory1[U, TC$arity$]): MatcherFactory$arity$[U, $commaSeparatedTCNs$] =
    new MatcherFactory$arity$[U, $commaSeparatedTCNs$] {
      def matcher[V <: U : $colonSeparatedTCNs$]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherFactory.matcher
            val rightMatcher = rightMatcherFactory.matcher
            orMatchersAndApply(left, leftMatcher, rightMatcher)
          }
          override def toString: String = "(" + Prettifier.default(thisMatcherFactory) + ") or (" + Prettifier.default(rightMatcherFactory) + ")"
        }
      }
      override def toString: String = "(" + Prettifier.default(thisMatcherFactory) + ") or (" + Prettifier.default(rightMatcherFactory) + ")"
    }
                """

  // And and or taking a MF1 that has the rightmost type class
  // Need one for the same typeclass and one for a different typeclass, yes, and can overload because
  // one returns a MatcherFactory1 the other a MatcherFactory2.
  // "hi" should (equal ("hi") or {mockClown.hasBigRedNose; equal ("ho")})
  // Changes the 1's to N's here, and will need to add TYPECLASSN for each N in 3 places
  // And what I'd do is use the rightmost TC. I may call these TC's. If it is the same, then I return the same one.
  // "hi" should (equal ("ho") and {mockClown.hasBigRedNose; equal ("ho")})
  // Yes, same for and. Essentially, each N must have and one each and and or methods that takes a Matcher, one and and or
  // method that takes each other MatcherFactoryN, plus one extra one for MatcherFactory1 of the rightmost type.

  // And and or taking a MF1 with a different type class
  // This one, though, I'd need to add 1 more. Amazing this overloads, but anyway. And I really need this for each N. The above
  // special case is just for MatcherFactory1. The other N's I'm not going to bother trying to do a quickie overload.

  // passedArity is the arity of the right matcher factory passed in to and and or
  // resultArity is the arity of the right matcher factory passed in to and and or, which is the arity of this
  // matcher factory itself plus the arity of the passed matcher factory.
  val middlePart = """

  /**
   * Ands this matcher factory with the passed matcher factory.
   */
  infix def and[U <: SC, $passedTypeConstructors$](rightMatcherFactory: MatcherFactory$passedArity$[U, $passedCommaSeparatedTCNs$]): MatcherFactory$resultArity$[U, $resultCommaSeparatedTCNs$] =
    new MatcherFactory$resultArity$[U, $resultCommaSeparatedTCNs$] {
      def matcher[V <: U : $resultColonSeparatedTCNs$]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherFactory.matcher
            val rightMatcher = rightMatcherFactory.matcher
            andMatchersAndApply(left, leftMatcher, rightMatcher)
          }
        }
      }
    }

  /**
   * Ors this matcher factory with the passed matcher factory.
   */
  infix def or[U <: SC, $passedTypeConstructors$](rightMatcherFactory: MatcherFactory$passedArity$[U, $passedCommaSeparatedTCNs$]): MatcherFactory$resultArity$[U, $resultCommaSeparatedTCNs$] =
    new MatcherFactory$resultArity$[U, $resultCommaSeparatedTCNs$] {
      def matcher[V <: U : $resultColonSeparatedTCNs$]: Matcher[V] = {
        new Matcher[V] {
          def apply(left: V): MatchResult = {
            val leftMatcher = thisMatcherFactory.matcher
            val rightMatcher = rightMatcherFactory.matcher
            orMatchersAndApply(left, leftMatcher, rightMatcher)
          }
        }
      }
    }
"""

  val bottomPart1 = """
  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndHaveWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and have length (3 - 1)
     *                          ^
     * </pre>
     */
    infix def length(expectedLength: Long): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Length] = and(MatcherWords.have.length(expectedLength))

    // These guys need to generate a MatcherFactory of N+1. And it needs N-1 TC's, with the last one being Length.

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and have size (3 - 1)
     *                          ^
     * </pre>
     */
    infix def size(expectedSize: Long): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Size] = and(MatcherWords.have.size(expectedSize))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and have message ("A message from Mars!")
     *                          ^
     * </pre>
     */
    infix def message(expectedMessage: String): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Messaging] = and(MatcherWords.have.message(expectedMessage))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
   *
   * <pre class="stHighlight">
   * aMatcherFactory and have size (3 - 1)
   *                     ^
   * </pre>
   */
  infix def and(haveWord: HaveWord): AndHaveWord = new AndHaveWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndContainWord(prettifier: Prettifier, pos: source.Position) {

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain (3 - 1)
     *                             ^
     * </pre>
     */
    def apply(expectedElement: Any): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] = thisMatcherFactory.and(MatcherWords.contain(expectedElement))

    // And some, the ones that would by themselves already generate a Matcher, just return a MatcherFactoryN where N is the same.

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain key ("one")
     *                             ^
     * </pre>
     */
    infix def key(expectedKey: Any): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, KeyMapping] = thisMatcherFactory.and(MatcherWords.contain.key(expectedKey))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain value (1)
     *                             ^
     * </pre>
     */
    infix def value(expectedValue: Any): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, ValueMapping] = thisMatcherFactory.and(MatcherWords.contain.value(expectedValue))

    // And some, the ones that would by themselves already generate a Matcher, just return a MatcherFactoryN where N is the same.

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain theSameElementsAs List(1, 2, 3)
     *                             ^
     * </pre>
     */
    infix def theSameElementsAs(right: Iterable[_]): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.and(MatcherWords.contain.theSameElementsAs(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain theSameElementsInOrderAs List(1, 2, 3)
     *                             ^
     * </pre>
     */
    infix def theSameElementsInOrderAs(right: Iterable[_]): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Sequencing] =
      thisMatcherFactory.and(MatcherWords.contain.theSameElementsInOrderAs(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain inOrderOnly (1, 2, 3)
     *                             ^
     * </pre>
     */
    infix def inOrderOnly(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Sequencing] =
      thisMatcherFactory.and(MatcherWords.contain.inOrderOnly(firstEle, secondEle, remainingEles.toList: _*)(prettifier, pos))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain allOf (1, 2, 3)
     *                             ^
     * </pre>
     */
    infix def allOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.and(MatcherWords.contain.allOf(firstEle, secondEle, remainingEles  .toList: _*)(prettifier, pos))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain allElementsOf List(1, 2, 3)
     *                             ^
     * </pre>
     */
    infix def allElementsOf(elements: Iterable[Any]): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.and(MatcherWords.contain.allElementsOf(elements))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain inOrder (1, 2, 3)
     *                             ^
     * </pre>
     */
    infix def inOrder(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Sequencing] =
      thisMatcherFactory.and(MatcherWords.contain.inOrder(firstEle, secondEle, remainingEles.toList: _*)(prettifier, pos))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain inOrderElementsOf List(1, 2, 3)
     *                             ^
     * </pre>
     */
    infix def inOrderElementsOf(elements: Iterable[Any]): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Sequencing] =
      thisMatcherFactory.and(MatcherWords.contain.inOrderElementsOf(elements))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain oneOf (1, 2, 3)
     *                             ^
     * </pre>
     */
    infix def oneOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.and(MatcherWords.contain.oneOf(firstEle, secondEle, remainingEles.toList: _*)(prettifier, pos))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain oneElementOf (1, 2, 3)
     *                             ^
     * </pre>
     */
    infix def oneElementOf(elements: Iterable[Any]): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.and(MatcherWords.contain.oneElementOf(elements))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain atLeastOneOf (1, 2, 3)
     *                             ^
     * </pre>
     */
    infix def atLeastOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.and(MatcherWords.contain.atLeastOneOf(firstEle, secondEle, remainingEles.toList: _*)(prettifier, pos))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain atLeastOneElementOf (1, 2, 3)
     *                             ^
     * </pre>
     */
    infix def atLeastOneElementOf(elements: Iterable[Any]): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.and(MatcherWords.contain.atLeastOneElementOf(elements))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain only (1, 2, 3)
     *                             ^
     * </pre>
     */
    infix def only(right: Any*): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.and(MatcherWords.contain.only(right.toList: _*)(prettifier, pos))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain noneOf (1, 2, 3)
     *                             ^
     * </pre>
     */
    infix def noneOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.and(MatcherWords.contain.noneOf(firstEle, secondEle, remainingEles.toList: _*)(prettifier, pos))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain noElementsOf (1, 2, 3)
     *                             ^
     * </pre>
     */
    infix def noElementsOf(elements: Iterable[Any]): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.and(MatcherWords.contain.noElementsOf(elements))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain atMostOneOf (1, 2, 3)
     *                             ^
     * </pre>
     */
    infix def atMostOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.and(MatcherWords.contain.atMostOneOf(firstEle, secondEle, remainingEles.toList: _*)(prettifier, pos))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and contain atMostOneOf (1, 2, 3)
     *                             ^
     * </pre>
     */
    infix def atMostOneElementOf(elements: Iterable[Any]): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.and(MatcherWords.contain.atMostOneElementOf(elements))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
   *
   * <pre class="stHighlight">
   * aMatcherFactory and contain key ("one")
   *                 ^
   * </pre>
   */
  infix def and(containWord: ContainWord)(implicit prettifier: Prettifier, pos: source.Position): AndContainWord = new AndContainWord(prettifier, pos)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndBeWord {

    // SKIP-SCALATESTJS,NATIVE-START
    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and be a ('file)
     *                        ^
     * </pre>
     */
    infix def a(symbol: Symbol): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] = and(MatcherWords.be.a(symbol))
    // SKIP-SCALATESTJS,NATIVE-END

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>file</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and be a (file)
     *                        ^
     * </pre>
     */
    infix def a[U](bePropertyMatcher: BePropertyMatcher[U]): MatcherFactory$arity$[SC with AnyRef with U, $commaSeparatedTCNs$] = and(MatcherWords.be.a(bePropertyMatcher))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>validNumber</code> is an <a href="AMatcher.html"><code>AMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and be a (validNumber)
     *                        ^
     * </pre>
     */
    infix def a[U](aMatcher: AMatcher[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = and(MatcherWords.be.a(aMatcher))

    // SKIP-SCALATESTJS,NATIVE-START
    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and be an ('apple)
     *                        ^
     * </pre>
     */
    infix def an(symbol: Symbol): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] = and(MatcherWords.be.an(symbol))
    // SKIP-SCALATESTJS,NATIVE-END

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>apple</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and be an (apple)
     *                        ^
     * </pre>
     */
    infix def an[U](bePropertyMatcher: BePropertyMatcher[U]): MatcherFactory$arity$[SC with AnyRef with U, $commaSeparatedTCNs$] = and(MatcherWords.be.an(bePropertyMatcher))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>integerNumber</code> is an <a href="AnMatcher.html"><code>AnMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and be an (integerNumber)
     *                        ^
     * </pre>
     */
    infix def an[U](anMatcher: AnMatcher[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = and(MatcherWords.be.an(anMatcher))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and be theSameInstanceAs (string)
     *                        ^
     * </pre>
     */
    infix def theSameInstanceAs(anyRef: AnyRef): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] = and(MatcherWords.be.theSameInstanceAs(anyRef))

    /**
     * This method enables the following syntax, where <code>fraction</code> refers to a <code>PartialFunction</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and be definedAt (8)
     *                        ^
     * </pre>
     */
    infix def definedAt[A, U <: PartialFunction[A, _]](right: A): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = and(MatcherWords.be.definedAt(right))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
   *
   * <pre class="stHighlight">
   * aMatcherFactory and be a ('file)
   *                 ^
   * </pre>
   */
  infix def and(beWord: BeWord): AndBeWord = new AndBeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndFullyMatchWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and fullyMatch regex (decimal)
     *                                ^
     * </pre>
     */
    infix def regex(regexString: String): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = and(MatcherWords.fullyMatch.regex(regexString))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and fullyMatch regex (("a(b*)c" withGroup "bb"))
     *                                ^
     * </pre>
     */
    infix def regex(regexWithGroups: RegexWithGroups): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = and(MatcherWords.fullyMatch.regex(regexWithGroups))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and fullyMatch regex (decimalRegex)
     *                                ^
     * </pre>
     */
    infix def regex(regex: Regex): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = and(MatcherWords.fullyMatch.regex(regex))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
   *
   * <pre class="stHighlight">
   * aMatcherFactory and fullyMatch regex (decimalRegex)
   *                 ^
   * </pre>
   */
  infix def and(fullyMatchWord: FullyMatchWord): AndFullyMatchWord = new AndFullyMatchWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndIncludeWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and include regex (decimal)
     *                             ^
     * </pre>
     */
    infix def regex(regexString: String): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = and(MatcherWords.include.regex(regexString))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and include regex ("a(b*)c" withGroup "bb")
     *                             ^
     * </pre>
     */
    infix def regex(regexWithGroups: RegexWithGroups): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = and(MatcherWords.include.regex(regexWithGroups))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and include regex (decimalRegex)
     *                             ^
     * </pre>
     */
    infix def regex(regex: Regex): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = and(MatcherWords.include.regex(regex))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
   *
   * <pre class="stHighlight">
   * aMatcherFactory and include regex ("wor.d")
   *                 ^
   * </pre>
   */
  infix def and(includeWord: IncludeWord): AndIncludeWord = new AndIncludeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndStartWithWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and startWith regex (decimal)
     *                               ^
     * </pre>
     */
    infix def regex(regexString: String): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = and(MatcherWords.startWith.regex(regexString))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and startWith regex ("a(b*)c" withGroup "bb")
     *                               ^
     * </pre>
     */
    infix def regex(regexWithGroups: RegexWithGroups): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = and(MatcherWords.startWith.regex(regexWithGroups))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and startWith regex (decimalRegex)
     *                               ^
     * </pre>
     */
    infix def regex(regex: Regex): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = and(MatcherWords.startWith.regex(regex))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
   *
   * <pre class="stHighlight">
   * aMatcherFactory and startWith regex ("1.7")
   *                 ^
   * </pre>
   */
  infix def and(startWithWord: StartWithWord): AndStartWithWord = new AndStartWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndEndWithWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and endWith regex (decimal)
     *                             ^
     * </pre>
     */
    infix def regex(regexString: String): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = and(MatcherWords.endWith.regex(regexString))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and endWith regex ("a(b*)c" withGroup "bb")
     *                             ^
     * </pre>
     */
    infix def regex(regexWithGroups: RegexWithGroups): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = and(MatcherWords.endWith.regex(regexWithGroups))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and endWith regex (decimalRegex)
     *                             ^
     * </pre>
     */
    infix def regex(regex: Regex): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = and(MatcherWords.endWith.regex(regex))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
   *
   * <pre class="stHighlight">
   * aMatcherFactory and endWith regex (decimalRegex)
   *                 ^
   * </pre>
   */
  infix def and(endWithWord: EndWithWord): AndEndWithWord = new AndEndWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class AndNotWord(prettifier: Prettifier, pos: source.Position) { thisAndNotWord =>

    /**
     * Get the <code>MatcherFactory</code> instance, currently used by macro only.
     */
     val owner = thisMatcherFactory

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not equal (3 - 1)
     *                         ^
     * </pre>
     */
    infix def equal(any: Any): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Equality] =
      thisMatcherFactory.and(MatcherWords.not.apply(MatcherWords.equal(any)))

    /**
     * This method enables the following syntax, for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not equal (17.0 +- 0.2)
     *                         ^
     * </pre>
     */
    infix def equal[U](spread: Spread[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = thisMatcherFactory.and(MatcherWords.not.equal(spread))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not equal (null)
     *                         ^
     * </pre>
     */
    infix def equal(o: Null): MatcherFactory$arity$[SC, $commaSeparatedTCNs$] = {
      thisMatcherFactory and {
        new Matcher[SC] {
          def apply(left: SC): MatchResult = {
            MatchResult(
              left != null,
              Resources.rawEqualedNull,
              Resources.rawDidNotEqualNull,
              Resources.rawMidSentenceEqualedNull,
              Resources.rawDidNotEqualNull,
              Vector.empty,
              Vector(left)
            )
          }
          override def toString: String = "not equal null"
        }
      }
    }

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be (3 - 1)
     *                         ^
     * </pre>
     */
    infix def be(any: Any): MatcherFactory$arity$[SC, $commaSeparatedTCNs$] =
      thisMatcherFactory.and(MatcherWords.not.apply(MatcherWords.be(any)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not have length (3)
     *                         ^
     * </pre>
     */
    infix def have(resultOfLengthWordApplication: ResultOfLengthWordApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Length] =
      thisMatcherFactory.and(MatcherWords.not.apply(MatcherWords.have.length(resultOfLengthWordApplication.expectedLength)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not have size (3)
     *                         ^
     * </pre>
     */
    infix def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Size] =
      thisMatcherFactory.and(MatcherWords.not.apply(MatcherWords.have.size(resultOfSizeWordApplication.expectedSize)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not have message ("Message from Mars!")
     *                         ^
     * </pre>
     */
    infix def have(resultOfMessageWordApplication: ResultOfMessageWordApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Messaging] =
      thisMatcherFactory.and(MatcherWords.not.apply(MatcherWords.have.message(resultOfMessageWordApplication.expectedMessage)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not have (author ("Melville"))
     *                         ^
     * </pre>
     */
    infix def have[U](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] =
      thisMatcherFactory.and(MatcherWords.not.apply(MatcherWords.have(firstPropertyMatcher, propertyMatchers: _*)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be &lt; (6)
     *                         ^
     * </pre>
     */
    infix def be[U](resultOfLessThanComparison: ResultOfLessThanComparison[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] =
      thisMatcherFactory.and(MatcherWords.not.be(resultOfLessThanComparison))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be (null)
     *                         ^
     * </pre>
     */
    infix def be(o: Null): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] = thisMatcherFactory.and(MatcherWords.not.be(o))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory (8) and not be &gt; (6)
     *                             ^
     * </pre>
     */
    infix def be[U](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] =
      thisMatcherFactory.and(MatcherWords.not.be(resultOfGreaterThanComparison))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be &lt;= (2)
     *                         ^
     * </pre>
     */
    infix def be[U](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] =
      thisMatcherFactory.and(MatcherWords.not.be(resultOfLessThanOrEqualToComparison))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be &gt;= (6)
     *                         ^
     * </pre>
     */
    infix def be[U](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] =
      thisMatcherFactory.and(MatcherWords.not.be(resultOfGreaterThanOrEqualToComparison))

    /**
     * <strong>
     * The deprecation period for the should be === syntax has expired, and the syntax may no longer be
     * used.  Please use should equal, should ===, shouldEqual,
     * should be, or shouldBe instead.
     * </strong>
     *
     * <p>
     * Note: usually syntax will be removed after its deprecation period. This was left in because otherwise the syntax could in some
     * cases still compile, but silently wouldn't work.
     * </p>
     */
    infix def be(tripleEqualsInvocation: TripleEqualsInvocation[_]): MatcherFactory$arity$[SC, $commaSeparatedTCNs$] =
      thisMatcherFactory.and(MatcherWords.not.be(tripleEqualsInvocation)(pos))

    // SKIP-SCALATESTJS,NATIVE-START
    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be ('empty)
     *                         ^
     * </pre>
     */
    infix def be(symbol: Symbol): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] = thisMatcherFactory.and(MatcherWords.not.be(symbol))
    // SKIP-SCALATESTJS,NATIVE-END

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>odd</code> is a <a href="BeMatcher.html"><code>BeMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be (odd)
     *                         ^
     * </pre>
     */
    infix def be[U](beMatcher: BeMatcher[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = thisMatcherFactory.and(MatcherWords.not.be(beMatcher))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>directory</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be (directory)
     *                         ^
     * </pre>
     */
    infix def be[U](bePropertyMatcher: BePropertyMatcher[U]): MatcherFactory$arity$[SC with AnyRef with U, $commaSeparatedTCNs$] = thisMatcherFactory.and(MatcherWords.not.be(bePropertyMatcher))

    // SKIP-SCALATESTJS,NATIVE-START
    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be a ('file)
     *                         ^
     * </pre>
     */
    infix def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] = thisMatcherFactory.and(MatcherWords.not.be(resultOfAWordApplication))
    // SKIP-SCALATESTJS,NATIVE-END

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>validMarks</code> is an <a href="AMatcher.html"><code>AMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be a (validMarks)
     *                         ^
     * </pre>
     */
    infix def be[U](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = thisMatcherFactory.and(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>directory</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be a (directory)
     *                         ^
     * </pre>
     */
    infix def be[U <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = thisMatcherFactory.and(MatcherWords.not.be(resultOfAWordApplication))

    // SKIP-SCALATESTJS,NATIVE-START
    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be a primeNumber
     *                         ^
     * </pre>
     */
    infix def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] = thisMatcherFactory.and(MatcherWords.not.be(resultOfAnWordApplication))
    // SKIP-SCALATESTJS,NATIVE-END

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>apple</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be an (apple)
     *                         ^
     * </pre>
     */
    infix def be[SC <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[SC]) = thisMatcherFactory.and(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>invalidMarks</code> is a <a href="AnMatcher.html"><code>AnMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be an (invalidMarks)
     *                         ^
     * </pre>
     */
    infix def be[U](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = thisMatcherFactory.and(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be a [Book]
     *                         ^
     * </pre>
     */
    inline infix def be(aType: ResultOfATypeInvocation[_]): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] =
      \${ MatcherFactory$arity$.andNotATypeMatcherFactory$arity$[SC, $commaSeparatedTCNs$]('{thisAndNotWord: MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$]#AndNotWord}, '{aType}) }
0
    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be an [Apple]
     *                         ^
     * </pre>
     */
    inline infix def be(anType: ResultOfAnTypeInvocation[_]): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] =
      \${ MatcherFactory$arity$.andNotAnTypeMatcherFactory$arity$[SC, $commaSeparatedTCNs$]('{thisAndNotWord: MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$]#AndNotWord}, '{anType}) }

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be theSameInstanceAs (otherString)
     *                         ^
     * </pre>
     */
    infix def be(resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] = thisMatcherFactory.and(MatcherWords.not.be(resultOfTheSameInstanceAsApplication))

    /**
     * This method enables the following syntax, for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be (17.0 +- 0.2)
     *                         ^
     * </pre>
     */
    infix def be[U](spread: Spread[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = thisMatcherFactory.and(MatcherWords.not.be(spread))

    /**
     * This method enables the following syntax, where <code>fraction</code> is a <code>PartialFunction</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be definedAt (8)
     *                         ^
     * </pre>
     */
    infix def be[A, U <: PartialFunction[A, _]](resultOfDefinedAt: ResultOfDefinedAt[A]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] =
      thisMatcherFactory.and(MatcherWords.not.be(resultOfDefinedAt))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be sorted
     *                         ^
     * </pre>
     */
    infix def be(sortedWord: SortedWord) =
      thisMatcherFactory.and(MatcherWords.not.be(sortedWord))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be readable
     *                         ^
     * </pre>
     */
    infix def be(readableWord: ReadableWord) =
      thisMatcherFactory.and(MatcherWords.not.be(readableWord))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be writable
     *                         ^
     * </pre>
     */
    infix def be(writableWord: WritableWord) =
      thisMatcherFactory.and(MatcherWords.not.be(writableWord))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be empty
     *                         ^
     * </pre>
     */
    infix def be(emptyWord: EmptyWord) =
      thisMatcherFactory.and(MatcherWords.not.be(emptyWord))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be defined
     *                         ^
     * </pre>
     */
    infix def be(definedWord: DefinedWord) =
      thisMatcherFactory.and(MatcherWords.not.be(definedWord))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not fullyMatch regex (decimal)
     *                         ^
     * </pre>
     */
    infix def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] =
      thisMatcherFactory.and(MatcherWords.not.fullyMatch(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not include regex (decimal)
     *                         ^
     * </pre>
     */
    infix def include(resultOfRegexWordApplication: ResultOfRegexWordApplication): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] =
      thisMatcherFactory.and(MatcherWords.not.include(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not include ("1.7")
     *                         ^
     * </pre>
     */
    infix def include(expectedSubstring: String): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] =
      thisMatcherFactory.and(MatcherWords.not.include(expectedSubstring))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not startWith regex (decimal)
     *                         ^
     * </pre>
     */
    infix def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] =
      thisMatcherFactory.and(MatcherWords.not.startWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not startWith ("1.7")
     *                         ^
     * </pre>
     */
    infix def startWith(expectedSubstring: String): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] =
      thisMatcherFactory.and(MatcherWords.not.startWith(expectedSubstring))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not endWith regex (decimal)
     *                         ^
     * </pre>
     */
    infix def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] =
      thisMatcherFactory.and(MatcherWords.not.endWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not endWith ("1.7")
     *                         ^
     * </pre>
     */
    infix def endWith(expectedSubstring: String): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] =
      thisMatcherFactory.and(MatcherWords.not.endWith(expectedSubstring))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain (3)
     *                         ^
     * </pre>
     */
    infix def contain[U](expectedElement: U): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.and(MatcherWords.not.contain(expectedElement))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain key ("three")
     *                         ^
     * </pre>
     */
    infix def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, KeyMapping] =
      thisMatcherFactory.and(MatcherWords.not.contain(resultOfKeyWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain value (3)
     *                         ^
     * </pre>
     */
    infix def contain(resultOfValueWordApplication: ResultOfValueWordApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, ValueMapping] =
      thisMatcherFactory.and(MatcherWords.not.contain(resultOfValueWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain oneOf (8, 1, 2)
     *                         ^
     * </pre>
     */
    infix def contain(right: ResultOfOneOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.and(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain oneElementOf (List(8, 1, 2))
     *                         ^
     * </pre>
     */
    infix def contain(right: ResultOfOneElementOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.and(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain atLeastOneOf (8, 1, 2)
     *                         ^
     * </pre>
     */
    infix def contain(right: ResultOfAtLeastOneOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.and(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain atLeastOneOf (8, 1, 2)
     *                         ^
     * </pre>
     */
    infix def contain(right: ResultOfAtLeastOneElementOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.and(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain noneOf (8, 1, 2)
     *                         ^
     * </pre>
     */
    infix def contain(right: ResultOfNoneOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.and(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain noElementsOf (8, 1, 2)
     *                         ^
     * </pre>
     */
    infix def contain(right: ResultOfNoElementsOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.and(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain theSameElementsAs (List(8, 1, 2))
     *                         ^
     * </pre>
     */
    infix def contain(right: ResultOfTheSameElementsAsApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.and(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain theSameElementsInOrderAs (List(8, 1, 2))
     *                         ^
     * </pre>
     */
    infix def contain(right: ResultOfTheSameElementsInOrderAsApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Sequencing] =
      thisMatcherFactory.and(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain only (8, 1, 2)
     *                         ^
     * </pre>
     */
    infix def contain(right: ResultOfOnlyApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.and(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain inOrderOnly (8, 1, 2)
     *                         ^
     * </pre>
     */
    infix def contain(right: ResultOfInOrderOnlyApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Sequencing] =
      thisMatcherFactory.and(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain allOf (8, 1, 2)
     *                         ^
     * </pre>
     */
    infix def contain(right: ResultOfAllOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.and(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain allElementsOf (8, 1, 2)
     *                         ^
     * </pre>
     */
    infix def contain(right: ResultOfAllElementsOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.and(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain inOrder (8, 1, 2)
     *                         ^
     * </pre>
     */
    infix def contain(right: ResultOfInOrderApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Sequencing] =
      thisMatcherFactory.and(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain inOrder (8, 1, 2)
     *                         ^
     * </pre>
     */
    infix def contain(right: ResultOfInOrderElementsOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Sequencing] =
      thisMatcherFactory.and(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain atMostOneOf (8, 1, 2)
     *                         ^
     * </pre>
     */
    infix def contain(right: ResultOfAtMostOneOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.and(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not contain atMostOneElementOf (List(8, 1, 2))
     *                         ^
     * </pre>
     */
    infix def contain(right: ResultOfAtMostOneElementOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.and(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not matchPattern { case Person("Bob", _) =>}
     *                         ^
     * </pre>
     */
    inline infix def matchPattern(inline right: PartialFunction[Any, _]) =
      \${ MatcherFactory$arity$.andNotMatchPattern('{thisAndNotWord: MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$]#AndNotWord}, '{right}) }
  }
                    """

  val bottomPart2 = """

  /**
   * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
   *
   * <pre class="stHighlight">
   * aMatcherFactory and not contain value (3)
   *                 ^
   * </pre>
   */
  infix def and(notWord: NotWord)(implicit prettifier: Prettifier, pos: source.Position): AndNotWord = new AndNotWord(prettifier, pos)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcherFactory and exist
   *                 ^
   * </pre>
   */
  infix def and(existWord: ExistWord): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Existence] =
    thisMatcherFactory.and(MatcherWords.exist.matcherFactory)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcherFactory and not (exist)
   *                 ^
   * </pre>
   */
  infix def and(notExist: ResultOfNotExist): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Existence] =
    thisMatcherFactory.and(MatcherWords.not.exist)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrHaveWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or have length (3 - 1)
     *                         ^
     * </pre>
     */
    infix def length(expectedLength: Long): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Length] = or(MatcherWords.have.length(expectedLength))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or have size (3 - 1)
     *                         ^
     * </pre>
     */
    infix def size(expectedSize: Long): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Size] = or(MatcherWords.have.size(expectedSize))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or have message ("Message from Mars!")
     *                         ^
     * </pre>
     */
    infix def message(expectedMessage: String): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Messaging] = or(MatcherWords.have.message(expectedMessage))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
   *
   * <pre class="stHighlight">
   * aMatcherFactory or have size (3 - 1)
   *                 ^
   * </pre>
   */
  infix def or(haveWord: HaveWord): OrHaveWord = new OrHaveWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrContainWord(prettifier: Prettifier, pos: source.Position) {

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain (3 - 1)
     *                            ^
     * </pre>
     */
    def apply(expectedElement: Any): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] = thisMatcherFactory.or(MatcherWords.contain(expectedElement))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain key ("one")
     *                            ^
     * </pre>
     */
    infix def key(expectedKey: Any): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, KeyMapping] = thisMatcherFactory.or(MatcherWords.contain.key(expectedKey))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain value (1)
     *                            ^
     * </pre>
     */
    infix def value(expectedValue: Any): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, ValueMapping] = thisMatcherFactory.or(MatcherWords.contain.value(expectedValue))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain theSameElementsAs List(1, 2, 3)
     *                            ^
     * </pre>
     */
    infix def theSameElementsAs(right: Iterable[_]): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.or(MatcherWords.contain.theSameElementsAs(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain theSameElementsInOrderAs List(1, 2, 3)
     *                            ^
     * </pre>
     */
    infix def theSameElementsInOrderAs(right: Iterable[_]): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Sequencing] =
      thisMatcherFactory.or(MatcherWords.contain.theSameElementsInOrderAs(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain inOrderOnly (1, 2, 3)
     *                            ^
     * </pre>
     */
    infix def inOrderOnly(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Sequencing] =
      thisMatcherFactory.or(MatcherWords.contain.inOrderOnly(firstEle, secondEle, remainingEles.toList: _*)(prettifier, pos))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain allOf (1, 2, 3)
     *                            ^
     * </pre>
     */
    infix def allOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.or(MatcherWords.contain.allOf(firstEle, secondEle, remainingEles.toList: _*)(prettifier, pos))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain allElementsOf (1, 2, 3)
     *                            ^
     * </pre>
     */
    infix def allElementsOf(elements: Iterable[Any]): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.or(MatcherWords.contain.allElementsOf(elements))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain inOrder (1, 2, 3)
     *                            ^
     * </pre>
     */
    infix def inOrder(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Sequencing] =
      thisMatcherFactory.or(MatcherWords.contain.inOrder(firstEle, secondEle, remainingEles.toList: _*)(prettifier, pos))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain inOrderElementsOf List(1, 2, 3)
     *                            ^
     * </pre>
     */
    infix def inOrderElementsOf(elements: Iterable[Any]): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Sequencing] =
      thisMatcherFactory.or(MatcherWords.contain.inOrderElementsOf(elements))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain oneOf (1, 2, 3)
     *                            ^
     * </pre>
     */
    infix def oneOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.or(MatcherWords.contain.oneOf(firstEle, secondEle, remainingEles.toList: _*)(prettifier, pos))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain oneElementOf (1, 2, 3)
     *                            ^
     * </pre>
     */
    infix def oneElementOf(elements: Iterable[Any]): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.or(MatcherWords.contain.oneElementOf(elements))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain atLeastOneOf (1, 2, 3)
     *                            ^
     * </pre>
     */
    infix def atLeastOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.or(MatcherWords.contain.atLeastOneOf(firstEle, secondEle, remainingEles.toList: _*)(prettifier, pos))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain atLeastOneOf (1, 2, 3)
     *                            ^
     * </pre>
     */
    infix def atLeastOneElementOf(elements: Iterable[Any]): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.or(MatcherWords.contain.atLeastOneElementOf(elements))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain only (1, 2, 3)
     *                            ^
     * </pre>
     */
    infix def only(right: Any*): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.or(MatcherWords.contain.only(right.toList: _*)(prettifier, pos))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain noneOf (1, 2, 3)
     *                            ^
     * </pre>
     */
    infix def noneOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.or(MatcherWords.contain.noneOf(firstEle, secondEle, remainingEles.toList: _*)(prettifier, pos))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain noElementsOf (1, 2, 3)
     *                            ^
     * </pre>
     */
    infix def noElementsOf(elements: Iterable[Any]): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.or(MatcherWords.contain.noElementsOf(elements))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain atMostOneOf (1, 2, 3)
     *                            ^
     * </pre>
     */
    infix def atMostOneOf(firstEle: Any, secondEle: Any, remainingEles: Any*): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.or(MatcherWords.contain.atMostOneOf(firstEle, secondEle, remainingEles.toList: _*)(prettifier, pos))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or contain atMostOneOf (1, 2, 3)
     *                            ^
     * </pre>
     */
    infix def atMostOneElementOf(elements: Iterable[Any]): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.or(MatcherWords.contain.atMostOneElementOf(elements))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
   *
   * <pre class="stHighlight">
   * (aMatcherFactory or contain value (1))
   *                  ^
   * </pre>
   */
  infix def or(containWord: ContainWord)(implicit prettifier: Prettifier, pos: source.Position): OrContainWord = new OrContainWord(prettifier, pos)

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrBeWord {

    // SKIP-SCALATESTJS,NATIVE-START
    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or be a ('directory)
     *                       ^
     * </pre>
     */
    infix def a(symbol: Symbol): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] = or(MatcherWords.be.a(symbol))
    // SKIP-SCALATESTJS,NATIVE-END

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or be a (directory)
     *                       ^
     * </pre>
     */
    infix def a[U](bePropertyMatcher: BePropertyMatcher[U]): MatcherFactory$arity$[SC with AnyRef with U, $commaSeparatedTCNs$] = or(MatcherWords.be.a(bePropertyMatcher))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or be a (validNumber)
     *                       ^
     * </pre>
     */
    infix def a[U](aMatcher: AMatcher[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = or(MatcherWords.be.a(aMatcher))

    // SKIP-SCALATESTJS,NATIVE-START
    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or be an ('apple)
     *                       ^
     * </pre>
     */
    infix def an(symbol: Symbol): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] = or(MatcherWords.be.an(symbol))
    // SKIP-SCALATESTJS,NATIVE-END

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>apple</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or be an (apple)
     *                       ^
     * </pre>
     */
    infix def an[U](bePropertyMatcher: BePropertyMatcher[U]): MatcherFactory$arity$[SC with AnyRef with U, $commaSeparatedTCNs$] = or(MatcherWords.be.an(bePropertyMatcher))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>integerNumber</code> is a <a href="AnMatcher.html"><code>AnMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or be an (integerNumber)
     *                       ^
     * </pre>
     */
    infix def an[U](anMatcher: AnMatcher[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = or(MatcherWords.be.an(anMatcher))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or be theSameInstanceAs (otherString)
     *                       ^
     * </pre>
     */
    infix def theSameInstanceAs(anyRef: AnyRef): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] = or(MatcherWords.be.theSameInstanceAs(anyRef))

    /**
     * This method enables the following syntax, where <code>fraction</code> refers to a <code>PartialFunction</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or be definedAt (8)
     *                       ^
     * </pre>
     */
    infix def definedAt[A, U <: PartialFunction[A, _]](right: A): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = or(MatcherWords.be.definedAt(right))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
   *
   * <pre class="stHighlight">
   * aMatcherFactory or be a ('directory)
   *                 ^
   * </pre>
   */
  infix def or(beWord: BeWord): OrBeWord = new OrBeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrFullyMatchWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or fullyMatch regex (decimal)
     *                               ^
     * </pre>
     */
    infix def regex(regexString: String): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = or(MatcherWords.fullyMatch.regex(regexString))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or fullyMatch regex ("a(b*)c" withGroup "bb")
     *                               ^
     * </pre>
     */
    infix def regex(regexWithGroups: RegexWithGroups): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = or(MatcherWords.fullyMatch.regex(regexWithGroups))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or fullyMatch regex (decimal)
     *                               ^
     * </pre>
     */
    infix def regex(regex: Regex): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = or(MatcherWords.fullyMatch.regex(regex))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
   *
   * <pre class="stHighlight">
   * aMatcherFactory or fullyMatch regex (decimal)
   *                 ^
   * </pre>
   */
  infix def or(fullyMatchWord: FullyMatchWord): OrFullyMatchWord = new OrFullyMatchWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrIncludeWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or include regex (decimal)
     *                            ^
     * </pre>
     */
    infix def regex(regexString: String): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = or(MatcherWords.include.regex(regexString))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or include regex ("a(b*)c" withGroup "bb")
     *                            ^
     * </pre>
     */
    infix def regex(regexWithGroups: RegexWithGroups): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = or(MatcherWords.include.regex(regexWithGroups))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or include regex (decimal)
     *                            ^
     * </pre>
     */
    infix def regex(regex: Regex): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = or(MatcherWords.include.regex(regex))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
   *
   * <pre class="stHighlight">
   * aMatcherFactory or include regex ("1.7")
   *                 ^
   * </pre>
   */
  infix def or(includeWord: IncludeWord): OrIncludeWord = new OrIncludeWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrStartWithWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or startWith regex (decimal)
     *                              ^
     * </pre>
     */
    infix def regex(regexString: String): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = or(MatcherWords.startWith.regex(regexString))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or startWith regex ("a(b*)c" withGroup "bb")
     *                              ^
     * </pre>
     */
    infix def regex(regexWithGroups: RegexWithGroups): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = or(MatcherWords.startWith.regex(regexWithGroups))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or startWith regex (decimal)
     *                              ^
     * </pre>
     */
    infix def regex(regex: Regex): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = or(MatcherWords.startWith.regex(regex))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
   *
   * <pre class="stHighlight">
   * aMatcherFactory or startWith regex ("1.7")
   *                 ^
   * </pre>
   */
  infix def or(startWithWord: StartWithWord): OrStartWithWord = new OrStartWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrEndWithWord {

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or endWith regex (decimal)
     *                            ^
     * </pre>
     */
    infix def regex(regexString: String): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = or(MatcherWords.endWith.regex(regexString))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or endWith regex ("d(e*)f" withGroup "ee")
     *                            ^
     * </pre>
     */
    infix def regex(regexWithGroups: RegexWithGroups): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = or(MatcherWords.endWith.regex(regexWithGroups))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or endWith regex (decimal)
     *                            ^
     * </pre>
     */
    infix def regex(regex: Regex): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] = or(MatcherWords.endWith.regex(regex))
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
   *
   * <pre class="stHighlight">
   * aMatcherFactory or endWith regex ("7b")
   *                 ^
   * </pre>
   */
  infix def or(endWithWord: EndWithWord): OrEndWithWord = new OrEndWithWord

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * @author Bill Venners
   */
  final class OrNotWord(prettifier: Prettifier, pos: source.Position) { thisOrNotWord =>

    /**
     * Get the <code>MatcherFactory</code> instance, currently used by macro.
     */
    val owner = thisMatcherFactory

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not equal (3 - 1)
     *                        ^
     * </pre>
     */
    infix def equal(any: Any): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Equality] =
      thisMatcherFactory.or(MatcherWords.not.apply(MatcherWords.equal(any)))

    /**
     * This method enables the following syntax for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not equal (17.0 +- 0.2)
     *                        ^
     * </pre>
     */
    infix def equal[U](spread: Spread[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = thisMatcherFactory.or(MatcherWords.not.equal(spread))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not equal (null)
     *                        ^
     * </pre>
     */
    infix def equal(o: Null): MatcherFactory$arity$[SC, $commaSeparatedTCNs$] = {
      thisMatcherFactory or {
        new Matcher[SC] {
          def apply(left: SC): MatchResult = {
            MatchResult(
              left != null,
              Resources.rawEqualedNull,
              Resources.rawDidNotEqualNull,
              Resources.rawMidSentenceEqualedNull,
              Resources.rawDidNotEqualNull,
              Vector.empty,
              Vector(left)
            )
          }
          override def toString: String = "not equal null"
        }
      }
    }

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be (2)
     *                        ^
     * </pre>
     */
    infix def be(any: Any): MatcherFactory$arity$[SC, $commaSeparatedTCNs$] =
      thisMatcherFactory.or(MatcherWords.not.apply(MatcherWords.be(any)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not have length (3)
     *                        ^
     * </pre>
     */
    infix def have(resultOfLengthWordApplication: ResultOfLengthWordApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Length] =
      thisMatcherFactory.or(MatcherWords.not.apply(MatcherWords.have.length(resultOfLengthWordApplication.expectedLength)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not have size (3)
     *                        ^
     * </pre>
     */
    infix def have(resultOfSizeWordApplication: ResultOfSizeWordApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Size] =
      thisMatcherFactory.or(MatcherWords.not.apply(MatcherWords.have.size(resultOfSizeWordApplication.expectedSize)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not have message ("Message from Mars!")
     *                        ^
     * </pre>
     */
    infix def have(resultOfMessageWordApplication: ResultOfMessageWordApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Messaging] =
      thisMatcherFactory.or(MatcherWords.not.apply(MatcherWords.have.message(resultOfMessageWordApplication.expectedMessage)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not have (author ("Melville"))
     *                        ^
     * </pre>
     */
    infix def have[U](firstPropertyMatcher: HavePropertyMatcher[U, _], propertyMatchers: HavePropertyMatcher[U, _]*): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] =
      thisMatcherFactory.or(MatcherWords.not.apply(MatcherWords.have(firstPropertyMatcher, propertyMatchers: _*)))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be (null)
     *                        ^
     * </pre>
     */
    infix def be(o: Null): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] = thisMatcherFactory.or(MatcherWords.not.be(o))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be &lt; (8)
     *                        ^
     * </pre>
     */
    infix def be[U](resultOfLessThanComparison: ResultOfLessThanComparison[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] =
      thisMatcherFactory.or(MatcherWords.not.be(resultOfLessThanComparison))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be &gt; (6)
     *                        ^
     * </pre>
     */
    infix def be[U](resultOfGreaterThanComparison: ResultOfGreaterThanComparison[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] =
      thisMatcherFactory.or(MatcherWords.not.be(resultOfGreaterThanComparison))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be &lt;= (2)
     *                        ^
     * </pre>
     */
    infix def be[U](resultOfLessThanOrEqualToComparison: ResultOfLessThanOrEqualToComparison[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] =
      thisMatcherFactory.or(MatcherWords.not.be(resultOfLessThanOrEqualToComparison))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be &gt;= (6)
     *                        ^
     * </pre>
     */
    infix def be[U](resultOfGreaterThanOrEqualToComparison: ResultOfGreaterThanOrEqualToComparison[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] =
      thisMatcherFactory.or(MatcherWords.not.be(resultOfGreaterThanOrEqualToComparison))

    /**
     * <strong>
     * The deprecation period for the "be ===" syntax has expired, and the syntax
     * will now throw <code>NotAllowedException</code>.  Please use should equal, should ===, shouldEqual,
     * should be, or shouldBe instead.
     * </strong>
     *
     * <p>
     * Note: usually syntax will be removed after its deprecation period. This was left in because otherwise the syntax could in some
     * cases still compile, but silently wouldn't work.
     * </p>
     */
    infix def be(tripleEqualsInvocation: TripleEqualsInvocation[_]): MatcherFactory$arity$[SC, $commaSeparatedTCNs$] =
      thisMatcherFactory.or(MatcherWords.not.be(tripleEqualsInvocation)(pos))

    // SKIP-SCALATESTJS,NATIVE-START
    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be ('empty)
     *                        ^
     * </pre>
     */
    infix def be(symbol: Symbol): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] = thisMatcherFactory.or(MatcherWords.not.be(symbol))
    // SKIP-SCALATESTJS,NATIVE-END

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>odd</code> is a <a href="BeMatcher.html"><code>BeMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be (odd)
     *                        ^
     * </pre>
     */
    infix def be[U](beMatcher: BeMatcher[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = thisMatcherFactory.or(MatcherWords.not.be(beMatcher))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>file</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be (file)
     *                        ^
     * </pre>
     */
    infix def be[U](bePropertyMatcher: BePropertyMatcher[U]): MatcherFactory$arity$[SC with AnyRef with U, $commaSeparatedTCNs$] = thisMatcherFactory.or(MatcherWords.not.be(bePropertyMatcher))

    // SKIP-SCALATESTJS,NATIVE-START
    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be a ('file)
     *                        ^
     * </pre>
     */
    infix def be(resultOfAWordApplication: ResultOfAWordToSymbolApplication): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] = thisMatcherFactory.or(MatcherWords.not.be(resultOfAWordApplication))
    // SKIP-SCALATESTJS,NATIVE-END

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>validMarks</code> is an <a href="AMatcher.html"><code>AMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be a (validMarks)
     *                        ^
     * </pre>
     */
    infix def be[U](resultOfAWordApplication: ResultOfAWordToAMatcherApplication[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = thisMatcherFactory.or(MatcherWords.not.be(resultOfAWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>file</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be a (file)
     *                        ^
     * </pre>
     */
    infix def be[U <: AnyRef](resultOfAWordApplication: ResultOfAWordToBePropertyMatcherApplication[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = thisMatcherFactory.or(MatcherWords.not.be(resultOfAWordApplication))

    // SKIP-SCALATESTJS,NATIVE-START
    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be an ('apple)
     *                        ^
     * </pre>
     */
    infix def be(resultOfAnWordApplication: ResultOfAnWordToSymbolApplication): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] = thisMatcherFactory.or(MatcherWords.not.be(resultOfAnWordApplication))
    // SKIP-SCALATESTJS,NATIVE-END

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>apple</code> is a <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be an (apple)
     *                        ^
     * </pre>
     */
    infix def be[U <: AnyRef](resultOfAnWordApplication: ResultOfAnWordToBePropertyMatcherApplication[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = thisMatcherFactory.or(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>, where <code>invalidMarks</code> is an <a href="AnMatcher.html"><code>AnMatcher</code></a>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory and not be an (invalidMarks)
     *                         ^
     * </pre>
     */
    infix def be[U](resultOfAnWordApplication: ResultOfAnWordToAnMatcherApplication[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = thisMatcherFactory.or(MatcherWords.not.be(resultOfAnWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be a [Book]
     *                        ^
     * </pre>
     */
    inline infix def be(aType: ResultOfATypeInvocation[_]): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] =
      \${ MatcherFactory$arity$.orNotATypeMatcherFactory$arity$[SC, $commaSeparatedTCNs$]('{thisOrNotWord: MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$]#OrNotWord}, '{aType}) }

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be an [Apple]
     *                        ^
     * </pre>
     */
    inline infix def be(anType: ResultOfAnTypeInvocation[_]): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] =
      \${ MatcherFactory$arity$.orNotAnTypeMatcherFactory$arity$[SC, $commaSeparatedTCNs$]('{thisOrNotWord: MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$]#OrNotWord}, '{anType}) }

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be theSameInstanceAs (string)
     *                        ^
     * </pre>
     */
    infix def be(resultOfTheSameInstanceAsApplication: ResultOfTheSameInstanceAsApplication): MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$] = thisMatcherFactory.or(MatcherWords.not.be(resultOfTheSameInstanceAsApplication))

    /**
     * This method enables the following syntax for the "primitive" numeric types:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be (17.0 +- 0.2)
     *                        ^
     * </pre>
     */
    infix def be[U](spread: Spread[U]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] = thisMatcherFactory.or(MatcherWords.not.be(spread))

    /**
     * This method enables the following syntax, where <code>fraction</code> is a <code>PartialFunction</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be definedAt (8)
     *                        ^
     * </pre>
     */
    infix def be[A, U <: PartialFunction[A, _]](resultOfDefinedAt: ResultOfDefinedAt[A]): MatcherFactory$arity$[SC with U, $commaSeparatedTCNs$] =
      thisMatcherFactory.or(MatcherWords.not.be(resultOfDefinedAt))

    /**
     * This method enables the following syntax, where <code>fraction</code> is a <code>PartialFunction</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be sorted
     *                        ^
     * </pre>
     */
    infix def be(sortedWord: SortedWord) =
      thisMatcherFactory.or(MatcherWords.not.be(sortedWord))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be readable
     *                        ^
     * </pre>
     */
    infix def be(readableWord: ReadableWord) =
      thisMatcherFactory.or(MatcherWords.not.be(readableWord))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be writable
     *                        ^
     * </pre>
     */
    infix def be(writableWord: WritableWord) =
      thisMatcherFactory.or(MatcherWords.not.be(writableWord))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be empty
     *                        ^
     * </pre>
     */
    infix def be(emptyWord: EmptyWord) =
      thisMatcherFactory.or(MatcherWords.not.be(emptyWord))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not be defined
     *                        ^
     * </pre>
     */
    infix def be(definedWord: DefinedWord) =
      thisMatcherFactory.or(MatcherWords.not.be(definedWord))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not fullyMatch regex (decimal)
     *                        ^
     * </pre>
     */
    infix def fullyMatch(resultOfRegexWordApplication: ResultOfRegexWordApplication): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] =
      thisMatcherFactory.or(MatcherWords.not.fullyMatch(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not include regex (decimal)
     *                        ^
     * </pre>
     */
    infix def include(resultOfRegexWordApplication: ResultOfRegexWordApplication): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] =
      thisMatcherFactory.or(MatcherWords.not.include(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not include ("1.7")
     *                        ^
     * </pre>
     */
    infix def include(expectedSubstring: String): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] =
      thisMatcherFactory.or(MatcherWords.not.include(expectedSubstring))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not startWith regex (decimal)
     *                        ^
     * </pre>
     */
    infix def startWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] =
      thisMatcherFactory.or(MatcherWords.not.startWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not startWith ("1.7")
     *                        ^
     * </pre>
     */
    infix def startWith(expectedSubstring: String): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] =
      thisMatcherFactory.or(MatcherWords.not.startWith(expectedSubstring))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not endWith regex (decimal)
     *                        ^
     * </pre>
     */
    infix def endWith(resultOfRegexWordApplication: ResultOfRegexWordApplication): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] =
      thisMatcherFactory.or(MatcherWords.not.endWith(resultOfRegexWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not endWith ("1.7")
     *                        ^
     * </pre>
     */
    infix def endWith(expectedSubstring: String): MatcherFactory$arity$[SC with String, $commaSeparatedTCNs$] =
      thisMatcherFactory.or(MatcherWords.not.endWith(expectedSubstring))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain (3)
     *                        ^
     * </pre>
     */
    infix def contain[U](expectedElement: U): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.or(MatcherWords.not.contain(expectedElement))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain key ("three")
     *                        ^
     * </pre>
     */
    infix def contain(resultOfKeyWordApplication: ResultOfKeyWordApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, KeyMapping] =
      thisMatcherFactory.or(MatcherWords.not.contain(resultOfKeyWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain value (3)
     *                        ^
     * </pre>
     */
    infix def contain(resultOfValueWordApplication: ResultOfValueWordApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, ValueMapping] =
      thisMatcherFactory.or(MatcherWords.not.contain(resultOfValueWordApplication))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain oneOf (8, 1, 2)
     *                        ^
     * </pre>
     */
    infix def contain(right: ResultOfOneOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain oneOf (8, 1, 2)
     *                        ^
     * </pre>
     */
    infix def contain(right: ResultOfOneElementOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain atLeastOneOf (8, 1, 2)
     *                        ^
     * </pre>
     */
    infix def contain(right: ResultOfAtLeastOneOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain atLeastOneElementOf (8, 1, 2)
     *                        ^
     * </pre>
     */
     infix def contain(right: ResultOfAtLeastOneElementOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
       thisMatcherFactory.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain noneOf (8, 1, 2)
     *                        ^
     * </pre>
     */
    infix def contain(right: ResultOfNoneOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain noElementsOf (8, 1, 2)
     *                        ^
     * </pre>
     */
    infix def contain(right: ResultOfNoElementsOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Containing] =
      thisMatcherFactory.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain theSameElementsAs (List(8, 1, 2))
     *                        ^
     * </pre>
     */
    infix def contain(right: ResultOfTheSameElementsAsApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain theSameElementsInOrderAs (List(8, 1, 2))
     *                        ^
     * </pre>
     */
    infix def contain(right: ResultOfTheSameElementsInOrderAsApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Sequencing] =
      thisMatcherFactory.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain only (8, 1, 2)
     *                        ^
     * </pre>
     */
    infix def contain(right: ResultOfOnlyApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain inOrderOnly (8, 1, 2))
     *                        ^
     * </pre>
     */
    infix def contain(right: ResultOfInOrderOnlyApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Sequencing] =
      thisMatcherFactory.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain allOf (8, 1, 2)
     *                        ^
     * </pre>
     */
    infix def contain(right: ResultOfAllOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain allOf (8, 1, 2)
     *                        ^
     * </pre>
     */
    infix def contain(right: ResultOfAllElementsOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain inOrder (8, 1, 2)
     *                        ^
     * </pre>
     */
    infix def contain(right: ResultOfInOrderApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Sequencing] =
      thisMatcherFactory.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain inOrderElementsOf (8, 1, 2)
     *                        ^
     * </pre>
     */
    infix def contain(right: ResultOfInOrderElementsOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Sequencing] =
      thisMatcherFactory.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain atMostOneOf (8, 1, 2)
     *                        ^
     * </pre>
     */
    infix def contain(right: ResultOfAtMostOneOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not contain atMostOneElementOf (List(8, 1, 2))
     *                        ^
     * </pre>
     */
    infix def contain(right: ResultOfAtMostOneElementOfApplication): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Aggregating] =
      thisMatcherFactory.or(MatcherWords.not.contain(right))

    /**
     * This method enables the following syntax:
     *
     * <pre class="stHighlight">
     * aMatcherFactory or not matchPattern { case Person("Bob", _) =>}
     *                        ^
     * </pre>
     */
    inline infix def matchPattern(inline right: PartialFunction[Any, _]) =
      \${ MatcherFactory$arity$.orNotMatchPattern('{thisOrNotWord: MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$]#OrNotWord}, '{right}) }
  }

  /**
   * This method enables the following syntax given a <code>MatcherFactory$arity$</code>:
   *
   * <pre class="stHighlight">
   * aMatcherFactory or not contain value (3)
   *                 ^
   * </pre>
   */
  infix def or(notWord: NotWord)(implicit prettifier: Prettifier, pos: source.Position): OrNotWord = new OrNotWord(prettifier, pos)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcherFactory or exist
   *                 ^
   * </pre>
   */
  infix def or(existWord: ExistWord): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Existence] =
    thisMatcherFactory.or(MatcherWords.exist.matcherFactory)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * aMatcherFactory or not (exist)
   *                 ^
   * </pre>
   */
  infix def or(notExist: ResultOfNotExist): MatcherFactory$arityPlusOne$[SC, $commaSeparatedTCNs$, Existence] =
    thisMatcherFactory.or(MatcherWords.not.exist)
}

/**
 * Companion object containing an implicit method that converts a <code>MatcherFactory$arity$</code> to a <code>Matcher</code>.
 *
 * @author Bill Venners
 */
object MatcherFactory$arity$ {

  import scala.language.implicitConversions
  import scala.quoted.*

  /**
   * Converts a <code>MatcherFactory$arity$</code> to a <code>Matcher</code>.
   *
   * @param matcherFactory a MatcherFactory$arity$ to convert
   * @return a Matcher produced by the passed MatcherFactory$arity$
   */
  implicit def produceMatcher[SC, $typeConstructors$, T <: SC : $colonSeparatedTCNs$](matcherFactory: MatcherFactory$arity$[SC, $commaSeparatedTCNs$]): Matcher[T] =
    matcherFactory.matcher

  /**
   * This method is called by macro that supports 'and not a [Type]' syntax.
   */
  def andNotATypeMatcherFactory$arity$[SC: Type, $typeConstructors$](self: Expr[MatcherFactory$arity$[SC & AnyRef, $commaSeparatedTCNs$]#AndNotWord], aType: Expr[ResultOfATypeInvocation[_]])(using Quotes, $typeConstructorsWithType$): Expr[MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$]] = {
    val rhs = TypeMatcherMacro.notATypeMatcher(aType)

    '{ (\$self).owner.and(\$rhs) }
  }

  /**
   * This method is called by macro that supports 'or not a [Type]' syntax.
   */
  def orNotATypeMatcherFactory$arity$[SC: Type, $typeConstructors$](self: Expr[MatcherFactory$arity$[SC & AnyRef, $commaSeparatedTCNs$]#OrNotWord], aType: Expr[ResultOfATypeInvocation[_]])(using Quotes, $typeConstructorsWithType$): Expr[MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$]] = {
    val rhs = TypeMatcherMacro.notATypeMatcher(aType)
    '{ (\$self).owner.or(\$rhs) }
  }

  /**
   * This method is called by macro that supports 'and not a [Type]' syntax.
   */
  def andNotAnTypeMatcherFactory$arity$[SC: Type, $typeConstructors$](self: Expr[MatcherFactory$arity$[SC & AnyRef, $commaSeparatedTCNs$]#AndNotWord], anType: Expr[ResultOfAnTypeInvocation[_]])(using Quotes, $typeConstructorsWithType$): Expr[MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$]] = {
    val rhs = TypeMatcherMacro.notAnTypeMatcher(anType)
    '{ (\$self).owner.and(\$rhs) }
  }

  /**
   * This method is called by macro that supports 'or not a [Type]' syntax.
   */
  def orNotAnTypeMatcherFactory$arity$[SC: Type, $typeConstructors$](self: Expr[MatcherFactory$arity$[SC & AnyRef, $commaSeparatedTCNs$]#OrNotWord], anType: Expr[ResultOfAnTypeInvocation[_]])(using Quotes, $typeConstructorsWithType$): Expr[MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$]] = {
    val rhs = TypeMatcherMacro.notAnTypeMatcher(anType)
    '{ (\$self).owner.or(\$rhs) }
  }

  def andNotMatchPattern[SC: Type, $typeConstructors$](self: Expr[MatcherFactory$arity$[SC & AnyRef, $commaSeparatedTCNs$]#AndNotWord], right: Expr[PartialFunction[Any, _]])(using Quotes, $typeConstructorsWithType$): Expr[MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$]] = {
    org.scalatest.matchers.MatchPatternMacro.checkCaseDefinitions(right)
    val notMatcher = '{ MatchPatternHelper.notMatchPatternMatcher(\$right) }
    '{ (\$self).owner.and(\$notMatcher) }
  }

  def orNotMatchPattern[SC: Type, $typeConstructors$](self: Expr[MatcherFactory$arity$[SC & AnyRef, $commaSeparatedTCNs$]#OrNotWord], right: Expr[PartialFunction[Any, _]])(using Quotes, $typeConstructorsWithType$): Expr[MatcherFactory$arity$[SC with AnyRef, $commaSeparatedTCNs$]] = {
    org.scalatest.matchers.MatchPatternMacro.checkCaseDefinitions(right)
    val notMatcher = '{ MatchPatternHelper.notMatchPatternMatcher(\$right) }
    '{ (\$self).owner.or(\$notMatcher) }
  }
}
                    """

// For some reason that I don't understand, I need to leave off the stars before the <pre> when
// they are next to ST commands. So I say  "   <pre>" sometimes instead of " * <pre>".

  val MaxArity = 9

  def main(args: Array[String]) {
    val targetDir = args(0)
    val version = args(1)
    val scalaVersion = args(2)
    val mainDir = new File(targetDir + "/main/scala/org/scalatest/matchers")
    mainDir.mkdirs()
    genMain(mainDir, version, scalaVersion)

/*
    val testDir = new File("gentests/" + targetDir + "/test/scala/org/scalatest/matchers")
    testDir.mkdirs()
    genTest(testDir, scalaVersion)
*/
  }

  def genMain(dir: File, version: String, scalaVersion: String): Seq[File] = {
    dir.mkdirs()
    for (arity <- 1 to MaxArity) yield {
      genMatcherFactory(dir, arity, false)
    }
  }

  def genMainJS(dir: File, version: String, scalaVersion: String): Seq[File] = {
    dir.mkdirs()
    for (arity <- 1 to MaxArity) yield {
      genMatcherFactory(dir, arity, true)
    }
  }

/*
  def genTest(dir: File, scalaVersion: String) {
    dir.mkdirs()
    genTableSuite(dir)
  }
*/

  def transform(content: String): String = {
    var skipMode = false
    content.split("\n").map { line =>
      if (line.trim == "// SKIP-SCALATESTJS,NATIVE-START")
        skipMode = true
      else if (line.trim == "// SKIP-SCALATESTJS,NATIVE-END")
        skipMode = false
      else if (!skipMode)
        line
      else
        ""
    }.mkString("\n")
  }

  def genMatcherFactory(targetDir: File, arity: Int, scalaJS: Boolean): File = {

    def setCommonOnes(arity: Int, st: org.antlr.stringtemplate.StringTemplate) {
      if (arity == 1)
        st.setAttribute("arityIsOne", "true");
      st.setAttribute("arity", arity);
      val typeConstructors = (1 to arity).map("TC" + _ + "[_]").mkString(", ")
      st.setAttribute("typeConstructors", typeConstructors);
      val typeConstructorsWithType = (1 to arity).map(i => s"Type[TC$i]").mkString(", ")
      st.setAttribute("typeConstructorsWithType", typeConstructorsWithType);
      val colonSeparatedTCNs = (1 to arity).map("TC" + _).mkString(" : ")
      st.setAttribute("colonSeparatedTCNs", colonSeparatedTCNs);
      val commaSeparatedTCNs = (1 to arity).map("TC" + _).mkString(", ")
      st.setAttribute("commaSeparatedTCNs", commaSeparatedTCNs);
    }

    val targetFile = new File(targetDir, "MatcherFactory" + arity + ".scala")
    if (!targetFile.exists || generatorSource.lastModified > targetFile.lastModified) {
      val bw = new BufferedWriter(new FileWriter(targetFile))

      try {

        val topSt = new org.antlr.stringtemplate.StringTemplate(topPart)
        setCommonOnes(arity, topSt)
        val nTypeclassInstances =
          if (arity == 1) "one typeclass instance"
          else {
            val numStr =
              arity match {
                case 2 => "two"
                case 3 => "three"
                case 4 => "four"
                case 5 => "five"
                case 6 => "six"
                case 7 => "seven"
                case 8 => "eight"
                case 9 => "nine"
              }
            numStr + " typeclass instances"
          }
        topSt.setAttribute("nTypeclassInstances", nTypeclassInstances)
        bw.write(transform(topSt.toString))

        // Now do the and/or methods that take matcher factories of various arities
        for (passedArity <- 1 to MaxArity - arity) {
          val resultArity = arity + passedArity
          val middleSt = new org.antlr.stringtemplate.StringTemplate(middlePart)
          setCommonOnes(arity, middleSt)
          middleSt.setAttribute("passedArity", passedArity);
          middleSt.setAttribute("resultArity", resultArity);
          val resultColonSeparatedTCNs = (1 to resultArity).map("TC" + _).mkString(" : ")
          middleSt.setAttribute("resultColonSeparatedTCNs", resultColonSeparatedTCNs);
          val resultCommaSeparatedTCNs = (1 to resultArity).map("TC" + _).mkString(", ")
          middleSt.setAttribute("resultCommaSeparatedTCNs", resultCommaSeparatedTCNs);
          val passedTypeConstructors = (arity + 1 to resultArity).map("TC" + _ + "[_]").mkString(", ")
          middleSt.setAttribute("passedTypeConstructors", passedTypeConstructors);
          val passedCommaSeparatedTCNs = (arity + 1 to resultArity).map("TC" + _).mkString(", ")
          middleSt.setAttribute("passedCommaSeparatedTCNs", passedCommaSeparatedTCNs);
          bw.write(transform(middleSt.toString))
        }

        // Just don't put the and/or DSL under MatcherFactory<MaxArity>, even though the ones that could
        // return another MatcherFactory<MaxArity> could be implemented. That would mean only *some* of the
        // DSL is implemented under MatcherFactory<MaxArity>. I'd rather just say none of it is.
        if (arity < MaxArity) {
          def doABottomHalf(bottomSt: org.antlr.stringtemplate.StringTemplate) {
            setCommonOnes(arity, bottomSt)
            bottomSt.setAttribute("arityPlusOne", arity + 1);
            bw.write(transform(bottomSt.toString))
          }
          doABottomHalf(new org.antlr.stringtemplate.StringTemplate(bottomPart1)) // Do in two halves, because hitting class file max string size limit
          doABottomHalf(new org.antlr.stringtemplate.StringTemplate(bottomPart2))
        }
        else {
          bw.write("}\n")
        }
      }
      finally {
        bw.close()
      }
    }
    targetFile
  }
}

