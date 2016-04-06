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

import org.scalactic.source.SourceInfo
import org.scalatest.matchers._
import org.scalactic._
import org.scalactic.TripleEqualsSupport.Spread
import org.scalatest.FailureMessages
import org.scalatest.Resources
import org.scalatest.UnquotedString
import org.scalatest.Suite
import org.scalatest.Assertions.areEqualComparingArraysStructurally
// SKIP-SCALATESTJS-START
import org.scalatest.MatchersHelper.matchSymbolToPredicateMethod
// SKIP-SCALATESTJS-END
import org.scalatest.enablers.Sequencing
import org.scalatest.enablers.Sortable
import org.scalatest.enablers.Readability
import org.scalatest.enablers.Writability
import org.scalatest.enablers.Emptiness
import org.scalatest.enablers.Definition
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> or <a href="../MustMatchers.html"><code>MustMatchers</code></a> for an overview of
 * the matchers DSL.
 *
 * <p>
 * Class <code>BeWord</code> contains an <code>apply</code> method that takes a <code>Symbol</code>, which uses reflection
 * to find and access a <code>Boolean</code> property and determine if it is <code>true</code>.
 * If the symbol passed is <code>'empty</code>, for example, the <code>apply</code> method
 * will use reflection to look for a public Java field named
 * "empty", a public method named "empty", or a public method named "isEmpty". If a field, it must be of type <code>Boolean</code>.
 * If a method, it must take no parameters and return <code>Boolean</code>. If multiple candidates are found,
 * the <code>apply</code> method will select based on the following algorithm:
 * </p>
 * 
 * <table class="stTable">
 * <tr><th class="stHeadingCell">Field</th><th class="stHeadingCell">Method</th><th class="stHeadingCell">"is" Method</th><th class="stHeadingCell">Result</th></tr>
 * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Throws <code>TestFailedException</code>, because no candidates found</td></tr>
 * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>isEmpty()</code></td></tr>
 * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Invokes <code>empty()</code></td></tr>
 * <tr><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>empty()</code> (this can occur when <code>BeanProperty</code> annotation is used)</td></tr>
 * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Accesses field <code>empty</code></td></tr>
 * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>isEmpty()</code></td></tr>
 * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell">&nbsp;</td><td class="stTableCell">Invokes <code>empty()</code></td></tr>
 * <tr><td class="stTableCell"><code>empty</code></td><td class="stTableCell"><code>empty()</code></td><td class="stTableCell"><code>isEmpty()</code></td><td class="stTableCell">Invokes <code>empty()</code> (this can occur when <code>BeanProperty</code> annotation is used)</td></tr>
 * </table>
 * 
 * @author Bill Venners
 */
final class BeWord {


  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should be &lt; (7)
   *                  ^
   * </pre>
   *
   * <p>
   * Note that the less than operator will be invoked on <code>be</code> in this expression, not
   * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
   * in the matchers DSL, because the less than operator has a higher precedence than <code>should</code>.
   * Thus in the above case the first expression evaluated will be <code>be &lt; (7)</code>, which results
   * in a matcher that is passed to <code>should</code>.
   * </p>
   *
   * <p>
   * This method also enables the following syntax:
   * </p>
   *
   * <pre class="stHighlight">
   * result should not (be &lt; (7))
   *                       ^
   * </pre>
   */
  def <[T : Ordering](right: T): Matcher[T] =
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        val ordering = implicitly[Ordering[T]]
        MatchResult(
          ordering.lt(left, right), // left < right
          Resources.rawWasNotLessThan,
          Resources.rawWasLessThan,
          Vector(left, right)
        )
      }
      override def toString: String = "be < " + Prettifier.default(right)
    }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should be &gt; (7)
   *                  ^
   * </pre>
   *
   * <p>
   * Note that the greater than operator will be invoked on <code>be</code> in this expression, not
   * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
   * in the matchers DSL, because the greater than operator has a higher precedence than <code>should</code>.
   * Thus in the above case the first expression evaluated will be <code>be &gt; (7)</code>, which results
   * in a matcher that is passed to <code>should</code>.
   * </p>
   *
   * <p>
   * This method also enables the following syntax:
   * </p>
   *
   * <pre class="stHighlight">
   * result should not (be &gt; (7))
   *                       ^
   * </pre>
   */
  def >[T : Ordering](right: T): Matcher[T] =
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        val ordering = implicitly[Ordering[T]]
        MatchResult(
          ordering.gt(left, right), // left > right
          Resources.rawWasNotGreaterThan,
          Resources.rawWasGreaterThan,
          Vector(left, right)
        )
      }
      override def toString: String = "be > " + Prettifier.default(right)
    }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should be &lt;= (7)
   *                  ^
   * </pre>
   *
   * <p>
   * Note that the less than or equal to operator will be invoked on <code>be</code> in this expression, not
   * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
   * in the matchers DSL, because the less than or equal to operator has a higher precedence than <code>should</code>.
   * Thus in the above case the first expression evaluated will be <code>be &lt;= (7)</code>, which results
   * in a matcher that is passed to <code>should</code>.
   * </p>
   *
   * <p>
   * This method also enables the following syntax:
   * </p>
   *
   * <pre class="stHighlight">
   * result should not (be &lt;= (7))
   *                       ^
   * </pre>
   */
  def <=[T : Ordering](right: T): Matcher[T] =
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        val ordering = implicitly[Ordering[T]]
        MatchResult(
          ordering.lteq(left, right), // left <= right
          Resources.rawWasNotLessThanOrEqualTo,
          Resources.rawWasLessThanOrEqualTo,
          Vector(left, right)
        )
      }
      override def toString: String = "be <= " + Prettifier.default(right)
    }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should be &gt;= (7)
   *                  ^
   * </pre>
   *
   * <p>
   * Note that the greater than or equal to operator will be invoked on <code>be</code> in this expression, not
   * on a result of passing <code>be</code> to <code>should</code>, as with most other operators
   * in the matchers DSL, because the greater than or equal to operator has a higher precedence than <code>should</code>.
   * Thus in the above case the first expression evaluated will be <code>be &gt;= (7)</code>, which results
   * in a matcher that is passed to <code>should</code>.
   * </p>
   *
   * <p>
   * This method also enables the following syntax:
   * </p>
   *
   * <pre class="stHighlight">
   * result should not (be &gt;= (7))
   *                       ^
   * </pre>
   */
  def >=[T : Ordering](right: T): Matcher[T] =
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        val ordering = implicitly[Ordering[T]]
        MatchResult(
          ordering.gteq(left, right), // left >= right
          Resources.rawWasNotGreaterThanOrEqualTo,
          Resources.rawWasGreaterThanOrEqualTo,
          Vector(left, right)
        )
      }
      override def toString: String = "be >= " + Prettifier.default(right)
    }

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
  @deprecated("The deprecation period for the be === syntax has expired. Please use should equal, should ===, shouldEqual, should be, or shouldBe instead.")
  def ===(right: Any): Matcher[Any] = {
    throw new NotAllowedException(FailureMessages.beTripleEqualsNotAllowed,
                                  getStackDepthFun("BeWord.scala", "$eq$eq$eq"))
  }

  // SKIP-SCALATESTJS-START
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * fileMock should not { be a ('file) }
   *                          ^
   * </pre>
   */
  def a(right: Symbol)(implicit prettifier: Prettifier, sourceInfo: SourceInfo): Matcher[AnyRef] =
    new Matcher[AnyRef] {
      def apply(left: AnyRef): MatchResult = matchSymbolToPredicateMethod(left, right, true, true, prettifier, sourceInfo)
      override def toString: String = "be a " + prettifier(right)
    }
  // SKIP-SCALATESTJS-END

  /**
   * This method enables the following syntax, where <code>fileMock</code> is, for example, of type <code>File</code> and
   * <code>file</code> refers to a <code>BePropertyMatcher[File]</code>:
   *
   * <pre class="stHighlight">
   * fileMock should not { be a (file) }
   *                          ^
   * </pre>
   */
  def a[S <: AnyRef](bePropertyMatcher: BePropertyMatcher[S]): Matcher[S] =
    new Matcher[S] {
      def apply(left: S): MatchResult = {
        val result = bePropertyMatcher(left)
        MatchResult(
          result.matches,
          Resources.rawWasNotA,
          Resources.rawWasA,
          Vector(left, UnquotedString(result.propertyName))
        )
      }
      override def toString: String = "be a " + Prettifier.default(bePropertyMatcher)
    }
  
  /**
   * This method enables the following syntax, where <code>negativeNumber</code> is, for example, of type <code>AMatcher</code>:
   *
   * <pre class="stHighlight">
   * 8 should not { be a (negativeNumber) }
   *                   ^
   * </pre>
   */
  def a[S](aMatcher: AMatcher[S]): Matcher[S] = 
    new Matcher[S] {
      def apply(left: S): MatchResult = aMatcher(left)
      override def toString: String = "be a " + Prettifier.default(aMatcher)
    }

  // SKIP-SCALATESTJS-START
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * animal should not { be an ('elephant) }
   *                        ^
   * </pre>
   */
  def an(right: Symbol)(implicit prettifier: Prettifier, sourceInfo: SourceInfo): Matcher[AnyRef] =
    new Matcher[AnyRef] {
      def apply(left: AnyRef): MatchResult = matchSymbolToPredicateMethod(left, right, true, false, prettifier, sourceInfo)
      override def toString: String = "be an " + Prettifier.default(right)
    }
  // SKIP-SCALATESTJS-END

  /**
   * This method enables the following syntax, where <code>keyEvent</code> is, for example, of type <code>KeyEvent</code> and
   * <code>actionKey</code> refers to a <code>BePropertyMatcher[KeyEvent]</code>:
   *
   * <pre class="stHighlight">
   * keyEvent should not { be an (actionKey) }
   *                          ^
   * </pre>
   */
  def an[S <: AnyRef](bePropertyMatcher: BePropertyMatcher[S]): Matcher[S] =
    new Matcher[S] {
      def apply(left: S): MatchResult = {
        val result = bePropertyMatcher(left)
        MatchResult(
          result.matches,
          Resources.rawWasNotAn,
          Resources.rawWasAn,
          Vector(left, UnquotedString(result.propertyName))
        )
      }
      override def toString: String = "be an " + Prettifier.default(bePropertyMatcher)
    }
  
  /**
   * This method enables the following syntax, where <code>oddNumber</code> is, for example, of type <code>AnMatcher</code>:
   *
   * <pre class="stHighlight">
   * 8 should not { be an (oddNumber) }
   *                   ^
   * </pre>
   */
  def an[S](anMatcher: AnMatcher[S]): Matcher[S] = 
    new Matcher[S] {
      def apply(left: S): MatchResult = anMatcher(left)
      override def toString: String = "be an " + Prettifier.default(anMatcher)
    }

  /**
   * This method enables the following syntax for the "primitive" numeric types: 
   *
   * <pre class="stHighlight">
   * sevenDotOh should be (7.1 +- 0.2)
   *                      ^
   * </pre>
   */
  def apply[U](spread: Spread[U]): Matcher[U] =
    new Matcher[U] {
      def apply(left: U): MatchResult = {
        MatchResult(
          spread.isWithin(left),
          Resources.rawWasNotPlusOrMinus,
          Resources.rawWasPlusOrMinus,
          Vector(left, spread.pivot, spread.tolerance)
        )
      }
      override def toString: String = "be (" + Prettifier.default(spread) + ")"
    }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should be theSameInstancreAs (anotherObject)
   *                  ^
   * </pre>
   */
  def theSameInstanceAs(right: AnyRef): Matcher[AnyRef] =
    new Matcher[AnyRef] {
      def apply(left: AnyRef): MatchResult =
        MatchResult(
          left eq right,
          Resources.rawWasNotSameInstanceAs,
          Resources.rawWasSameInstanceAs,
          Vector(left, right)
        )
      override def toString: String = "be theSameInstanceAs " + Prettifier.default(right)
    }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should be (true)
   *                  ^
   * </pre>
   */
  def apply(right: Boolean): Matcher[Boolean] = 
    new Matcher[Boolean] {
      def apply(left: Boolean): MatchResult =
        MatchResult(
          left == right,
          Resources.rawWasNot,
          Resources.rawWas,
          Vector(left, right)
        )
      override def toString: String = "be (" + Prettifier.default(right) + ")"
    }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should be (null)
   *                  ^
   * </pre>
   */
  def apply(o: Null): Matcher[AnyRef] = 
    new Matcher[AnyRef] {
      def apply(left: AnyRef): MatchResult = {
        MatchResult(
          left == null,
          Resources.rawWasNotNull,
          Resources.rawWasNull,
          Resources.rawWasNotNull,
          Resources.rawMidSentenceWasNull,
          Vector(left), 
          Vector.empty
        )
      }
      override def toString: String = "be (null)"
    }

  /* *
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * set should be ('empty)
   *               ^
   * </pre>
  def apply[T](right: AType[T]): Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = 
        MatchResult(
          right.isAssignableFromClassOf(left),
          FailureMessages.wasNotAnInstanceOf(left, UnquotedString(right.className), UnquotedString(left.getClass.getName)),
          FailureMessages.wasAnInstanceOf, // TODO, missing the left, right.className here. Write a test and fix it.
          FailureMessages.wasNotAnInstanceOf(left, UnquotedString(right.className), UnquotedString(left.getClass.getName)),
          FailureMessages.wasAnInstanceOf
        )
    }
   */

  // SKIP-SCALATESTJS-START
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * set should be ('empty)
   *               ^
   * </pre>
   */
  def apply(right: Symbol)(implicit prettifier: Prettifier, sourceInfo: SourceInfo): Matcher[AnyRef] =
    new Matcher[AnyRef] {
      def apply(left: AnyRef): MatchResult = matchSymbolToPredicateMethod(left, right, false, false, prettifier, sourceInfo)
      override def toString: String = "be (" + Prettifier.default(right) + ")"
    }
  // SKIP-SCALATESTJS-END

  /**
   * This method enables the following syntax, where <code>num</code> is, for example, of type <code>Int</code> and
   * <code>odd</code> refers to a <code>BeMatcher[Int]</code>:
   *
   * <pre class="stHighlight">
   * num should be (odd)
   *               ^
   * </pre>
   */
  def apply[T](right: BeMatcher[T]): Matcher[T] =
    new Matcher[T] {
      def apply(left: T): MatchResult = right(left)
      override def toString: String = "be (" + Prettifier.default(right) + ")"
    }

  /**
   * This method enables the following syntax, where <code>open</code> refers to a <code>BePropertyMatcher</code>:
   *
   * <pre class="stHighlight">
   * door should be (open)
   *                ^
   * </pre>
   */
  def apply[T](bePropertyMatcher: BePropertyMatcher[T]): Matcher[T] =
    new Matcher[T] {
      def apply(left: T): MatchResult = {
        val result = bePropertyMatcher(left)
        MatchResult(
          result.matches,
          Resources.rawWasNot,
          Resources.rawWas,
          Vector(left, UnquotedString(result.propertyName))
        )
      }
      override def toString: String = "be (" + Prettifier.default(bePropertyMatcher) + ")"
    }

  /**
   * This method enables <code>be</code> to be used for equality comparison. Here are some examples: 
   *
   * <pre class="stHighlight">
   * result should be (None)
   *                  ^
   * result should be (Some(1))
   *                  ^
   * result should be (true)
   *                  ^
   * result should be (false)
   *                  ^
   * sum should be (19)
   *               ^
   * </pre>
   */
  def apply(right: Any): Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right) // TODO: To move this to reporter
        MatchResult(
          areEqualComparingArraysStructurally(left, right),
          Resources.rawWasNotEqualTo,
          Resources.rawWasEqualTo,
          Vector(leftee, rightee), 
          Vector(left, right)
        )
      }
      override def toString: String = "be (" + Prettifier.default(right) + ")"
    }
  
  /**
   * This method enables the following syntax, where <code>open</code> refers to a <code>BePropertyMatcher</code>:
   *
   * <pre class="stHighlight">
   * List(1, 2, 3) should be (sorted)
   *                          ^
   * </pre>
   */
  def apply(right: SortedWord): MatcherFactory1[Any, Sortable] = 
    new MatcherFactory1[Any, Sortable] {
      def matcher[T <: Any : Sortable]: Matcher[T] = 
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            val sortable = implicitly[Sortable[T]]
            MatchResult(
              sortable.isSorted(left), 
              Resources.rawWasNotSorted,
              Resources.rawWasSorted,
              Vector(left)
            )
          }
          override def toString: String = "be (sorted)"
        }
      override def toString: String = "be (sorted)"
    }
  
  /**
   * This method enables the following syntax, where <code>fraction</code> refers to a <code>PartialFunction</code>:
   *
   * <pre class="stHighlight">
   * fraction should (be definedAt (6) and be definedAt (8))
   *                     ^
   * </pre>
   */
  def definedAt[A, U <: PartialFunction[A, _]](right: A): Matcher[U] = 
    new Matcher[U] {
      def apply(left: U): MatchResult =
        MatchResult(
          left.isDefinedAt(right),
          Resources.rawWasNotDefinedAt,
          Resources.rawWasDefinedAt,
          Vector(left, right)
        )
      override def toString: String = "be definedAt " + Prettifier.default(right)
    }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * a[Exception] should (be thrownBy { "hi".charAt(-1) })
   *                         ^
   * </pre>
   */
  def thrownBy(code: => Unit) = new ResultOfBeThrownBy(Vector(() => code))
  
  /**
   * This method enables the following syntax, where <code>fraction</code> refers to a <code>PartialFunction</code>:
   *
   * <pre class="stHighlight">
   * fraction should (be (definedAt (6)) and be (definedAt (8)))
   *                  ^
   * </pre>
   */
  def apply[A, U <: PartialFunction[A, _]](resultOfDefinedAt: ResultOfDefinedAt[A]): Matcher[U] =
    new Matcher[U] {
      def apply(left: U): MatchResult =
        MatchResult(
          left.isDefinedAt(resultOfDefinedAt.right),
          Resources.rawWasNotDefinedAt,
          Resources.rawWasDefinedAt,
          Vector(left, resultOfDefinedAt.right)
        )
      override def toString: String = "be definedAt " + Prettifier.default(resultOfDefinedAt.right)
    }

  import language.experimental.macros
  
  /**
   * This method enables the following syntax, where <code>open</code> refers to a <code>BePropertyMatcher</code>:
   *
   * <pre class="stHighlight">
   * result should be (a [Book])
   *               ^
   * </pre>
   */
  def apply(aType: ResultOfATypeInvocation[_]): Matcher[Any] = macro TypeMatcherMacro.aTypeMatcherImpl
  
  /**
   * This method enables the following syntax, where <code>open</code> refers to a <code>BePropertyMatcher</code>:
   *
   * <pre class="stHighlight">
   * result should be (an [Book])
   *               ^
   * </pre>
   */
  def apply(anType: ResultOfAnTypeInvocation[_]): Matcher[Any] = macro TypeMatcherMacro.anTypeMatcherImpl
  
  /**
   * This method enables the following syntax, where <code>open</code> refers to a <code>BePropertyMatcher</code>:
   *
   * <pre class="stHighlight">
   * file should be (readable)
   *                ^
   * </pre>
   */
  def apply(readable: ReadableWord): MatcherFactory1[Any, Readability] = 
    new MatcherFactory1[Any, Readability] {
      def matcher[T <: Any : Readability]: Matcher[T] = 
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            val readability = implicitly[Readability[T]]
            MatchResult(
              readability.isReadable(left), 
              Resources.rawWasNotReadable,
              Resources.rawWasReadable,
              Vector(left)
            )
          }
          override def toString: String = "be (" + Prettifier.default(readable) + ")"
        }
      override def toString: String = "be (" + Prettifier.default(readable) + ")"
    }
  
  /**
   * This method enables the following syntax, where <code>open</code> refers to a <code>BePropertyMatcher</code>:
   *
   * <pre class="stHighlight">
   * file should be (writable)
   *                 ^
   * </pre>
   */
  def apply(writable: WritableWord): MatcherFactory1[Any, Writability] = 
    new MatcherFactory1[Any, Writability] {
      def matcher[T <: Any : Writability]: Matcher[T] = 
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            val writability = implicitly[Writability[T]]
            MatchResult(
              writability.isWritable(left), 
              Resources.rawWasNotWritable,
              Resources.rawWasWritable,
              Vector(left)
            )
          }
          override def toString: String = "be (writable)"
        }
      override def toString: String = "be (writable)"
    }
  
  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * array should be (empty)
   *                 ^
   * </pre>
   */
  def apply(empty: EmptyWord): MatcherFactory1[Any, Emptiness] = 
    new MatcherFactory1[Any, Emptiness] {
      def matcher[T <: Any : Emptiness]: Matcher[T] = 
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            val emptiness = implicitly[Emptiness[T]]
            MatchResult(
              emptiness.isEmpty(left), 
              Resources.rawWasNotEmpty,
              Resources.rawWasEmpty,
              Vector(left)
            )
          }
          override def toString: String = "be (empty)"
        }
      override def toString: String = "be (empty)"
    }
  
  /**
   * This method enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * array should be (defined)
   *                 ^
   * </pre>
   */
  def apply(defined: DefinedWord): MatcherFactory1[Any, Definition] = 
    new MatcherFactory1[Any, Definition] {
      def matcher[T <: Any : Definition]: Matcher[T] = 
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            val definition = implicitly[Definition[T]]
            MatchResult(
              definition.isDefined(left), 
              Resources.rawWasNotDefined,
              Resources.rawWasDefined,
              Vector(left)
            )
          }
          override def toString: String = "be (defined)"
        }
      override def toString: String = "be (defined)"
    }
  
  /**
   * Overrides toString to return "be"
   */
  override def toString: String = "be"
}
