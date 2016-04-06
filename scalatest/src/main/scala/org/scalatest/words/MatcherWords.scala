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
import org.scalactic.{Equality, Prettifier}
import org.scalatest.Resources
import org.scalatest.Suite
import org.scalatest.Assertions.areEqualComparingArraysStructurally

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
trait MatcherWords {

  /**
   * This field enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * string should (fullyMatch regex ("Hel*o, wor.d") and not have length (99))
   *                ^
   * </pre>
   */
  val fullyMatch = new FullyMatchWord

  /**
   * This field enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * string should (startWith ("Four") and include ("year"))
   *                ^
   * </pre>
   */
  val startWith = new StartWithWord

  /**
   * This field enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * string should (endWith ("ago") and include ("score"))
   *                ^
   * </pre>
   */
  val endWith = new EndWithWord

  /**
   * This field enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * string should (include ("hope") and not startWith ("no"))
   *                ^
   * </pre>
   */
  val include = new IncludeWord

/*
    In HaveWord's methods key, value, length, and size, I can give type parameters.
    The type HaveWord can contain a key method that takes a S or what not, and returns a matcher, which
    stores the key value in a val and whose apply method checks the passed map for the remembered key. This one would be used in things like:

    map should { have key 9 and have value "bob" }

    There's an overloaded should method on Shouldifier that takes a HaveWord. This method results in
    a different type that also has a key method that takes an S. So when you say:

    map should have key 9

    what happens is that this alternate should method gets invoked. The result is this other class that
    has a key method, and its constructor takes the map and stores it in a val. So this time when key is
    invoked, it checks to make sure the passed key is in the remembered map, and does the assertion.

    length and size can probably use structural types, because I want to use length on string and array for
    starters, and other people may create classes that have length methods. Would be nice to be able to use them.
  */

  /**
   * This field enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * list should (have length (3) and not contain ('a'))
   *              ^
   * </pre>
   */
  val have = new HaveWord

  /**
   * This field enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * obj should (be theSameInstanceAs (string) and be theSameInstanceAs (string))
   *             ^
   * </pre>
   */
  val be = new BeWord

  /**
   * This field enables syntax such as the following:
   *
   * <pre class="stHighlight">
   * list should (contain ('a') and have length (7))
   *              ^
   * </pre>
   */
  val contain = new ContainWord

  /**
   * This field enables syntax like the following: 
   *
   * <pre class="stHighlight">
   * myFile should (not be an (directory) and not have ('name ("foo.bar")))
   *                ^
   * </pre>
   */
  val not = new NotWord
  
  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * "hi" should not have length (3)
   *                      ^
   * </pre>
   */
  val length = new LengthWord
  
  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * set should not have size (3)
   *                     ^
   * </pre>
   */
  val size = new SizeWord
  
  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * seq should be (sorted)
   *               ^
   * </pre>
   */
  val sorted = new SortedWord

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * seq should be (defined)
   *               ^
   * </pre>
   */
  val defined = new DefinedWord
  
  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * noException should be thrownBy
   * ^
   * </pre>
   */
  def noException(implicit prettiifier: Prettifier, sourceInfo: SourceInfo) = new NoExceptionWord(prettiifier, sourceInfo)
  
  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * file should exist
   *             ^
   * </pre>
   */
  val exist = new ExistWord

  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * 
   * file should be (readable)
   *                 ^
   * </pre>
   */
  val readable = new ReadableWord
  
  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * file should be (writable)
   *                 ^
   * </pre>
   */
  val writable = new WritableWord
  
  /**
   * This field enables the following syntax: 
   *
   * <pre class="stHighlight">
   * 
   * list should be (empty)
   *                 ^
   * </pre>
   */
  val empty = new EmptyWord

  /**
   * This field enables the following syntax:
   *
   * <pre class="stHighlight">
   *
   * "val a: String = 1" shouldNot compile
   *                               ^
   * </pre>
   */
  val compile = new CompileWord

  /**
   * This field enables the following syntax:
   *
   * <pre class="stHighlight">
   *
   * "val a: String = 1" shouldNot typeCheck
   *                               ^
   * </pre>
   */
  val typeCheck = new TypeCheckWord

  /**
   * This field enables the following syntax:
   *
   * <pre class="stHighlight">
   *
   * result should matchPattern { case Person("Bob", _) => }
   *               ^
   * </pre>
   */
  val matchPattern = new MatchPatternWord

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * result should equal (7)
   *               ^
   * </pre>
   *
   * <p>
   * The <code>left should equal (right)</code> syntax works by calling <code>==</code> on the <code>left</code>
   * value, passing in the <code>right</code> value, on every type except arrays. If both <code>left</code> and right are arrays, <code>deep</code>
   * will be invoked on both <code>left</code> and <code>right</code> before comparing them with <em>==</em>. Thus, even though this expression
   * will yield false, because <code>Array</code>'s <code>equals</code> method compares object identity:
   * </p>
   * 
   * <pre class="stHighlight">
   * Array(1, 2) == Array(1, 2) // yields false
   * </pre>
   *
   * <p>
   * The following expression will <em>not</em> result in a <code>TestFailedException</code>, because ScalaTest will compare
   * the two arrays structurally, taking into consideration the equality of the array's contents:
   * </p>
   *
   * <pre class="stHighlight">
   * Array(1, 2) should equal (Array(1, 2)) // succeeds (i.e., does not throw TestFailedException)
   * </pre>
   *
   * <p>
   * If you ever do want to verify that two arrays are actually the same object (have the same identity), you can use the
   * <code>be theSameInstanceAs</code> syntax.
   * </p>
   *
   */
  def equal(right: Any): MatcherFactory1[Any, Equality] =
    new MatcherFactory1[Any, Equality] {
      def matcher[T <: Any : Equality]: Matcher[T] = {
        val equality = implicitly[Equality[T]]
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right) // TODO: to move this code to reporters
            MatchResult(
              equality.areEqual(left, right),
              Resources.rawDidNotEqual,
              Resources.rawEqualed,
              Vector(leftee, rightee), 
              Vector(left, right)
            )
          }
          override def toString: String = "equal (" + Prettifier.default(right) + ")"
        }
      }
      override def toString: String = "equal (" + Prettifier.default(right) + ")"
    }
}

object MatcherWords extends MatcherWords
