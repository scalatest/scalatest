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
package org.scalautils

/**
 * Trait containing an implicit conversion that adds an <code>asAny</code> method to
 * anything, which returns the same object as type <code>Any</code>.
 *
 * <p>
 * The purpose of this method is to appease the type checker when necessary. For example,
 * in ScalaTest's matchers DSL the type passed to <code>contain</code> must be consistent
 * with the element type of the collection on which <code>should</code> is invoked. So
 * this type checks:
 * </p>
 *
 * <pre>
 * Set(1, 2) should contain (2)
 * </pre>
 * 
 * <p>
 * But this does not type check:
 * </p>
 * 
 * <pre>
 * Set(1, 2) should contain ("2")
 * </pre>
 * 
 * <p>
 * That is all well and good, but it turns out that this does also not type check, because the element type of
 * the collection (<code>Any</code>) is a supertype of the type passed to contain (<code>String</code>):
 * </p>
 * 
 * <pre>
 * Set(1, "2") should contain ("2") // Does not compile
 * </pre>
 * 
 * <p>
 * You can appease the type checker by casting the type of <code>"2"</code> to <code>Any</code>, a cast that
 * will always succeed. Using <code>asAny</code> makes this prettier:
 * </p>
 * 
 * <pre>
 * Set(1, "2") should contain ("2".asAny)
 * </pre>
 * 
 */
trait AsAny {

  /**
   * Wrapper class with an <code>asAny</code> method that returns the passed object
   * as type <code>Any</code>.
   *
   * @param o the object to return from <code>asAny</code>
   *
   * @author Bill Venners
   */
  class AsAnyWrapper(o: Any) {

    /**
     * Returns the object, <code>o</code>, passed to the constructor.
     *
     * @return the object passed to the constructor
     */
    def asAny: Any = o
  }

  /**
   * Implicit conversion that adds an <code>asAny</code> method to an object, which returns
   * the exact same object but as type <code>Any</code>.
   */
  implicit def convertToAsAnyWrapper(o: Any): AsAnyWrapper = new AsAnyWrapper(o)
}

/**
 * Companion object to trait <code>AsAny</code> that facilitates the importing of <code>AsAny</code> members as 
 * an alternative to mixing it in. One use case is to import <code>AsAny</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $ scala -classpath scalatest.jar
 * Welcome to Scala version 2.10.0
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 *
 * scala&gt; import org.scalatest._
 * import org.scalatest._
 *
 * scala&gt; import Matchers._
 * import Matchers._
 *
 * scala&gt; Set(1, "2") should contain (1)
 * <console>:14: error: overloaded method value should with alternatives:
 * [R](inv: org.scalautils.TripleEqualsInvocation[R])(implicit constraint: org.scalautils.EqualityConstraint[scala.collection.immutable.Set[Any],R])Unit <and>
 * (notWord: org.scalatest.Matchers.NotWord)org.scalatest.Matchers.ResultOfNotWordForTraversable[Any,scala.collection.immutable.Set] <and>
 * (beWord: org.scalatest.Matchers.BeWord)org.scalatest.Matchers.ResultOfBeWordForAnyRef[scala.collection.GenTraversable[Any]] <and>
 * (containMatcher: org.scalatest.ContainMatcher[Any])Unit <and>
 * (containWord: org.scalatest.Matchers.ContainWord)org.scalatest.Matchers.ResultOfContainWordForTraversable[Any] <and>
 * (haveWord: org.scalatest.Matchers.HaveWord)org.scalatest.Matchers.ResultOfHaveWordForTraversable[Any] <and>
 * (rightMatcherGen1: org.scalatest.Matchers.MatcherGen1[scala.collection.immutable.Set[Any],org.scalautils.Equality])(implicit equality: org.scalautils.Equality[scala.collection.immutable.Set[Any]])Unit <and>
 * (rightMatcherX6: org.scalatest.matchers.Matcher[scala.collection.GenTraversable[Any]])Unit
 *cannot be applied to (org.scalatest.matchers.Matcher[scala.collection.GenTraversable[Int]])
 *             Set(1, "2") should contain (1)
 *                         ^
 *
 * scala&gt; Set(1, "2") should contain (1.asAny)
 *
 * scala&gt;
 * </pre>
 */
object AsAny extends AsAny

