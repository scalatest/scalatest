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

import org.scalatest.Resources
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatcherFactory1
import org.scalatest.matchers.MatcherFactory2
import org.scalatest.matchers.MatchResult
import org.scalatest.enablers.Existence

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 * @author Chee Seng
 */
final class ExistWord {
  
  private[scalatest] val matcherFactory: MatcherFactory1[Any, Existence] = 
    new MatcherFactory1[Any, Existence] {
      def matcher[T <: Any : Existence]: Matcher[T] = 
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            val existence = implicitly[Existence[T]]
            MatchResult(
              existence.exists(left), 
              Resources("doesNotExist"), 
              Resources("exists"), 
              Vector(left)
            )
          }
          override def toString: String = "exist"
        }
      override def toString: String = "exist"
    }
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * file should (exist and equal (existFile))
   *                    ^
   * </pre>
   */
  def and[TYPECLASS1[_]](anotherMatcherFactory: MatcherFactory1[Any, TYPECLASS1]): MatcherFactory2[Any, Existence, TYPECLASS1] = 
    matcherFactory.and(anotherMatcherFactory)
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * file should (exist and be (existFile))
   *                    ^
   * </pre>
   */
  def and(anotherMatcher: Matcher[Any]): MatcherFactory1[Any, Existence] = 
    matcherFactory.and(anotherMatcher)
    
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * file should (exist or equal (existFile))
   *                    ^
   * </pre>
   */
  def or[TYPECLASS1[_]](anotherMatcherFactory: MatcherFactory1[Any, TYPECLASS1]): MatcherFactory2[Any, Existence, TYPECLASS1] = 
    matcherFactory.or(anotherMatcherFactory)
    
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * file should (exist or be (existFile))
   *                    ^
   * </pre>
   */
  def or(anotherMatcher: Matcher[Any]): MatcherFactory1[Any, Existence] = 
    matcherFactory.or(anotherMatcher)
}