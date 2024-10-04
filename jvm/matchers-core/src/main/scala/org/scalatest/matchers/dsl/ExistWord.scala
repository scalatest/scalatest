/*
 * Copyright 2001-2024 Artima, Inc.
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

import org.scalatest.enablers.Existence
import org.scalatest.matchers.Matcher
import org.scalatest.Resources
import org.scalatest.matchers.MatchResult

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
              Resources.rawDoesNotExist,
              Resources.rawExists,
              Vector(left)
            )
          }
          override def toString: String = "exist"
        }
      override def toString: String = "exist"
    }

  import scala.language.higherKinds
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * file should (exist and equal (existFile))
   *                    ^
   * </pre>
   */
  //DOTTY-ONLY infix def and[TYPECLASS1[_]](anotherMatcherFactory: MatcherFactory1[Any, TYPECLASS1]): MatcherFactory2[Any, Existence, TYPECLASS1] = 
  // SKIP-DOTTY-START 
  def and[TYPECLASS1[_]](anotherMatcherFactory: MatcherFactory1[Any, TYPECLASS1]): MatcherFactory2[Any, Existence, TYPECLASS1] = 
  // SKIP-DOTTY-END
    matcherFactory.and(anotherMatcherFactory)
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * file should (exist and be (existFile))
   *                    ^
   * </pre>
   */
  //DOTTY-ONLY infix def and(anotherMatcher: Matcher[Any]): MatcherFactory1[Any, Existence] = 
  // SKIP-DOTTY-START 
  def and(anotherMatcher: Matcher[Any]): MatcherFactory1[Any, Existence] = 
  // SKIP-DOTTY-END
    matcherFactory.and(anotherMatcher)  
    
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * file should (exist or equal (existFile))
   *                    ^
   * </pre>
   */
  //DOTTY-ONLY infix def or[TYPECLASS1[_]](anotherMatcherFactory: MatcherFactory1[Any, TYPECLASS1]): MatcherFactory2[Any, Existence, TYPECLASS1] =  
  // SKIP-DOTTY-START 
  def or[TYPECLASS1[_]](anotherMatcherFactory: MatcherFactory1[Any, TYPECLASS1]): MatcherFactory2[Any, Existence, TYPECLASS1] = 
  // SKIP-DOTTY-END
    matcherFactory.or(anotherMatcherFactory)
    
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * file should (exist or be (existFile))
   *                    ^
   * </pre>
   */
  //DOTTY-ONLY infix def or(anotherMatcher: Matcher[Any]): MatcherFactory1[Any, Existence] = 
  // SKIP-DOTTY-START 
  def or(anotherMatcher: Matcher[Any]): MatcherFactory1[Any, Existence] = 
  // SKIP-DOTTY-END
    matcherFactory.or(anotherMatcher)
}
