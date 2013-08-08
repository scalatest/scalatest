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
package org.scalatest.matchers

trait MatcherProducers {
  implicit def decorate[T](f: T => Matcher[T]) = 
    new {
      def composeTwice[U](g: U => T): U => Matcher[U] = (f compose g) andThen (_ compose g)
      def mapResult(prettify: MatchResult => MatchResult): T => Matcher[T] =
        (o: T) => f(o) mapResult prettify
      def mapArgs(prettify: Any => String): T => Matcher[T] =
        (o: T) => f(o) mapArgs prettify
    }
}

object MatcherProducers extends MatcherProducers

