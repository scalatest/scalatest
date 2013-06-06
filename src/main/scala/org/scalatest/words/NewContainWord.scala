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

import scala.collection.GenTraversable
import org.scalatest.enablers.Containing
import org.scalatest.enablers.Aggregating
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatcherFactory1
import org.scalatest.matchers.MatchResult
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class NewContainWord {

  def newOneOf(right: Any*): MatcherFactory1[Any, Containing] = {
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
  
  def newNoneOf(right: Any*): MatcherFactory1[Any, Containing] = {
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
  
  def newTheSameElementsAs(right: GenTraversable[Any]): MatcherFactory1[Any, Aggregating] = {
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
  
  def newTheSameElementsInOrderAs(right: GenTraversable[Any]): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              aggregating.containsTheSameElementsInOrderAs(left, right),
              FailureMessages("didNotContainSameElementsInOrder", left, right),
              FailureMessages("containedSameElementsInOrder", left, right)
            )
          }
        }
      }
    }
  }
  
  def newOnly(right: Any*): MatcherFactory1[Any, Aggregating] = {
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

  def newInOrderOnly(right: Any*): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              aggregating.containsInOrderOnly(left, right),
              FailureMessages("didNotContainInOrderOnlyElements", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              FailureMessages("containedInOrderOnlyElements", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
            )
          }
        }
      }
    }
  }
  
  def newAllOf(right: Any*): MatcherFactory1[Any, Aggregating] = {
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
  
  def newInOrder(right: Any*): MatcherFactory1[Any, Aggregating] = {
    new MatcherFactory1[Any, Aggregating] {
      def matcher[T](implicit aggregating: Aggregating[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
            MatchResult(
              aggregating.containsInOrder(left, right),
              FailureMessages("didNotContainAllOfElementsInOrder", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              FailureMessages("containedAllOfElementsInOrder", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
            )
          }
        }
      }
    }
  }
}

