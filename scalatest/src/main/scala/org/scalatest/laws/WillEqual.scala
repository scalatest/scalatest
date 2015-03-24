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
package org.scalatest

import org.scalatest.matchers._
import org.scalatest.enablers._
import java.lang.reflect.Method
import java.lang.reflect.Modifier
import scala.util.matching.Regex
import java.lang.reflect.Field
import scala.reflect.Manifest
import MatchersHelper.transformOperatorChars
import scala.collection.Traversable
import Assertions.areEqualComparingArraysStructurally
import scala.collection.GenTraversable
import scala.collection.GenSeq
import scala.collection.GenMap
import org.scalactic.Tolerance
import org.scalactic.Explicitly
import org.scalactic.EqualityPolicy.Spread
import org.scalactic.EqualityPolicy.TripleEqualsInvocation
import org.scalactic.Equality
import org.scalactic.EqualityPolicy.TripleEqualsInvocationOnSpread
import org.scalactic.EqualityConstraint
import org.scalactic.Prettifier
import org.scalactic.Every
import MatchersHelper.andMatchersAndApply
import MatchersHelper.orMatchersAndApply
import org.scalatest.words._
import MatchersHelper.matchSymbolToPredicateMethod
import MatchersHelper.accessProperty
import MatchersHelper.newTestFailedException
import MatchersHelper.fullyMatchRegexWithGroups
import MatchersHelper.startWithRegexWithGroups
import MatchersHelper.endWithRegexWithGroups
import MatchersHelper.includeRegexWithGroups
import org.scalactic.NormalizingEquality
import Assertions.checkExpectedException
import Assertions.checkNoException
import exceptions.StackDepthExceptionHelper.getStackDepthFun
import exceptions.NotAllowedException
import scala.language.experimental.macros
import scala.language.higherKinds
import exceptions.TestFailedException

trait WillEqual { willEqual =>

  import scala.language.implicitConversions

  /**
   * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
   * the matchers DSL.
   *
   * <p>
   * This class is used in conjunction with an implicit conversion to enable <code>will</code> methods to
   * be invoked on objects of type <code>Any</code>.
   * </p>
   *
   * @author Bill Venners
   */
  sealed class AnyWillEqualWrapper[T](val leftSideValue: T) {

    /**
     * This method enables syntax such as the following:
     *
     * <pre class="stHighlight">
     * a willEqual b
     *   ^
     * </pre>
     */
    def willEqual[R](right: R)(implicit evidence: EqualityConstraint[T, R]): Fact = {
      val (leftee, rightee) = Suite.getObjectsForFailureMessage(leftSideValue, right)
      if (evidence.areEqual(leftSideValue, right)) {
        Yes(
          Resources.rawDidNotEqual,
          Resources.rawEqualed,
          Resources.rawDidNotEqual,
          Resources.rawEqualed,
          Vector(leftee, rightee),
          Vector(leftee, rightee),
          Vector(leftee, rightee),
          Vector(leftee, rightee)
        )
      }
      else {
        No(
          Resources.rawDidNotEqual,
          Resources.rawEqualed,
          Resources.rawDidNotEqual,
          Resources.rawEqualed,
          Vector(leftee, rightee),
          Vector(leftee, rightee),
          Vector(leftee, rightee),
          Vector(leftee, rightee)
        )
      }
    }
  }

  implicit def convertToAnyWillEqualWrapper[T](o: T): AnyWillEqualWrapper[T] = new AnyWillEqualWrapper(o)
}

object WillEqual extends WillEqual
