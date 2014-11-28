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
package org.scalactic

import org.scalatest._
import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenSet
import scala.collection.GenIterable
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce

class NormalizedEqualitySpec extends Spec with NonImplicitAssertions {

  final case class StringWrapper(var value: String, var isNormalized: Boolean = false, var equalsWasCalled: Boolean = false) {
    override def equals(other: Any): Boolean = {
      equalsWasCalled = true
      other match {
        case that: StringWrapper => value == that.value
        case _ => false
      }
    }
  }

  class NormalizedStringWrapperEquality extends NormalizingEquality[StringWrapper] {
    def normalized(sw: StringWrapper): StringWrapper = {
      sw.value = sw.value.toLowerCase
      sw.isNormalized = true
      sw
    }
    def normalizedCanHandle(b: Any) =
      b match {
        case s: StringWrapper => true
        case _ => false
      }
    def normalizedOrSame(b: Any) =
      b match {
        case s: StringWrapper => normalized(s)
        case _ => b
      }
  }

  object `A NormalizingEquality type class` {

    def `should call .equals on the left hand object (and not on the right hand object)` {

      val a = StringWrapper("HowDy")
      val b = StringWrapper("hoWdY")
      assert(!a.equalsWasCalled)
      assert(!b.equalsWasCalled)
      assert((new NormalizedStringWrapperEquality).areEqual(a, b))
      assert(a.equalsWasCalled)
      assert(!b.equalsWasCalled)
    }

    def `should normalize both sides when areEqual is called` {

      val a = StringWrapper("HowDy")
      val b = StringWrapper("hoWdY")
      assert(!a.isNormalized)
      assert(!b.isNormalized)
      assert((new NormalizedStringWrapperEquality).areEqual(a, b))
      assert(a.isNormalized)
      assert(b.isNormalized)
    }

    def `should call .deep first if left side, right side, or both are Arrays` {

      class NormalizedArrayOfStringEquality extends NormalizingEquality[Array[String]] {
        def normalized(arr: Array[String]): Array[String] = arr.map(_.trim.toLowerCase)
        def normalizedCanHandle(b: Any) =
          b match {
            case arr: Array[_] =>
              if (arr.forall(_.isInstanceOf[String]))
                true
              else
                false
            case _ => false
          }
        def normalizedOrSame(b: Any) =
          b match {
            case arr: Array[_] =>
              if (arr.forall(_.isInstanceOf[String]))
                normalized(arr.asInstanceOf[Array[String]])
              else
                b
            case _ => b
          }
      }

      val a = Array(" hi", "ThErE    ", "DuDeS  ")
      val b = Array("HI", "there", "  dUdEs")
      val v = Vector("hi", "there", "dudes")
      assert((new NormalizedArrayOfStringEquality).areEqual(a, v))
      assert((new NormalizedArrayOfStringEquality).areEqual(a, b))
    }
  }
  object `Normalizations` {
    def `should be composable with and` {
      import StringNormalizations._
      assert(lowerCased.normalized("HowdY") == "howdy")
      assert(lowerCased.normalized("HowdY") != "howdy padna!")
      assert(trimmed.normalized("\nhowdy  ") == "howdy")
      assert(trimmed.normalized("\nHowdY  ") != "howdy")
      assert((trimmed and lowerCased).normalized("\nHowdY  ") == "howdy")
      assert((trimmed and lowerCased).normalized("\nHowdY  ") != "rowdy")
    }
  }
}

