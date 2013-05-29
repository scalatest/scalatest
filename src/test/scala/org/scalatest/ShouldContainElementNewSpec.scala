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

import org.scalatest.prop.Checkers
import org.scalacheck._
import Arbitrary._
import Prop._
import org.scalatest.exceptions.TestFailedException
import org.scalautils.Equality
import org.scalautils.Explicitly
import SharedHelpers.thisLineNumber

// Calling this ShouldContainElementNewSpec so that it is easy to 
// keep track of the new tests that we'll need to port over to
// inspector shorthands.
class ShouldContainElementNewSpec extends Spec with Matchers with Explicitly {

  // Checking for a specific size
  object `The 'contain (<value>)' syntax` {
    def `should allow any type to be passed in` {
      Vector(1, "2") should contain ("2")
      Vector(1, "2") should contain (1)
    }

    def `should use an Equality of the element type of the left-hand "holder" on a GenTraversable` {

      Vector(2, 2) should contain (2)
      val e1 = intercept[TestFailedException] {
        Vector(2, 2) should not contain (2)
      }

      e1.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      implicit val e = new Equality[Int] {
        def areEqual(a: Int, b: Any): Boolean = a != b
      }
      
      val e2 = intercept[TestFailedException] {
        Vector(2, 2) should contain (2)
      }
      Vector(2, 2) should not contain (2)

      e2.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 5))

      (Vector(2, 2) should contain (2)) (decided by defaultEquality[Int])
      val e3 = intercept[TestFailedException] {
        (Vector(2, 2) should not contain (2)) (decided by defaultEquality[Int])
      }

      e3.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
    }

    def `should use an Equality of the element type of the left-hand "holder" on a String` {
      
      "22" should contain ('2')
      val e1 = intercept[TestFailedException] {
        "22" should not contain ('2')
      }

      e1.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      implicit val e = new Equality[Char] {
        def areEqual(a: Char, b: Any): Boolean = a != b
      }
      
      val e2 = intercept[TestFailedException] {
        "22" should contain ('2')
      }
      "22" should not contain ('2')

      e2.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 5))

      ("22" should contain ('2')) (decided by defaultEquality[Char])
      val e3 = intercept[TestFailedException] {
        ("22" should not contain ('2')) (decided by defaultEquality[Char])
      }

      e3.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
    }

    def `should use an Equality of the element type of the left-hand "holder" on an Array` {
      
      Array(2, 2) should contain (2)
      val e1 = intercept[TestFailedException] {
        Array(2, 2) should not contain (2)
      }

      e1.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      implicit val e = new Equality[Int] {
        def areEqual(a: Int, b: Any): Boolean = a != b
      }
      
      val e2 = intercept[TestFailedException] {
        Array(2, 2) should contain (2)
      }
      Array(2, 2) should not contain (2)

      e2.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 5))

      (Array(2, 2) should contain (2)) (decided by defaultEquality[Int])
      val e3 = intercept[TestFailedException] {
        (Array(2, 2) should not contain (2)) (decided by defaultEquality[Int])
      }

      e3.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
    }

    def `should use an Equality of the element type of the left-hand "holder" on a Java Collection` {

      val javaSet: java.util.Set[Int] = new java.util.HashSet
      javaSet.add(2)

      javaSet should contain (2)
      val e1 = intercept[TestFailedException] {
        javaSet should not contain (2)
      }

      e1.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e1.failedCodeLineNumber should be (Some(thisLineNumber - 4))

      implicit val e = new Equality[Int] {
        def areEqual(a: Int, b: Any): Boolean = a != b
      }

      val e2 = intercept[TestFailedException] {
        javaSet should contain (2)
      }
      javaSet should not contain (2)

      e2.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 5))

      (javaSet should contain (2)) (decided by defaultEquality[Int])
      val e3 = intercept[TestFailedException] {
        (javaSet should not contain (2)) (decided by defaultEquality[Int])
      }

      e3.failedCodeFileName should be (Some("ShouldContainElementNewSpec.scala"))
      e3.failedCodeLineNumber should be (Some(thisLineNumber - 4))
    }
  }
}

