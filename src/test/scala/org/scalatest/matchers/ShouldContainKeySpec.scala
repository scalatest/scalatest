/*
 * Copyright 2001-2008 Artima, Inc.
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

import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck._
import Arbitrary._
import Prop._
import org.scalatest.exceptions.TestFailedException

class ShouldContainKeySpec extends Spec with ShouldMatchers with Checkers with ReturnsNormallyThrowsAssertion {

  // Checking for a specific size
  object `The 'contain key (Int)' syntax` {

    object `on scala.collection.immutable.Map` {

      def `should do nothing if map contains specified key` {
        Map("one" -> 1, "two" -> 2) should contain key ("two")
        Map("one" -> 1, "two" -> 2) should (contain key ("two"))
        Map(1 -> "one", 2 -> "two") should contain key (2)
      }

      def `should do nothing if map does not contain the specified key and used with not` {
        Map("one" -> 1, "two" -> 2) should not { contain key ("three") }
        Map("one" -> 1, "two" -> 2) should not contain key ("three")
        Map("one" -> 1, "two" -> 2) should (not contain key ("three"))
      }

      def `should do nothing when map contains specified key and used in a logical-and expression` {
        Map("one" -> 1, "two" -> 2) should { contain key ("two") and (contain key ("one")) }
        Map("one" -> 1, "two" -> 2) should ((contain key ("two")) and (contain key ("one")))
        Map("one" -> 1, "two" -> 2) should (contain key ("two") and contain key ("one"))
      }

      def `should do nothing when map contains specified key and used in a logical-or expression` {
        Map("one" -> 1, "two" -> 2) should { contain key ("cat") or (contain key ("one")) }
        Map("one" -> 1, "two" -> 2) should ((contain key ("cat")) or (contain key ("one")))
        Map("one" -> 1, "two" -> 2) should (contain key ("cat") or contain key ("one"))
      }

      def `should do nothing when map does not contain the specified key and used in a logical-and expression with not` {
        Map("one" -> 1, "two" -> 2) should { not { contain key ("five") } and not { contain key ("three") }}
        Map("one" -> 1, "two" -> 2) should ((not contain key ("five")) and (not contain key ("three")))
        Map("one" -> 1, "two" -> 2) should (not contain key ("five") and not contain key ("three"))
      }

      def `should do nothing when map does not contain the specified key and used in a logical-or expression with not` {
        Map("one" -> 1, "two" -> 2) should { not { contain key ("two") } or not { contain key ("three") }}
        Map("one" -> 1, "two" -> 2) should ((not contain key ("two")) or (not contain key ("three")))
        Map("one" -> 1, "two" -> 2) should (not contain key ("two") or not contain key ("three"))
      }

      def `should throw TestFailedException if map does not contain the specified key` {
        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should contain key ("three")
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain key \"three\"")
      }

      def `should throw TestFailedException if contains the specified key when used with not` {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (not contain key ("two"))
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained key \"two\"")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should not (contain key ("two"))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained key \"two\"")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should not contain key ("two")
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained key \"two\"")
      }

      def `should throw an TestFailedException when map doesn't contain specified key and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should { contain key ("five") and (contain key ("two")) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain key \"five\"")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should ((contain key ("five")) and (contain key ("two")))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain key \"five\"")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (contain key ("five") and contain key ("two"))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain key \"five\"")
      }

      def `should throw an TestFailedException when map doesn't contain specified key and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should { contain key ("fifty five") or (contain key ("twenty two")) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain key \"fifty five\", and Map(one -> 1, two -> 2) did not contain key \"twenty two\"")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should ((contain key ("fifty five")) or (contain key ("twenty two")))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain key \"fifty five\", and Map(one -> 1, two -> 2) did not contain key \"twenty two\"")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (contain key ("fifty five") or contain key ("twenty two"))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain key \"fifty five\", and Map(one -> 1, two -> 2) did not contain key \"twenty two\"")
      }

      def `should throw an TestFailedException when map contains specified key and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should { not { contain key ("three") } and not { contain key ("two") }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain key \"three\", but Map(one -> 1, two -> 2) contained key \"two\"")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should ((not contain key ("three")) and (not contain key ("two")))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain key \"three\", but Map(one -> 1, two -> 2) contained key \"two\"")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (not contain key ("three") and not contain key ("two"))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain key \"three\", but Map(one -> 1, two -> 2) contained key \"two\"")
      }

      def `should throw an TestFailedException when map contains specified key and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should { not { contain key ("two") } or not { contain key ("two") }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained key \"two\", and Map(one -> 1, two -> 2) contained key \"two\"")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should ((not contain key ("two")) or (not contain key ("two")))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained key \"two\", and Map(one -> 1, two -> 2) contained key \"two\"")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (not contain key ("two") or not contain key ("two"))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained key \"two\", and Map(one -> 1, two -> 2) contained key \"two\"")
      }

      def `should work on parallel form` {
        Map("one" -> 1, "two" -> 2).par should contain key ("two")
      }
    }

    object `on scala.collection.mutable.Map` {

      import scala.collection.mutable

      def `should do nothing if map contains specified key` {
        mutable.Map("one" -> 1, "two" -> 2) should contain key ("two")
        mutable.Map("one" -> 1, "two" -> 2) should (contain key ("two"))
        mutable.Map(1 -> "one", 2 -> "two") should contain key (2)
      }

      def `should do nothing if map does not contain the specified key and used with not` {
        mutable.Map("one" -> 1, "two" -> 2) should not { contain key ("three") }
        mutable.Map("one" -> 1, "two" -> 2) should not contain key ("three")
        mutable.Map("one" -> 1, "two" -> 2) should (not contain key ("three"))
      }

      def `should do nothing when map contains specified key and used in a logical-and expression` {
        mutable.Map("one" -> 1, "two" -> 2) should { contain key ("two") and (contain key ("one")) }
        mutable.Map("one" -> 1, "two" -> 2) should ((contain key ("two")) and (contain key ("one")))
        mutable.Map("one" -> 1, "two" -> 2) should (contain key ("two") and contain key ("one"))
      }

      def `should do nothing when map contains specified key and used in a logical-or expression` {
        mutable.Map("one" -> 1, "two" -> 2) should { contain key ("cat") or (contain key ("one")) }
        mutable.Map("one" -> 1, "two" -> 2) should ((contain key ("cat")) or (contain key ("one")))
        mutable.Map("one" -> 1, "two" -> 2) should (contain key ("cat") or contain key ("one"))
      }

      def `should do nothing when map does not contain the specified key and used in a logical-and expression with not` {
        mutable.Map("one" -> 1, "two" -> 2) should { not { contain key ("five") } and not { contain key ("three") }}
        mutable.Map("one" -> 1, "two" -> 2) should ((not contain key ("five")) and (not contain key ("three")))
        mutable.Map("one" -> 1, "two" -> 2) should (not contain key ("five") and not contain key ("three"))
      }

      def `should do nothing when map does not contain the specified key and used in a logical-or expression with not` {
        mutable.Map("one" -> 1, "two" -> 2) should { not { contain key ("two") } or not { contain key ("three") }}
        mutable.Map("one" -> 1, "two" -> 2) should ((not contain key ("two")) or (not contain key ("three")))
        mutable.Map("one" -> 1, "two" -> 2) should (not contain key ("two") or not contain key ("three"))
      }

      def `should throw TestFailedException if map does not contain the specified key` {
        val map = mutable.Map("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {
          map should contain key ("three")
        }
        assert(caught1.getMessage === map + " did not contain key \"three\"")
      }

      def `should throw TestFailedException if contains the specified key when used with not` {
        val map1 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {
          map1 should (not contain key ("two"))
        }
        assert(caught1.getMessage === map1 + " contained key \"two\"")
        val map2 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught2 = intercept[TestFailedException] {
          map2 should not (contain key ("two"))
        }
        assert(caught2.getMessage === map2 + " contained key \"two\"")
        
        val map3 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught3 = intercept[TestFailedException] {
          map3 should not contain key ("two")
        }
        assert(caught3.getMessage === map3 + " contained key \"two\"")
      }

      def `should throw an TestFailedException when map doesn't contain specified key and used in a logical-and expression` {
        val map1 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {
          map1 should { contain key ("five") and (contain key ("two")) }
        }
        assert(caught1.getMessage === map1 + " did not contain key \"five\"")
        
        val map2 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught2 = intercept[TestFailedException] {
          map2 should ((contain key ("five")) and (contain key ("two")))
        }
        assert(caught2.getMessage === map2 + " did not contain key \"five\"")
        
        val map3 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught3 = intercept[TestFailedException] {
          map3 should (contain key ("five") and contain key ("two"))
        }
        assert(caught3.getMessage === map3 + " did not contain key \"five\"")
      }

      def `should throw an TestFailedException when map doesn't contain specified key and used in a logical-or expression` {
        val map1 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {
          map1 should { contain key ("fifty five") or (contain key ("twenty two")) }
        }
        assert(caught1.getMessage === map1 + " did not contain key \"fifty five\", and " + map1 + " did not contain key \"twenty two\"")
        
        val map2 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught2 = intercept[TestFailedException] {
          map2 should ((contain key ("fifty five")) or (contain key ("twenty two")))
        }
        assert(caught2.getMessage === map2 + " did not contain key \"fifty five\", and " + map2 + " did not contain key \"twenty two\"")
        
        val map3 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught3 = intercept[TestFailedException] {
          map3 should (contain key ("fifty five") or contain key ("twenty two"))
        }
        assert(caught3.getMessage === map3 + " did not contain key \"fifty five\", and " + map3 + " did not contain key \"twenty two\"")
      }

      def `should throw an TestFailedException when map contains specified key and used in a logical-and expression with not` {
        val map1 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {
          map1 should { not { contain key ("three") } and not { contain key ("two") }}
        }
        assert(caught1.getMessage === map1 + " did not contain key \"three\", but " + map1 + " contained key \"two\"")
        
        val map2 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught2 = intercept[TestFailedException] {
          map2 should ((not contain key ("three")) and (not contain key ("two")))
        }
        assert(caught2.getMessage === map2 + " did not contain key \"three\", but " + map2 + " contained key \"two\"")
        
        val map3 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught3 = intercept[TestFailedException] {
          map3 should (not contain key ("three") and not contain key ("two"))
        }
        assert(caught3.getMessage === map3 + " did not contain key \"three\", but " + map3 + " contained key \"two\"")
      }

      def `should throw an TestFailedException when map contains specified key and used in a logical-or expression with not` {
        val map1 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {
          map1 should { not { contain key ("two") } or not { contain key ("two") }}
        }
        assert(caught1.getMessage === map1 + " contained key \"two\", and " + map1 + " contained key \"two\"")
        
        val map2 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught2 = intercept[TestFailedException] {
          map2 should ((not contain key ("two")) or (not contain key ("two")))
        }
        assert(caught2.getMessage === map2 + " contained key \"two\", and " + map2 + " contained key \"two\"")
        
        val map3 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught3 = intercept[TestFailedException] {
          map3 should (not contain key ("two") or not contain key ("two"))
        }
        assert(caught3.getMessage === map3 + " contained key \"two\", and " + map3 + " contained key \"two\"")
      }

      def `should work on parallel form` {
        mutable.Map("one" -> 1, "two" -> 2).par should contain key ("two")
      }
    }

    object `on scala.collection.Map` {

      val map: scala.collection.Map[String, Int] = Map("one" -> 1, "two" -> 2)

      def `should do nothing if map contains specified key` {
        map should contain key ("two")
        map should (contain key ("two"))
      }

      def `should do nothing if map does not contain the specified key and used with not` {
        map should not { contain key ("three") }
        map should not contain key ("three")
        map should (not contain key ("three"))
      }

      def `should do nothing when map contains specified key and used in a logical-and expression` {
        map should { contain key ("two") and (contain key ("one")) }
        map should ((contain key ("two")) and (contain key ("one")))
        map should (contain key ("two") and contain key ("one"))
      }

      def `should do nothing when map contains specified key and used in a logical-or expression` {
        map should { contain key ("cat") or (contain key ("one")) }
        map should ((contain key ("cat")) or (contain key ("one")))
        map should (contain key ("cat") or contain key ("one"))
      }

      def `should do nothing when map does not contain the specified key and used in a logical-and expression with not` {
        map should { not { contain key ("five") } and not { contain key ("three") }}
        map should ((not contain key ("five")) and (not contain key ("three")))
        map should (not contain key ("five") and not contain key ("three"))
      }

      def `should do nothing when map does not contain the specified key and used in a logical-or expression with not` {
        map should { not { contain key ("two") } or not { contain key ("three") }}
        map should ((not contain key ("two")) or (not contain key ("three")))
        map should (not contain key ("two") or not contain key ("three"))
      }

      def `should throw TestFailedException if map does not contain the specified key` {
        val caught1 = intercept[TestFailedException] {
          map should contain key ("three")
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain key \"three\"")
      }

      def `should throw TestFailedException if contains the specified key when used with not` {

        val caught1 = intercept[TestFailedException] {
          map should (not contain key ("two"))
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained key \"two\"")

        val caught2 = intercept[TestFailedException] {
          map should not (contain key ("two"))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained key \"two\"")

        val caught3 = intercept[TestFailedException] {
          map should not contain key ("two")
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained key \"two\"")
      }

      def `should throw an TestFailedException when map doesn't contain specified key and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          map should { contain key ("five") and (contain key ("two")) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain key \"five\"")

        val caught2 = intercept[TestFailedException] {
          map should ((contain key ("five")) and (contain key ("two")))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain key \"five\"")

        val caught3 = intercept[TestFailedException] {
          map should (contain key ("five") and contain key ("two"))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain key \"five\"")
      }

      def `should throw an TestFailedException when map doesn't contain specified key and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          map should { contain key ("fifty five") or (contain key ("twenty two")) }
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain key \"fifty five\", and Map(one -> 1, two -> 2) did not contain key \"twenty two\"")

        val caught2 = intercept[TestFailedException] {
          map should ((contain key ("fifty five")) or (contain key ("twenty two")))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain key \"fifty five\", and Map(one -> 1, two -> 2) did not contain key \"twenty two\"")

        val caught3 = intercept[TestFailedException] {
          map should (contain key ("fifty five") or contain key ("twenty two"))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain key \"fifty five\", and Map(one -> 1, two -> 2) did not contain key \"twenty two\"")
      }

      def `should throw an TestFailedException when map contains specified key and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          map should { not { contain key ("three") } and not { contain key ("two") }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain key \"three\", but Map(one -> 1, two -> 2) contained key \"two\"")

        val caught2 = intercept[TestFailedException] {
          map should ((not contain key ("three")) and (not contain key ("two")))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain key \"three\", but Map(one -> 1, two -> 2) contained key \"two\"")

        val caught3 = intercept[TestFailedException] {
          map should (not contain key ("three") and not contain key ("two"))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain key \"three\", but Map(one -> 1, two -> 2) contained key \"two\"")
      }

      def `should throw an TestFailedException when map contains specified key and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          map should { not { contain key ("two") } or not { contain key ("two") }}
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained key \"two\", and Map(one -> 1, two -> 2) contained key \"two\"")

        val caught2 = intercept[TestFailedException] {
          map should ((not contain key ("two")) or (not contain key ("two")))
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained key \"two\", and Map(one -> 1, two -> 2) contained key \"two\"")

        val caught3 = intercept[TestFailedException] {
          map should (not contain key ("two") or not contain key ("two"))
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained key \"two\", and Map(one -> 1, two -> 2) contained key \"two\"")
      }

      def `should work on parallel form` {
        map.par should contain key ("two")
      }
    }

    object `on scala.collection.immutable.HashMap` {

      import scala.collection.immutable.HashMap

      def `should do nothing if map contains specified key` {
        HashMap("one" -> 1, "two" -> 2) should contain key ("two")
        HashMap("one" -> 1, "two" -> 2) should (contain key ("two"))
        HashMap(1 -> "one", 2 -> "two") should contain key (2)
      }

      def `should do nothing if map does not contain the specified key and used with not` {
        HashMap("one" -> 1, "two" -> 2) should not { contain key ("three") }
        HashMap("one" -> 1, "two" -> 2) should not contain key ("three")
        HashMap("one" -> 1, "two" -> 2) should (not contain key ("three"))
      }

      def `should do nothing when map contains specified key and used in a logical-and expression` {
        HashMap("one" -> 1, "two" -> 2) should { contain key ("two") and (contain key ("one")) }
        HashMap("one" -> 1, "two" -> 2) should ((contain key ("two")) and (contain key ("one")))
        HashMap("one" -> 1, "two" -> 2) should (contain key ("two") and contain key ("one"))
      }

      def `should do nothing when map contains specified key and used in a logical-or expression` {
        HashMap("one" -> 1, "two" -> 2) should { contain key ("cat") or (contain key ("one")) }
        HashMap("one" -> 1, "two" -> 2) should ((contain key ("cat")) or (contain key ("one")))
        HashMap("one" -> 1, "two" -> 2) should (contain key ("cat") or contain key ("one"))
      }

      def `should do nothing when map does not contain the specified key and used in a logical-and expression with not` {
        HashMap("one" -> 1, "two" -> 2) should { not { contain key ("five") } and not { contain key ("three") }}
        HashMap("one" -> 1, "two" -> 2) should ((not contain key ("five")) and (not contain key ("three")))
        HashMap("one" -> 1, "two" -> 2) should (not contain key ("five") and not contain key ("three"))
      }

      def `should do nothing when map does not contain the specified key and used in a logical-or expression with not` {
        HashMap("one" -> 1, "two" -> 2) should { not { contain key ("two") } or not { contain key ("three") }}
        HashMap("one" -> 1, "two" -> 2) should ((not contain key ("two")) or (not contain key ("three")))
        HashMap("one" -> 1, "two" -> 2) should (not contain key ("two") or not contain key ("three"))
      }

      def `should throw TestFailedException if map does not contain the specified key` {
        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should contain key ("three")
        }
        //assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain key \"three\"")
        caught1.getMessage should fullyMatch regex ("Map(.*) did not contain key \"three\"")
      }

      def `should throw TestFailedException if contains the specified key when used with not` {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (not contain key ("two"))
        }
        //assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained key \"two\"")
        caught1.getMessage should fullyMatch regex ("Map(.*) contained key \"two\"")

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should not (contain key ("two"))
        }
        //assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained key \"two\"")
        caught1.getMessage should fullyMatch regex ("Map(.*) contained key \"two\"")

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should not contain key ("two")
        }
        //assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained key \"two\"")
        caught1.getMessage should fullyMatch regex ("Map(.*) contained key \"two\"")
      }

      def `should throw an TestFailedException when map doesn't contain specified key and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should { contain key ("five") and (contain key ("two")) }
        }
        //assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain key \"five\"")
        caught1.getMessage should fullyMatch regex ("Map(.*) did not contain key \"five\"")

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should ((contain key ("five")) and (contain key ("two")))
        }
        //assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain key \"five\"")
        caught1.getMessage should fullyMatch regex ("Map(.*) did not contain key \"five\"")

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (contain key ("five") and contain key ("two"))
        }
        //assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain key \"five\"")
        caught1.getMessage should fullyMatch regex ("Map(.*) did not contain key \"five\"")
      }

      def `should throw an TestFailedException when map doesn't contain specified key and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should { contain key ("fifty five") or (contain key ("twenty two")) }
        }
        //assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain key \"fifty five\", and Map(one -> 1, two -> 2) did not contain key \"twenty two\"")
        caught1.getMessage should fullyMatch regex ("Map(.*) did not contain key \"fifty five\", and Map(.*) did not contain key \"twenty two\"")

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should ((contain key ("fifty five")) or (contain key ("twenty two")))
        }
        //assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain key \"fifty five\", and Map(one -> 1, two -> 2) did not contain key \"twenty two\"")
        caught1.getMessage should fullyMatch regex ("Map(.*) did not contain key \"fifty five\", and Map(.*) did not contain key \"twenty two\"")

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (contain key ("fifty five") or contain key ("twenty two"))
        }
        // assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain key \"fifty five\", and Map(one -> 1, two -> 2) did not contain key \"twenty two\"")
        caught1.getMessage should fullyMatch regex ("Map(.*) did not contain key \"fifty five\", and Map(.*) did not contain key \"twenty two\"")
      }

      def `should throw an TestFailedException when map contains specified key and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should { not { contain key ("three") } and not { contain key ("two") }}
        }
        //assert(caught1.getMessage === "Map(one -> 1, two -> 2) did not contain key \"three\", but Map(one -> 1, two -> 2) contained key \"two\"")
        caught1.getMessage should fullyMatch regex ("Map(.*) did not contain key \"three\", but Map(.*) contained key \"two\"")

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should ((not contain key ("three")) and (not contain key ("two")))
        }
        //assert(caught2.getMessage === "Map(one -> 1, two -> 2) did not contain key \"three\", but Map(one -> 1, two -> 2) contained key \"two\"")
        caught1.getMessage should fullyMatch regex ("Map(.*) did not contain key \"three\", but Map(.*) contained key \"two\"")

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (not contain key ("three") and not contain key ("two"))
        }
        //assert(caught3.getMessage === "Map(one -> 1, two -> 2) did not contain key \"three\", but Map(one -> 1, two -> 2) contained key \"two\"")
        caught1.getMessage should fullyMatch regex ("Map(.*) did not contain key \"three\", but Map(.*) contained key \"two\"")
      }

      def `should throw an TestFailedException when map contains specified key and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should { not { contain key ("two") } or not { contain key ("two") }}
        }
        //assert(caught1.getMessage === "Map(one -> 1, two -> 2) contained key \"two\", and Map(one -> 1, two -> 2) contained key \"two\"")
        caught1.getMessage should fullyMatch regex ("Map(.*) contained key \"two\", and Map(.*) contained key \"two\"")

        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should ((not contain key ("two")) or (not contain key ("two")))
        }
        //assert(caught2.getMessage === "Map(one -> 1, two -> 2) contained key \"two\", and Map(one -> 1, two -> 2) contained key \"two\"")
        caught1.getMessage should fullyMatch regex ("Map(.*) contained key \"two\", and Map(.*) contained key \"two\"")

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (not contain key ("two") or not contain key ("two"))
        }
        //assert(caught3.getMessage === "Map(one -> 1, two -> 2) contained key \"two\", and Map(one -> 1, two -> 2) contained key \"two\"")
        caught1.getMessage should fullyMatch regex ("Map(.*) contained key \"two\", and Map(.*) contained key \"two\"")
      }

      def `should work on parallel form` {
        HashMap("one" -> 1, "two" -> 2).par should contain key ("two")
      }
    }

    object `on scala.collection.mutable.HashMap` {

      import scala.collection.mutable

      def `should do nothing if map contains specified key` {
        mutable.HashMap("one" -> 1, "two" -> 2) should contain key ("two")
        mutable.HashMap("one" -> 1, "two" -> 2) should (contain key ("two"))
        mutable.HashMap(1 -> "one", 2 -> "two") should contain key (2)
      }

      def `should do nothing if map does not contain the specified key and used with not` {
        mutable.HashMap("one" -> 1, "two" -> 2) should not { contain key ("three") }
        mutable.HashMap("one" -> 1, "two" -> 2) should not contain key ("three")
        mutable.HashMap("one" -> 1, "two" -> 2) should (not contain key ("three"))
      }

      def `should do nothing when map contains specified key and used in a logical-and expression` {
        mutable.HashMap("one" -> 1, "two" -> 2) should { contain key ("two") and (contain key ("one")) }
        mutable.HashMap("one" -> 1, "two" -> 2) should ((contain key ("two")) and (contain key ("one")))
        mutable.HashMap("one" -> 1, "two" -> 2) should (contain key ("two") and contain key ("one"))
      }

      def `should do nothing when map contains specified key and used in a logical-or expression` {
        mutable.HashMap("one" -> 1, "two" -> 2) should { contain key ("cat") or (contain key ("one")) }
        mutable.HashMap("one" -> 1, "two" -> 2) should ((contain key ("cat")) or (contain key ("one")))
        mutable.HashMap("one" -> 1, "two" -> 2) should (contain key ("cat") or contain key ("one"))
      }

      def `should do nothing when map does not contain the specified key and used in a logical-and expression with not` {
        mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain key ("five") } and not { contain key ("three") }}
        mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain key ("five")) and (not contain key ("three")))
        mutable.HashMap("one" -> 1, "two" -> 2) should (not contain key ("five") and not contain key ("three"))
      }

      def `should do nothing when map does not contain the specified key and used in a logical-or expression with not` {
        mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain key ("two") } or not { contain key ("three") }}
        mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain key ("two")) or (not contain key ("three")))
        mutable.HashMap("one" -> 1, "two" -> 2) should (not contain key ("two") or not contain key ("three"))
      }

      def `should throw TestFailedException if map does not contain the specified key` {
        val map = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {
          map should contain key ("three")
        }
        assert(caught1.getMessage === map + " did not contain key \"three\"")
      }

      def `should throw TestFailedException if contains the specified key when used with not` {
        val map1 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {
          map1 should (not contain key ("two"))
        }
        assert(caught1.getMessage === map1 + " contained key \"two\"")
        
        val map2 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught2 = intercept[TestFailedException] {
          map2 should not (contain key ("two"))
        }
        assert(caught2.getMessage === map2 + " contained key \"two\"")
        
        val map3 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught3 = intercept[TestFailedException] {
          map3 should not contain key ("two")
        }
        assert(caught3.getMessage === map3 + " contained key \"two\"")
      }

      def `should throw an TestFailedException when map doesn't contain specified key and used in a logical-and expression` {
        val map1 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {
          map1 should { contain key ("five") and (contain key ("two")) }
        }
        assert(caught1.getMessage === map1 + " did not contain key \"five\"")
        
        val map2 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught2 = intercept[TestFailedException] {
          map2 should ((contain key ("five")) and (contain key ("two")))
        }
        assert(caught2.getMessage === map2 + " did not contain key \"five\"")
        
        val map3 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught3 = intercept[TestFailedException] {
          map3 should (contain key ("five") and contain key ("two"))
        }
        assert(caught3.getMessage === map3 + " did not contain key \"five\"")
      }

      def `should throw an TestFailedException when map doesn't contain specified key and used in a logical-or expression` {
        val map1 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {
          map1 should { contain key ("fifty five") or (contain key ("twenty two")) }
        }
        assert(caught1.getMessage === map1 + " did not contain key \"fifty five\", and " + map1 + " did not contain key \"twenty two\"")
        
        val map2 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught2 = intercept[TestFailedException] {
          map2 should ((contain key ("fifty five")) or (contain key ("twenty two")))
        }
        assert(caught2.getMessage === map2 + " did not contain key \"fifty five\", and " + map2 + " did not contain key \"twenty two\"")
        
        val map3 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught3 = intercept[TestFailedException] {
          map3 should (contain key ("fifty five") or contain key ("twenty two"))
        }
        assert(caught3.getMessage === map3 + " did not contain key \"fifty five\", and " + map3 + " did not contain key \"twenty two\"")
      }

      def `should throw an TestFailedException when map contains specified key and used in a logical-and expression with not` {
        val map1 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {
          map1 should { not { contain key ("three") } and not { contain key ("two") }}
        }
        assert(caught1.getMessage === map1 + " did not contain key \"three\", but " + map1 + " contained key \"two\"")
        
        val map2 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught2 = intercept[TestFailedException] {
          map2 should ((not contain key ("three")) and (not contain key ("two")))
        }
        assert(caught2.getMessage === map2 + " did not contain key \"three\", but " + map2 + " contained key \"two\"")
        val map3 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught3 = intercept[TestFailedException] {
          map3 should (not contain key ("three") and not contain key ("two"))
        }
        assert(caught3.getMessage === map3 + " did not contain key \"three\", but " + map3 + " contained key \"two\"")
      }

      def `should throw an TestFailedException when map contains specified key and used in a logical-or expression with not` {
        val map1 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {
          map1 should { not { contain key ("two") } or not { contain key ("two") }}
        }
        assert(caught1.getMessage === map1 + " contained key \"two\", and " + map1 + " contained key \"two\"")
        
        val map2 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught2 = intercept[TestFailedException] {
          map2 should ((not contain key ("two")) or (not contain key ("two")))
        }
        assert(caught2.getMessage === map2 + " contained key \"two\", and " + map2 + " contained key \"two\"")
        
        val map3 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught3 = intercept[TestFailedException] {(
          map3 should (not contain key ("two") or not contain key ("two")))
        }
        assert(caught3.getMessage === map3 + " contained key \"two\", and " + map3 + " contained key \"two\"")
      }

      def `should work on parallel form` {
        mutable.HashMap("one" -> 1, "two" -> 2).par should contain key ("two")
      }
    }

    object `on java.util.Map` {

      val javaMap: java.util.Map[String, Int] = new java.util.HashMap
      javaMap.put("one",1)
      javaMap.put("two", 2)

      def `should do nothing if map contains specified key` {
        javaMap should contain key ("two")
        javaMap should (contain key ("two"))
      }

      def `should do nothing if map does not contain the specified key and used with not` {
        javaMap should not { contain key ("three") }
        javaMap should not contain key ("three")
        javaMap should (not contain key ("three"))
      }

      def `should do nothing when map contains specified key and used in a logical-and expression` {
        javaMap should { contain key ("two") and (contain key ("one")) }
        javaMap should ((contain key ("two")) and (contain key ("one")))
        javaMap should (contain key ("two") and contain key ("one"))
      }

      def `should do nothing when map contains specified key and used in a logical-or expression` {
        javaMap should { contain key ("cat") or (contain key ("one")) }
        javaMap should ((contain key ("cat")) or (contain key ("one")))
        javaMap should (contain key ("cat") or contain key ("one"))
      }

      def `should do nothing when map does not contain the specified key and used in a logical-and expression with not` {
        javaMap should { not { contain key ("five") } and not { contain key ("three") }}
        javaMap should ((not contain key ("five")) and (not contain key ("three")))
        javaMap should (not contain key ("five") and not contain key ("three"))
      }

      def `should do nothing when map does not contain the specified key and used in a logical-or expression with not` {
        javaMap should { not { contain key ("two") } or not { contain key ("three") }}
        javaMap should ((not contain key ("two")) or (not contain key ("three")))
        javaMap should (not contain key ("two") or not contain key ("three"))
      }

      def `should throw TestFailedException if map does not contain the specified key` {
        val caught1 = intercept[TestFailedException] {
          javaMap should contain key ("three")
        }
        caught1.getMessage should (be === "{one=1, two=2} did not contain key \"three\"" or
          be === "{two=2, one=1} did not contain key \"three\"")
      }

      def `should throw TestFailedException if contains the specified key when used with not` {

        val caught1 = intercept[TestFailedException] {
          javaMap should (not contain key ("two"))
        }
        caught1.getMessage should (be === "{one=1, two=2} contained key \"two\"" or
          be === "{two=2, one=1} contained key \"two\"")

        val caught2 = intercept[TestFailedException] {
          javaMap should not (contain key ("two"))
        }
        caught2.getMessage should (be === "{one=1, two=2} contained key \"two\"" or
          be === "{two=2, one=1} contained key \"two\"")

        val caught3 = intercept[TestFailedException] {
          javaMap should not contain key ("two")
        }
        caught3.getMessage should (be === "{one=1, two=2} contained key \"two\"" or
          be === "{two=2, one=1} contained key \"two\"")
      }

      def `should throw an TestFailedException when map doesn't contain specified key and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          javaMap should { contain key ("five") and (contain key ("two")) }
        }
        caught1.getMessage should (be === "{one=1, two=2} did not contain key \"five\"" or
          be === "{two=2, one=1} did not contain key \"five\"")

        val caught2 = intercept[TestFailedException] {
          javaMap should ((contain key ("five")) and (contain key ("two")))
        }
        caught2.getMessage should (be === "{one=1, two=2} did not contain key \"five\"" or
          be === "{two=2, one=1} did not contain key \"five\"")

        val caught3 = intercept[TestFailedException] {
          javaMap should (contain key ("five") and contain key ("two"))
        }
        caught3.getMessage should (be === "{one=1, two=2} did not contain key \"five\"" or
          be === "{two=2, one=1} did not contain key \"five\"")
      }

      def `should throw an TestFailedException when map doesn't contain specified key and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          javaMap should { contain key ("fifty five") or (contain key ("twenty two")) }
        }
        caught1.getMessage should (be === "{one=1, two=2} did not contain key \"fifty five\", and {one=1, two=2} did not contain key \"twenty two\"" or
          be === "{two=2, one=1} did not contain key \"fifty five\", and {two=2, one=1} did not contain key \"twenty two\"")

        val caught2 = intercept[TestFailedException] {
          javaMap should ((contain key ("fifty five")) or (contain key ("twenty two")))
        }
        caught2.getMessage should (be === "{one=1, two=2} did not contain key \"fifty five\", and {one=1, two=2} did not contain key \"twenty two\"" or
          be === "{two=2, one=1} did not contain key \"fifty five\", and {two=2, one=1} did not contain key \"twenty two\"")

        val caught3 = intercept[TestFailedException] {
          javaMap should (contain key ("fifty five") or contain key ("twenty two"))
        }
        caught3.getMessage should (be === "{one=1, two=2} did not contain key \"fifty five\", and {one=1, two=2} did not contain key \"twenty two\"" or
          be === "{two=2, one=1} did not contain key \"fifty five\", and {two=2, one=1} did not contain key \"twenty two\"")
      }

      def `should throw an TestFailedException when map contains specified key and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          javaMap should { not { contain key ("three") } and not { contain key ("two") }}
        }
        caught1.getMessage should (be === "{one=1, two=2} did not contain key \"three\", but {one=1, two=2} contained key \"two\"" or
          be === "{two=2, one=1} did not contain key \"three\", but {two=2, one=1} contained key \"two\"")

        val caught2 = intercept[TestFailedException] {
          javaMap should ((not contain key ("three")) and (not contain key ("two")))
        }
        caught2.getMessage should (be === "{one=1, two=2} did not contain key \"three\", but {one=1, two=2} contained key \"two\"" or
          be === "{two=2, one=1} did not contain key \"three\", but {two=2, one=1} contained key \"two\"")

        val caught3 = intercept[TestFailedException] {
          javaMap should (not contain key ("three") and not contain key ("two"))
        }
        caught3.getMessage should (be === "{one=1, two=2} did not contain key \"three\", but {one=1, two=2} contained key \"two\"" or
          be === "{two=2, one=1} did not contain key \"three\", but {two=2, one=1} contained key \"two\"")
      }

      def `should throw an TestFailedException when map contains specified key and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          javaMap should { not { contain key ("two") } or not { contain key ("two") }}
        }
        caught1.getMessage should (be === "{one=1, two=2} contained key \"two\", and {one=1, two=2} contained key \"two\"" or
          be === "{two=2, one=1} contained key \"two\", and {two=2, one=1} contained key \"two\"")

        val caught2 = intercept[TestFailedException] {
          javaMap should ((not contain key ("two")) or (not contain key ("two")))
        }
        caught2.getMessage should (be === "{one=1, two=2} contained key \"two\", and {one=1, two=2} contained key \"two\"" or
          be === "{two=2, one=1} contained key \"two\", and {two=2, one=1} contained key \"two\"")

        val caught3 = intercept[TestFailedException] {
          javaMap should (not contain key ("two") or not contain key ("two"))
        }
        caught3.getMessage should (be === "{one=1, two=2} contained key \"two\", and {one=1, two=2} contained key \"two\"" or
          be === "{two=2, one=1} contained key \"two\", and {two=2, one=1} contained key \"two\"")
      }
    }
  }
}
