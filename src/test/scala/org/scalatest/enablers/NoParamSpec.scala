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
package org.scalatest.enablers

import org.scalatest._
import org.scalactic.Equality
import scala.collection.immutable

class NoParamSpec extends Spec with Matchers with LoneElement {

  object `The implicit Containing providers` {
    def `should work with no-param collection types and default equality` {
      ConfigMap("hi" -> 1, "ho" -> "two") should contain ("hi" -> 1)
    }
    def `should be overridable with something that takes a specific equality` {
      implicit val inverseEquality = 
        new Equality[(String, Any)] {
          def areEqual(a: (String, Any), b: Any): Boolean = a != b
        }
      ConfigMap("hi" -> 1) should not contain ("hi" -> 1)
    }
  }
  object `The implicit Aggregating providers` {
    def `should work with no-param collection types and default equality` {
      ConfigMap("hi" -> 1, "ho" -> "two") should contain allOf ("hi" -> 1, "ho" -> "two")
    }
    def `should be overridable with something that takes a specific equality` {
      implicit val inverseEquality = 
        new Equality[(String, Any)] {
          def areEqual(a: (String, Any), b: Any): Boolean = a != b
        }
      ConfigMap("hi" -> 1) should not contain allOf ("hi" -> 1, "ho" -> "two")
    }
  }
  object `The implicit KeyMapping providers` {
    def `should work with no-param collection types and default equality` {
      ConfigMap("hi" -> 1, "ho" -> "two") should contain key ("hi")
    }
    def `should be overridable with something that takes a specific equality` {
      implicit val inverseEquality = 
        new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
      ConfigMap("hi" -> 1) should not contain key ("hi")
    }
  }
  object `The implicit ValueMapping providers` {
    def `should work with no-param collection types and default equality` {
      ConfigMap("hi" -> 1, "ho" -> "two") should contain value (1)
    }
    def `should be overridable with something that takes a specific equality` {
      implicit val inverseEquality = 
        new Equality[Any] {
          def areEqual(a: Any, b: Any): Boolean = a != b
        }
      ConfigMap("hi" -> 1) should not contain value (1)
    }
  }
  class MyStringSeq(underlying: immutable.Seq[String]) extends immutable.Seq[String] {
    def iterator: Iterator[String] = underlying.iterator
    def length: Int = underlying.length
    def apply(idx: Int): String = underlying(idx)
  }
  object MyStringSeq {
    def apply(args: String*): MyStringSeq = new MyStringSeq(immutable.Seq.empty[String] ++ args)
  }
  object `The implicit Sequencing providers` {
    def `should work with no-param collection types and default equality` {
      MyStringSeq("hi", "ho", "hey") should contain inOrder ("hi", "hey")
    }
    def `should be overridable with something that takes a specific equality` {
      implicit val inverseEquality = 
        new Equality[String] {
          def areEqual(a: String, b: Any): Boolean = a != b
        }
      MyStringSeq("hi", "ho") should not contain inOrder ("hi", "ho")
    }
  }
  object `The implicit Collecting providers` { // I am not sure why this one already works without needing to make Collecting contravariant in C
    def `should work with no-param collection types` {
      MyStringSeq("hi").loneElement should be ("hi")
      intercept[TestFailedException] {
        MyStringSeq("hi", "ho").loneElement
      }
      ConfigMap("hi" -> 1).loneElement should be ("hi" -> 1)
      intercept[TestFailedException] {
        ConfigMap("hi" -> 1, "ho" -> 2).loneElement
      }
    }
  }
  object `The implicit Sortable providers` {
    def `should work with no-param collection types` {
      MyStringSeq("hey", "hi", "ho") should be (sorted)
      MyStringSeq("hi", "di", "ho") should not be sorted
    }
  }
}
