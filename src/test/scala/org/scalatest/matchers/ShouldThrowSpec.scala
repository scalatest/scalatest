/*
 * Copyright 2001-2009 Artima, Inc.
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
import org.scalatest.exceptions.TestFailedException

class ShouldThrowSpec extends Spec with OptionValues with ShouldMatchers {

  object `The evaluating { ... } should produce [ExceptionType] syntax` {

    def `fail if a different exception is thrown` {
      val caught1 = intercept[TestFailedException] {
        evaluating { "hi".charAt(-1) } should produce [IllegalArgumentException]
      }
      assert(caught1.getMessage === "Expected exception java.lang.IllegalArgumentException to be thrown, but java.lang.StringIndexOutOfBoundsException was thrown.")
    }

    def `fail if no exception is thrown` {
      val caught2 = intercept[TestFailedException] {
        evaluating { "hi" } should produce [IllegalArgumentException]
      }
      assert(caught2.getMessage === "Expected exception java.lang.IllegalArgumentException to be thrown, but no exception was thrown.")
    }

    def `succeed if the expected exception is thrown` {
      evaluating { "hi".charAt(-1) } should produce [StringIndexOutOfBoundsException]
    }
    
    def `succeed if a subtype of the expected exception is thrown, where the expected type is a class` {
      evaluating { "hi".charAt(-1) } should produce [Exception]
    }

    def `succeed if a subtype of the expected exception is thrown, where the expected type is a trait` {
      trait Excitement
      def kaboom() { throw new Exception with Excitement }
      evaluating { kaboom() } should produce [Excitement]
    }
    
    def `return the caught exception` {
      def kaboom() { throw new Exception("howdy") }
      val thrown = evaluating { kaboom() } should produce [Exception]
      thrown.getMessage should be === "howdy"
    }  
    
    def `include that wrong exception as the TFE's cause` {
      val wrongException = new RuntimeException("oops!")
      val caught =
        intercept[TestFailedException] {
          evaluating { throw wrongException } should produce [IllegalArgumentException]
        }
      assert(caught.cause.value eq wrongException)
    }
  }
}
