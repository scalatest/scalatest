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

import SharedHelpers.thisLineNumber

class ShouldBeSortedSpec extends Spec with Matchers {
  
  def wasNotSorted(left: Any): String = 
    left + " was not sorted"
  
  object `Sorted matcher` {
    
    object `when work with 'should be'` {
      
      def `should do nothing for 'xs should be (sorted)' when xs is sorted` {
        List(1, 2, 3) should be (sorted)
      }
      
      def `should throw TestFailedException with correct stack depth for 'xs should be (sorted)' when xs is not sorted` {
        val list = List(2, 1, 3)
        val caught = intercept[TestFailedException] {
          list should be (sorted)
        }
        assert(caught.message === Some(wasNotSorted(list)))
        assert(caught.failedCodeFileName === Some("ShouldBeSortedSpec.scala"))
        assert(caught.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
    }
    
  }
  
}