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

import org.scalatest._
import SharedHelpers.createTempDirectory
import java.io.File

class ExistWordSpec extends Spec with Matchers {
  
  object `ExistWord ` {
    
    val existWord = new ExistWord
    
    object `matcherFactory produces Matcher that` {
      
      val mtf = existWord.matcherFactory
      val mt = mtf.matcher[File]
      
      def `should have pretty toString` {
        mtf.toString should be ("exist")
        mt.toString should be ("exist")
      }
      
      val tempDir = createTempDirectory()
      val lhs = File.createTempFile("delete", "me", tempDir)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (lhs + " does not exist"),
          'negatedFailureMessage (lhs + " exists"),
          'midSentenceFailureMessage (lhs + " does not exist"),
          'midSentenceNegatedFailureMessage (lhs + " exists"),
          'rawFailureMessage ("{0} does not exist"),
          'rawNegatedFailureMessage ("{0} exists"),
          'rawMidSentenceFailureMessage ("{0} does not exist"),
          'rawMidSentenceNegatedFailureMessage ("{0} exists"),
          'failureMessageArgs(Vector(lhs)),
          'negatedFailureMessageArgs(Vector(lhs)),
          'midSentenceFailureMessageArgs(Vector(lhs)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (lhs + " exists"),
          'negatedFailureMessage (lhs + " does not exist"),
          'midSentenceFailureMessage (lhs + " exists"),
          'midSentenceNegatedFailureMessage (lhs + " does not exist"),
          'rawFailureMessage ("{0} exists"),
          'rawNegatedFailureMessage ("{0} does not exist"),
          'rawMidSentenceFailureMessage ("{0} exists"),
          'rawMidSentenceNegatedFailureMessage ("{0} does not exist"),
          'failureMessageArgs(Vector(lhs)),
          'negatedFailureMessageArgs(Vector(lhs)),
          'midSentenceFailureMessageArgs(Vector(lhs)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs))    
        )
      }
    }
  }
  
}