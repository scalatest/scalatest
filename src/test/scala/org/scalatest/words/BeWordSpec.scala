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
import Matchers._
import matchers.{BePropertyMatcher, 
                 BePropertyMatchResult, 
                 AMatcher, 
                 AnMatcher, 
                 BeMatcher, 
                 MatchResult}

class BeWordSpec extends Spec with FileMocks {
  
  object `BeWord ` {
    
    def `should have pretty toString` {
      be.toString should be ("be")
    }
    
    object `< method returns Matcher` {
      
      val mt = be < 3
      
      def `should have pretty toString` {
        mt.toString should be ("be < 3")
      }
      
      val mr = mt(0)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("0 was not less than 3"),
          'negatedFailureMessage ("0 was less than 3"),
          'midSentenceFailureMessage ("0 was not less than 3"),
          'midSentenceNegatedFailureMessage ("0 was less than 3"),
          'rawFailureMessage ("{0} was not less than {1}"),
          'rawNegatedFailureMessage ("{0} was less than {1}"),
          'rawMidSentenceFailureMessage ("{0} was not less than {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was less than {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("0 was less than 3"),
          'negatedFailureMessage ("0 was not less than 3"),
          'midSentenceFailureMessage ("0 was less than 3"),
          'midSentenceNegatedFailureMessage ("0 was not less than 3"),
          'rawFailureMessage ("{0} was less than {1}"),
          'rawNegatedFailureMessage ("{0} was not less than {1}"),
          'rawMidSentenceFailureMessage ("{0} was less than {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not less than {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )
      }
    }
    
    object `> method returns Matcher` {
      
      val mt = be > 3
      
      def `should have pretty toString` {
        mt.toString should be ("be > 3")
      }
      
      val mr = mt(0)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("0 was not greater than 3"),
          'negatedFailureMessage ("0 was greater than 3"),
          'midSentenceFailureMessage ("0 was not greater than 3"),
          'midSentenceNegatedFailureMessage ("0 was greater than 3"),
          'rawFailureMessage ("{0} was not greater than {1}"),
          'rawNegatedFailureMessage ("{0} was greater than {1}"),
          'rawMidSentenceFailureMessage ("{0} was not greater than {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was greater than {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("0 was greater than 3"),
          'negatedFailureMessage ("0 was not greater than 3"),
          'midSentenceFailureMessage ("0 was greater than 3"),
          'midSentenceNegatedFailureMessage ("0 was not greater than 3"),
          'rawFailureMessage ("{0} was greater than {1}"),
          'rawNegatedFailureMessage ("{0} was not greater than {1}"),
          'rawMidSentenceFailureMessage ("{0} was greater than {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not greater than {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )
      }
    }
    
    object `<= method returns Matcher` {
      
      val mt = be <= 3
      
      def `should have pretty toString` {
        mt.toString should be ("be <= 3")
      }
      
      val mr = mt(0)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("0 was not less than or equal to 3"),
          'negatedFailureMessage ("0 was less than or equal to 3"),
          'midSentenceFailureMessage ("0 was not less than or equal to 3"),
          'midSentenceNegatedFailureMessage ("0 was less than or equal to 3"),
          'rawFailureMessage ("{0} was not less than or equal to {1}"),
          'rawNegatedFailureMessage ("{0} was less than or equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not less than or equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was less than or equal to {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("0 was less than or equal to 3"),
          'negatedFailureMessage ("0 was not less than or equal to 3"),
          'midSentenceFailureMessage ("0 was less than or equal to 3"),
          'midSentenceNegatedFailureMessage ("0 was not less than or equal to 3"),
          'rawFailureMessage ("{0} was less than or equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not less than or equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was less than or equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not less than or equal to {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )
      }
    }
    
    object `>= method returns Matcher` {
      
      val mt = be >= 3
      
      def `should have pretty toString` {
        mt.toString should be ("be >= 3")
      }
      
      val mr = mt(0)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("0 was not greater than or equal to 3"),
          'negatedFailureMessage ("0 was greater than or equal to 3"),
          'midSentenceFailureMessage ("0 was not greater than or equal to 3"),
          'midSentenceNegatedFailureMessage ("0 was greater than or equal to 3"),
          'rawFailureMessage ("{0} was not greater than or equal to {1}"),
          'rawNegatedFailureMessage ("{0} was greater than or equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not greater than or equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was greater than or equal to {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("0 was greater than or equal to 3"),
          'negatedFailureMessage ("0 was not greater than or equal to 3"),
          'midSentenceFailureMessage ("0 was greater than or equal to 3"),
          'midSentenceNegatedFailureMessage ("0 was not greater than or equal to 3"),
          'rawFailureMessage ("{0} was greater than or equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not greater than or equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was greater than or equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not greater than or equal to {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )
      }
    }
    
    object `=== method returns Matcher` {
      
      val mt = be === "cheese"
      
      def `should have pretty toString` {
        mt.toString should be ("be === \"cheese\"")
      }
      
      val mr = mt("chese")
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("\"che[]se\" was not equal to \"che[e]se\""),
          'negatedFailureMessage ("\"chese\" was equal to \"cheese\""),
          'midSentenceFailureMessage ("\"che[]se\" was not equal to \"che[e]se\""),
          'midSentenceNegatedFailureMessage ("\"chese\" was equal to \"cheese\""),
          'rawFailureMessage ("{0} was not equal to {1}"),
          'rawNegatedFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was equal to {1}"),
          'failureMessageArgs(Vector("che[]se", "che[e]se")),
          'negatedFailureMessageArgs(Vector("chese", "cheese")),
          'midSentenceFailureMessageArgs(Vector("che[]se", "che[e]se")),
          'midSentenceNegatedFailureMessageArgs(Vector("chese", "cheese"))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("\"chese\" was equal to \"cheese\""),
          'negatedFailureMessage ("\"che[]se\" was not equal to \"che[e]se\""),
          'midSentenceFailureMessage ("\"chese\" was equal to \"cheese\""),
          'midSentenceNegatedFailureMessage ("\"che[]se\" was not equal to \"che[e]se\""),
          'rawFailureMessage ("{0} was equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not equal to {1}"),
          'failureMessageArgs(Vector("chese", "cheese")),
          'negatedFailureMessageArgs(Vector("che[]se", "che[e]se")),
          'midSentenceFailureMessageArgs(Vector("chese", "cheese")),
          'midSentenceNegatedFailureMessageArgs(Vector("che[]se", "che[e]se"))
        )
      }
    }
    
    object `a(Symbol) method returns Matcher` {
      val mt = be a ('file)
      
      def `should have pretty toString` {
        mt.toString should be ("be a 'file")
      }
      
      val mr = mt(fileMock)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (fileMock + " was not a file"),
          'negatedFailureMessage (fileMock + " was a file"),
          'midSentenceFailureMessage (fileMock + " was not a file"),
          'midSentenceNegatedFailureMessage (fileMock + " was a file"),
          'rawFailureMessage ("{0} was not a {1}"),
          'rawNegatedFailureMessage ("{0} was a {1}"),
          'rawMidSentenceFailureMessage ("{0} was not a {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was a {1}"),
          'failureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(fileMock, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (fileMock + " was a file"),
          'negatedFailureMessage (fileMock + " was not a file"),
          'midSentenceFailureMessage (fileMock + " was a file"),
          'midSentenceNegatedFailureMessage (fileMock + " was not a file"),
          'rawFailureMessage ("{0} was a {1}"),
          'rawNegatedFailureMessage ("{0} was not a {1}"),
          'rawMidSentenceFailureMessage ("{0} was a {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not a {1}"),
          'failureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(fileMock, UnquotedString("file")))    
        )
      }
    }
    
    object `a(BePropertyMatcher) method returns Matcher` {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )

      class FileBePropertyMatcher extends BePropertyMatcher[MyFile] {
        def apply(file: MyFile) = {
          new BePropertyMatchResult(file.file, "file")
        }
      }
      
      val myFile = MyFile("test", true, false)
      val file = new FileBePropertyMatcher
      
      val mt = be a (file)
      
      def `should have pretty toString` {
        mt.toString should be ("be a " + file)
      }
      
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (myFile + " was not a file"),
          'negatedFailureMessage (myFile + " was a file"),
          'midSentenceFailureMessage (myFile + " was not a file"),
          'midSentenceNegatedFailureMessage (myFile + " was a file"),
          'rawFailureMessage ("{0} was not a {1}"),
          'rawNegatedFailureMessage ("{0} was a {1}"),
          'rawMidSentenceFailureMessage ("{0} was not a {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was a {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (myFile + " was a file"),
          'negatedFailureMessage (myFile + " was not a file"),
          'midSentenceFailureMessage (myFile + " was a file"),
          'midSentenceNegatedFailureMessage (myFile + " was not a file"),
          'rawFailureMessage ("{0} was a {1}"),
          'rawNegatedFailureMessage ("{0} was not a {1}"),
          'rawMidSentenceFailureMessage ("{0} was a {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not a {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
    }
    
    object `a(AMatcher) method returns Matcher` {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val file = AMatcher[MyFile]("file") { _.file  }
      val myFile = MyFile("test", true, false)
      
      val mt = be a (file)
      
      def `should have pretty toString` {
        mt.toString should be ("be a AMatcher[" + classOf[MyFile].getName + "](\"file\", " + classOf[MyFile].getName + " => Boolean)")
      }
      
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (myFile + " was not a file"),
          'negatedFailureMessage (myFile + " was a file"),
          'midSentenceFailureMessage (myFile + " was not a file"),
          'midSentenceNegatedFailureMessage (myFile + " was a file"),
          'rawFailureMessage ("{0} was not a {1}"),
          'rawNegatedFailureMessage ("{0} was a {1}"),
          'rawMidSentenceFailureMessage ("{0} was not a {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was a {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (myFile + " was a file"),
          'negatedFailureMessage (myFile + " was not a file"),
          'midSentenceFailureMessage (myFile + " was a file"),
          'midSentenceNegatedFailureMessage (myFile + " was not a file"),
          'rawFailureMessage ("{0} was a {1}"),
          'rawNegatedFailureMessage ("{0} was not a {1}"),
          'rawMidSentenceFailureMessage ("{0} was a {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not a {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
    }
    
    object `an(Symbol) method returns Matcher` {
      val mt = be an ('file)
      
      def `should have pretty toString` {
        mt.toString should be ("be an 'file")
      }
      
      val mr = mt(fileMock)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (fileMock + " was not an file"),
          'negatedFailureMessage (fileMock + " was an file"),
          'midSentenceFailureMessage (fileMock + " was not an file"),
          'midSentenceNegatedFailureMessage (fileMock + " was an file"),
          'rawFailureMessage ("{0} was not an {1}"),
          'rawNegatedFailureMessage ("{0} was an {1}"),
          'rawMidSentenceFailureMessage ("{0} was not an {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was an {1}"),
          'failureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(fileMock, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (fileMock + " was an file"),
          'negatedFailureMessage (fileMock + " was not an file"),
          'midSentenceFailureMessage (fileMock + " was an file"),
          'midSentenceNegatedFailureMessage (fileMock + " was not an file"),
          'rawFailureMessage ("{0} was an {1}"),
          'rawNegatedFailureMessage ("{0} was not an {1}"),
          'rawMidSentenceFailureMessage ("{0} was an {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not an {1}"),
          'failureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(fileMock, UnquotedString("file")))    
        )
      }
    }
    
    object `an(BePropertyMatcher) method returns Matcher` {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )

      class FileBePropertyMatcher extends BePropertyMatcher[MyFile] {
        def apply(file: MyFile) = {
          new BePropertyMatchResult(file.file, "file")
        }
      }
      
      val myFile = MyFile("test", true, false)
      val file = new FileBePropertyMatcher
      
      val mt = be an (file)
      
      def `should have pretty toString` {
        mt.toString should be ("be an " + file)
      }
      
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (myFile + " was not an file"),
          'negatedFailureMessage (myFile + " was an file"),
          'midSentenceFailureMessage (myFile + " was not an file"),
          'midSentenceNegatedFailureMessage (myFile + " was an file"),
          'rawFailureMessage ("{0} was not an {1}"),
          'rawNegatedFailureMessage ("{0} was an {1}"),
          'rawMidSentenceFailureMessage ("{0} was not an {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was an {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (myFile + " was an file"),
          'negatedFailureMessage (myFile + " was not an file"),
          'midSentenceFailureMessage (myFile + " was an file"),
          'midSentenceNegatedFailureMessage (myFile + " was not an file"),
          'rawFailureMessage ("{0} was an {1}"),
          'rawNegatedFailureMessage ("{0} was not an {1}"),
          'rawMidSentenceFailureMessage ("{0} was an {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not an {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
    }
    
    object `an(AnMatcher) method returns Matcher` {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val file = AnMatcher[MyFile]("file") { _.file  }
      val myFile = MyFile("test", true, false)
      
      val mt = be an (file)
      
      def `should have pretty toString` {
        mt.toString should be ("be an AnMatcher[" + classOf[MyFile].getName + "](\"file\", " + classOf[MyFile].getName + " => Boolean)")
      }
      
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (myFile + " was not an file"),
          'negatedFailureMessage (myFile + " was an file"),
          'midSentenceFailureMessage (myFile + " was not an file"),
          'midSentenceNegatedFailureMessage (myFile + " was an file"),
          'rawFailureMessage ("{0} was not an {1}"),
          'rawNegatedFailureMessage ("{0} was an {1}"),
          'rawMidSentenceFailureMessage ("{0} was not an {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was an {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (myFile + " was an file"),
          'negatedFailureMessage (myFile + " was not an file"),
          'midSentenceFailureMessage (myFile + " was an file"),
          'midSentenceNegatedFailureMessage (myFile + " was not an file"),
          'rawFailureMessage ("{0} was an {1}"),
          'rawNegatedFailureMessage ("{0} was not an {1}"),
          'rawMidSentenceFailureMessage ("{0} was an {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not an {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
    }
    
    object `apply(Spread) method returns Matcher` {
      val spread = 7.1 +- 0.2
      val mt = be (spread)
      
      def `should have pretty toString` {
        mt.toString should be ("be (7.1 +- 0.2)")
      }
      
      val mr = mt(7.0)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("7.0 was not 7.1 plus or minus 0.2"),
          'negatedFailureMessage ("7.0 was 7.1 plus or minus 0.2"),
          'midSentenceFailureMessage ("7.0 was not 7.1 plus or minus 0.2"),
          'midSentenceNegatedFailureMessage ("7.0 was 7.1 plus or minus 0.2"),
          'rawFailureMessage ("{0} was not {1} plus or minus {2}"),
          'rawNegatedFailureMessage ("{0} was {1} plus or minus {2}"),
          'rawMidSentenceFailureMessage ("{0} was not {1} plus or minus {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was {1} plus or minus {2}"),
          'failureMessageArgs(Vector(7.0, 7.1, 0.2)),
          'negatedFailureMessageArgs(Vector(7.0, 7.1, 0.2)),
          'midSentenceFailureMessageArgs(Vector(7.0, 7.1, 0.2)),
          'midSentenceNegatedFailureMessageArgs(Vector(7.0, 7.1, 0.2))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("7.0 was 7.1 plus or minus 0.2"),
          'negatedFailureMessage ("7.0 was not 7.1 plus or minus 0.2"),
          'midSentenceFailureMessage ("7.0 was 7.1 plus or minus 0.2"),
          'midSentenceNegatedFailureMessage ("7.0 was not 7.1 plus or minus 0.2"),
          'rawFailureMessage ("{0} was {1} plus or minus {2}"),
          'rawNegatedFailureMessage ("{0} was not {1} plus or minus {2}"),
          'rawMidSentenceFailureMessage ("{0} was {1} plus or minus {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not {1} plus or minus {2}"),
          'failureMessageArgs(Vector(7.0, 7.1, 0.2)),
          'negatedFailureMessageArgs(Vector(7.0, 7.1, 0.2)),
          'midSentenceFailureMessageArgs(Vector(7.0, 7.1, 0.2)),
          'midSentenceNegatedFailureMessageArgs(Vector(7.0, 7.1, 0.2))    
        )
      }
    }
    
    object `theSameInstanceAs(AnyRef) method returns Matcher` {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val myFileLeft = MyFile("left", true, false)
      val myFileRight = MyFile("right", true, false)
      
      val mt = be theSameInstanceAs (myFileRight)
      
      def `should have pretty toString` {
        mt.toString should be ("be theSameInstanceAs " + myFileRight)
      }
      
      val mr = mt(myFileLeft)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (myFileLeft + " was not the same instance as " + myFileRight),
          'negatedFailureMessage (myFileLeft + " was the same instance as " + myFileRight),
          'midSentenceFailureMessage (myFileLeft + " was not the same instance as " + myFileRight),
          'midSentenceNegatedFailureMessage (myFileLeft + " was the same instance as " + myFileRight),
          'rawFailureMessage ("{0} was not the same instance as {1}"),
          'rawNegatedFailureMessage ("{0} was the same instance as {1}"),
          'rawMidSentenceFailureMessage ("{0} was not the same instance as {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was the same instance as {1}"),
          'failureMessageArgs(Vector(myFileLeft, myFileRight)),
          'negatedFailureMessageArgs(Vector(myFileLeft, myFileRight)),
          'midSentenceFailureMessageArgs(Vector(myFileLeft, myFileRight)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFileLeft, myFileRight))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (myFileLeft + " was the same instance as " + myFileRight),
          'negatedFailureMessage (myFileLeft + " was not the same instance as " + myFileRight),
          'midSentenceFailureMessage (myFileLeft + " was the same instance as " + myFileRight),
          'midSentenceNegatedFailureMessage (myFileLeft + " was not the same instance as " + myFileRight),
          'rawFailureMessage ("{0} was the same instance as {1}"),
          'rawNegatedFailureMessage ("{0} was not the same instance as {1}"),
          'rawMidSentenceFailureMessage ("{0} was the same instance as {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not the same instance as {1}"),
          'failureMessageArgs(Vector(myFileLeft, myFileRight)),
          'negatedFailureMessageArgs(Vector(myFileLeft, myFileRight)),
          'midSentenceFailureMessageArgs(Vector(myFileLeft, myFileRight)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFileLeft, myFileRight))    
        )
      }
    }
    
    object `apply(Boolean) method returns Matcher` {
      val mt = be (true)
      
      def `should have pretty toString` {
        mt.toString should be ("be (true)")
      }
      
      val mr = mt(true)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("true was not true"),
          'negatedFailureMessage ("true was true"),
          'midSentenceFailureMessage ("true was not true"),
          'midSentenceNegatedFailureMessage ("true was true"),
          'rawFailureMessage ("{0} was not {1}"),
          'rawNegatedFailureMessage ("{0} was {1}"),
          'rawMidSentenceFailureMessage ("{0} was not {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was {1}"),
          'failureMessageArgs(Vector(true, true)),
          'negatedFailureMessageArgs(Vector(true, true)),
          'midSentenceFailureMessageArgs(Vector(true, true)),
          'midSentenceNegatedFailureMessageArgs(Vector(true, true))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("true was true"),
          'negatedFailureMessage ("true was not true"),
          'midSentenceFailureMessage ("true was true"),
          'midSentenceNegatedFailureMessage ("true was not true"),
          'rawFailureMessage ("{0} was {1}"),
          'rawNegatedFailureMessage ("{0} was not {1}"),
          'rawMidSentenceFailureMessage ("{0} was {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not {1}"),
          'failureMessageArgs(Vector(true, true)),
          'negatedFailureMessageArgs(Vector(true, true)),
          'midSentenceFailureMessageArgs(Vector(true, true)),
          'midSentenceNegatedFailureMessageArgs(Vector(true, true))    
        )
      }
    }
    
    object `apply(Null) method returns Matcher` {
      val mt = be (null)
      
      def `should have pretty toString` {
        mt.toString should be ("be (null)")
      }
      
      val aString = "something"
      
      val mr = mt(aString)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("\"something\" was not null"),
          'negatedFailureMessage ("The reference was null"),
          'midSentenceFailureMessage ("\"something\" was not null"),
          'midSentenceNegatedFailureMessage ("the reference was null"),
          'rawFailureMessage ("{0} was not null"),
          'rawNegatedFailureMessage ("The reference was null"),
          'rawMidSentenceFailureMessage ("{0} was not null"),
          'rawMidSentenceNegatedFailureMessage ("the reference was null"),
          'failureMessageArgs(Vector(aString)),
          'negatedFailureMessageArgs(Vector.empty),
          'midSentenceFailureMessageArgs(Vector(aString)),
          'midSentenceNegatedFailureMessageArgs(Vector.empty)    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("The reference was null"),
          'negatedFailureMessage ("\"something\" was not null"),
          'midSentenceFailureMessage ("the reference was null"),
          'midSentenceNegatedFailureMessage ("\"something\" was not null"),
          'rawFailureMessage ("The reference was null"),
          'rawNegatedFailureMessage ("{0} was not null"),
          'rawMidSentenceFailureMessage ("the reference was null"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not null"),
          'failureMessageArgs(Vector.empty),
          'negatedFailureMessageArgs(Vector(aString)),
          'midSentenceFailureMessageArgs(Vector.empty),
          'midSentenceNegatedFailureMessageArgs(Vector(aString))    
        )
      }
    }
    
    object `apply(Symbol) method returns Matcher` {
      val mt = be ('file)
      
      def `should have pretty toString` {
        mt.toString should be ("be ('file)")
      }
      
      val mr = mt(fileMock)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (fileMock + " was not file"),
          'negatedFailureMessage (fileMock + " was file"),
          'midSentenceFailureMessage (fileMock + " was not file"),
          'midSentenceNegatedFailureMessage (fileMock + " was file"),
          'rawFailureMessage ("{0} was not {1}"),
          'rawNegatedFailureMessage ("{0} was {1}"),
          'rawMidSentenceFailureMessage ("{0} was not {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was {1}"),
          'failureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(fileMock, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (fileMock + " was file"),
          'negatedFailureMessage (fileMock + " was not file"),
          'midSentenceFailureMessage (fileMock + " was file"),
          'midSentenceNegatedFailureMessage (fileMock + " was not file"),
          'rawFailureMessage ("{0} was {1}"),
          'rawNegatedFailureMessage ("{0} was not {1}"),
          'rawMidSentenceFailureMessage ("{0} was {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not {1}"),
          'failureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(fileMock, UnquotedString("file")))    
        )
      }
    }
    
    object `apply(BeMatcher) method returns Matcher` {
      class OddMatcher extends BeMatcher[Int] {
        def apply(left: Int): MatchResult = {
          MatchResult(
            left % 2 == 1,
            left.toString + " was even",
            left.toString + " was odd", 
            Vector(left)
          )
        }
      }
      val odd = new OddMatcher
      
      val mt = be (odd)
      
      def `should have pretty toString` {
        mt.toString should be ("be (" + odd + ")")
      }
      
      val mr = mt(1)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("1 was even"),
          'negatedFailureMessage ("1 was odd"),
          'midSentenceFailureMessage ("1 was even"),
          'midSentenceNegatedFailureMessage ("1 was odd"),
          'rawFailureMessage ("1 was even"),
          'rawNegatedFailureMessage ("1 was odd"),
          'rawMidSentenceFailureMessage ("1 was even"),
          'rawMidSentenceNegatedFailureMessage ("1 was odd"),
          'failureMessageArgs(Vector(1)),
          'negatedFailureMessageArgs(Vector(1)),
          'midSentenceFailureMessageArgs(Vector(1)),
          'midSentenceNegatedFailureMessageArgs(Vector(1))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("1 was odd"),
          'negatedFailureMessage ("1 was even"),
          'midSentenceFailureMessage ("1 was odd"),
          'midSentenceNegatedFailureMessage ("1 was even"),
          'rawFailureMessage ("1 was odd"),
          'rawNegatedFailureMessage ("1 was even"),
          'rawMidSentenceFailureMessage ("1 was odd"),
          'rawMidSentenceNegatedFailureMessage ("1 was even"),
          'failureMessageArgs(Vector(1)),
          'negatedFailureMessageArgs(Vector(1)),
          'midSentenceFailureMessageArgs(Vector(1)),
          'midSentenceNegatedFailureMessageArgs(Vector(1))    
        )
      }
    }
    
    object `apply(BePropertyMatcher) method returns Matcher` {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )

      class FileBePropertyMatcher extends BePropertyMatcher[MyFile] {
        def apply(file: MyFile) = {
          new BePropertyMatchResult(file.file, "file")
        }
      }
      
      val myFile = MyFile("test", true, false)
      val file = new FileBePropertyMatcher
      
      val mt = be (file)
      
      def `should have pretty toString` {
        mt.toString should be ("be (" + file + ")")
      }
      
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (myFile + " was not file"),
          'negatedFailureMessage (myFile + " was file"),
          'midSentenceFailureMessage (myFile + " was not file"),
          'midSentenceNegatedFailureMessage (myFile + " was file"),
          'rawFailureMessage ("{0} was not {1}"),
          'rawNegatedFailureMessage ("{0} was {1}"),
          'rawMidSentenceFailureMessage ("{0} was not {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (myFile + " was file"),
          'negatedFailureMessage (myFile + " was not file"),
          'midSentenceFailureMessage (myFile + " was file"),
          'midSentenceNegatedFailureMessage (myFile + " was not file"),
          'rawFailureMessage ("{0} was {1}"),
          'rawNegatedFailureMessage ("{0} was not {1}"),
          'rawMidSentenceFailureMessage ("{0} was {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
    }
    
    object `apply(Any) method returns Matcher` {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )

      val myFileLeft = MyFile("test left", true, false)
      val myFileRight = MyFile("test right", true, false)
      
      val mt = be (myFileRight)
      
      def `should have pretty toString` {
        mt.toString should be ("be (" + myFileRight + ")")
      }
      
      val mr = mt(myFileLeft)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (myFileLeft + " was not equal to " + myFileRight),
          'negatedFailureMessage (myFileLeft + " was equal to " + myFileRight),
          'midSentenceFailureMessage (myFileLeft + " was not equal to " + myFileRight),
          'midSentenceNegatedFailureMessage (myFileLeft + " was equal to " + myFileRight),
          'rawFailureMessage ("{0} was not equal to {1}"),
          'rawNegatedFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was equal to {1}"),
          'failureMessageArgs(Vector(myFileLeft, myFileRight)),
          'negatedFailureMessageArgs(Vector(myFileLeft, myFileRight)),
          'midSentenceFailureMessageArgs(Vector(myFileLeft, myFileRight)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFileLeft, myFileRight))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (myFileLeft + " was equal to " + myFileRight),
          'negatedFailureMessage (myFileLeft + " was not equal to " + myFileRight),
          'midSentenceFailureMessage (myFileLeft + " was equal to " + myFileRight),
          'midSentenceNegatedFailureMessage (myFileLeft + " was not equal to " + myFileRight),
          'rawFailureMessage ("{0} was equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not equal to {1}"),
          'failureMessageArgs(Vector(myFileLeft, myFileRight)),
          'negatedFailureMessageArgs(Vector(myFileLeft, myFileRight)),
          'midSentenceFailureMessageArgs(Vector(myFileLeft, myFileRight)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFileLeft, myFileRight))    
        )
      }
    }
    
    object `apply(SortedWord) method returns MatcherFactory` {
      
      val mtf = be (sorted)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("be (sorted)")
        mt.toString should be ("be (sorted)")
      }
      
      val leftList = List(1, 2, 3)
      val mr = mt(leftList)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (leftList + " was not sorted"),
          'negatedFailureMessage (leftList + " was sorted"),
          'midSentenceFailureMessage (leftList + " was not sorted"),
          'midSentenceNegatedFailureMessage (leftList + " was sorted"),
          'rawFailureMessage ("{0} was not sorted"),
          'rawNegatedFailureMessage ("{0} was sorted"),
          'rawMidSentenceFailureMessage ("{0} was not sorted"),
          'rawMidSentenceNegatedFailureMessage ("{0} was sorted"),
          'failureMessageArgs(Vector(leftList)),
          'negatedFailureMessageArgs(Vector(leftList)),
          'midSentenceFailureMessageArgs(Vector(leftList)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (leftList + " was sorted"),
          'negatedFailureMessage (leftList + " was not sorted"),
          'midSentenceFailureMessage (leftList + " was sorted"),
          'midSentenceNegatedFailureMessage (leftList + " was not sorted"),
          'rawFailureMessage ("{0} was sorted"),
          'rawNegatedFailureMessage ("{0} was not sorted"),
          'rawMidSentenceFailureMessage ("{0} was sorted"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not sorted"),
          'failureMessageArgs(Vector(leftList)),
          'negatedFailureMessageArgs(Vector(leftList)),
          'midSentenceFailureMessageArgs(Vector(leftList)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList))    
        )
      }
    }
    
    object `definedAt(PartialFunction) method returns Matcher` {
      
      val fraction = new PartialFunction[Int, Int] {
        def apply(d: Int) = 42 / d
        def isDefinedAt(d: Int) = d != 0
      }
      
      val mt = be definedAt (8)
      
      def `should have pretty toString` {
        mt.toString should be ("be definedAt 8")
      }
      
      val mr = mt(fraction)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (fraction + " was not defined at 8"),
          'negatedFailureMessage (fraction + " was defined at 8"),
          'midSentenceFailureMessage (fraction + " was not defined at 8"),
          'midSentenceNegatedFailureMessage (fraction + " was defined at 8"),
          'rawFailureMessage ("{0} was not defined at {1}"),
          'rawNegatedFailureMessage ("{0} was defined at {1}"),
          'rawMidSentenceFailureMessage ("{0} was not defined at {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was defined at {1}"),
          'failureMessageArgs(Vector(fraction, 8)),
          'negatedFailureMessageArgs(Vector(fraction, 8)),
          'midSentenceFailureMessageArgs(Vector(fraction, 8)),
          'midSentenceNegatedFailureMessageArgs(Vector(fraction, 8))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (fraction + " was defined at 8"),
          'negatedFailureMessage (fraction + " was not defined at 8"),
          'midSentenceFailureMessage (fraction + " was defined at 8"),
          'midSentenceNegatedFailureMessage (fraction + " was not defined at 8"),
          'rawFailureMessage ("{0} was defined at {1}"),
          'rawNegatedFailureMessage ("{0} was not defined at {1}"),
          'rawMidSentenceFailureMessage ("{0} was defined at {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not defined at {1}"),
          'failureMessageArgs(Vector(fraction, 8)),
          'negatedFailureMessageArgs(Vector(fraction, 8)),
          'midSentenceFailureMessageArgs(Vector(fraction, 8)),
          'midSentenceNegatedFailureMessageArgs(Vector(fraction, 8))    
        )
      }
    }
    
    object `apply(ResultOfDefinedAt) method returns Matcher` {
      
      val fraction = new PartialFunction[Int, Int] {
        def apply(d: Int) = 42 / d
        def isDefinedAt(d: Int) = d != 0
      }
      
      val resultOfDefinedAt = new ResultOfDefinedAt(8)
      
      val mt = be (resultOfDefinedAt)
      
      def `should have pretty toString` {
        mt.toString should be ("be definedAt 8")
      }
      
      val mr = mt(fraction)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (fraction + " was not defined at 8"),
          'negatedFailureMessage (fraction + " was defined at 8"),
          'midSentenceFailureMessage (fraction + " was not defined at 8"),
          'midSentenceNegatedFailureMessage (fraction + " was defined at 8"),
          'rawFailureMessage ("{0} was not defined at {1}"),
          'rawNegatedFailureMessage ("{0} was defined at {1}"),
          'rawMidSentenceFailureMessage ("{0} was not defined at {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was defined at {1}"),
          'failureMessageArgs(Vector(fraction, 8)),
          'negatedFailureMessageArgs(Vector(fraction, 8)),
          'midSentenceFailureMessageArgs(Vector(fraction, 8)),
          'midSentenceNegatedFailureMessageArgs(Vector(fraction, 8))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (fraction + " was defined at 8"),
          'negatedFailureMessage (fraction + " was not defined at 8"),
          'midSentenceFailureMessage (fraction + " was defined at 8"),
          'midSentenceNegatedFailureMessage (fraction + " was not defined at 8"),
          'rawFailureMessage ("{0} was defined at {1}"),
          'rawNegatedFailureMessage ("{0} was not defined at {1}"),
          'rawMidSentenceFailureMessage ("{0} was defined at {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not defined at {1}"),
          'failureMessageArgs(Vector(fraction, 8)),
          'negatedFailureMessageArgs(Vector(fraction, 8)),
          'midSentenceFailureMessageArgs(Vector(fraction, 8)),
          'midSentenceNegatedFailureMessageArgs(Vector(fraction, 8))    
        )
      }
    }
    
    object `apply(ResultOfATypeInvocation) method returns Matcher` {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val clazz = classOf[MyFile]
      val resultOfAType = new ResultOfATypeInvocation(clazz)
      
      val mt = be (resultOfAType)
      
      def `should have pretty toString` {
        mt.toString should be ("be (a [" + clazz.getName + "])")
      }
      
      val myFile = new MyFile("test", true, false)
      
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName),
          'negatedFailureMessage (myFile + " was an instance of " + clazz.getName),
          'midSentenceFailureMessage (myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName),
          'midSentenceNegatedFailureMessage (myFile + " was an instance of " + clazz.getName),
          'rawFailureMessage ("{0} was not an instance of {1}, but an instance of {2}"),
          'rawNegatedFailureMessage ("{0} was an instance of {1}"),
          'rawMidSentenceFailureMessage ("{0} was not an instance of {1}, but an instance of {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was an instance of {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName)))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (myFile + " was an instance of " + clazz.getName),
          'negatedFailureMessage (myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName),
          'midSentenceFailureMessage (myFile + " was an instance of " + clazz.getName),
          'midSentenceNegatedFailureMessage (myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName),
          'rawFailureMessage ("{0} was an instance of {1}"),
          'rawNegatedFailureMessage ("{0} was not an instance of {1}, but an instance of {2}"),
          'rawMidSentenceFailureMessage ("{0} was an instance of {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not an instance of {1}, but an instance of {2}"),
          'failureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName)))
        )
      }
    }
    
    object `apply(ResultOfAnTypeInvocation) method returns Matcher` {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val clazz = classOf[MyFile]
      val resultOfAnType = new ResultOfAnTypeInvocation(clazz)
      
      val mt = be (resultOfAnType)
      
      def `should have pretty toString` {
        mt.toString should be ("be (an [" + clazz.getName + "])")
      }
      
      val myFile = new MyFile("test", true, false)
      
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName),
          'negatedFailureMessage (myFile + " was an instance of " + clazz.getName),
          'midSentenceFailureMessage (myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName),
          'midSentenceNegatedFailureMessage (myFile + " was an instance of " + clazz.getName),
          'rawFailureMessage ("{0} was not an instance of {1}, but an instance of {2}"),
          'rawNegatedFailureMessage ("{0} was an instance of {1}"),
          'rawMidSentenceFailureMessage ("{0} was not an instance of {1}, but an instance of {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was an instance of {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName)))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (myFile + " was an instance of " + clazz.getName),
          'negatedFailureMessage (myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName),
          'midSentenceFailureMessage (myFile + " was an instance of " + clazz.getName),
          'midSentenceNegatedFailureMessage (myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName),
          'rawFailureMessage ("{0} was an instance of {1}"),
          'rawNegatedFailureMessage ("{0} was not an instance of {1}, but an instance of {2}"),
          'rawMidSentenceFailureMessage ("{0} was an instance of {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not an instance of {1}, but an instance of {2}"),
          'failureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName)))
        )
      }
    }
    
    object `apply(ReadableWord) method returns MatcherFactory` {
      
      class MyFile {
        def isReadable: Boolean = true
      }
      
      val mtf = be (readable)
      val mt = mtf.matcher[MyFile]
      
      def `should have pretty toString` {
        mtf.toString should be ("be (readable)")
        mt.toString should be ("be (readable)")
      }
      
      val myFile = new MyFile
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (myFile + " was not readable"),
          'negatedFailureMessage (myFile + " was readable"),
          'midSentenceFailureMessage (myFile + " was not readable"),
          'midSentenceNegatedFailureMessage (myFile + " was readable"),
          'rawFailureMessage ("{0} was not readable"),
          'rawNegatedFailureMessage ("{0} was readable"),
          'rawMidSentenceFailureMessage ("{0} was not readable"),
          'rawMidSentenceNegatedFailureMessage ("{0} was readable"),
          'failureMessageArgs(Vector(myFile)),
          'negatedFailureMessageArgs(Vector(myFile)),
          'midSentenceFailureMessageArgs(Vector(myFile)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (myFile + " was readable"),
          'negatedFailureMessage (myFile + " was not readable"),
          'midSentenceFailureMessage (myFile + " was readable"),
          'midSentenceNegatedFailureMessage (myFile + " was not readable"),
          'rawFailureMessage ("{0} was readable"),
          'rawNegatedFailureMessage ("{0} was not readable"),
          'rawMidSentenceFailureMessage ("{0} was readable"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not readable"),
          'failureMessageArgs(Vector(myFile)),
          'negatedFailureMessageArgs(Vector(myFile)),
          'midSentenceFailureMessageArgs(Vector(myFile)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile))    
        )
      }
    }
    
    object `apply(WritableWord) method returns MatcherFactory` {
      
      class MyFile {
        def isWritable: Boolean = true
      }
      
      val mtf = be (writable)
      val mt = mtf.matcher[MyFile]
      
      def `should have pretty toString` {
        mtf.toString should be ("be (writable)")
        mt.toString should be ("be (writable)")
      }
      
      val myFile = new MyFile
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (myFile + " was not writable"),
          'negatedFailureMessage (myFile + " was writable"),
          'midSentenceFailureMessage (myFile + " was not writable"),
          'midSentenceNegatedFailureMessage (myFile + " was writable"),
          'rawFailureMessage ("{0} was not writable"),
          'rawNegatedFailureMessage ("{0} was writable"),
          'rawMidSentenceFailureMessage ("{0} was not writable"),
          'rawMidSentenceNegatedFailureMessage ("{0} was writable"),
          'failureMessageArgs(Vector(myFile)),
          'negatedFailureMessageArgs(Vector(myFile)),
          'midSentenceFailureMessageArgs(Vector(myFile)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (myFile + " was writable"),
          'negatedFailureMessage (myFile + " was not writable"),
          'midSentenceFailureMessage (myFile + " was writable"),
          'midSentenceNegatedFailureMessage (myFile + " was not writable"),
          'rawFailureMessage ("{0} was writable"),
          'rawNegatedFailureMessage ("{0} was not writable"),
          'rawMidSentenceFailureMessage ("{0} was writable"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not writable"),
          'failureMessageArgs(Vector(myFile)),
          'negatedFailureMessageArgs(Vector(myFile)),
          'midSentenceFailureMessageArgs(Vector(myFile)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile))    
        )
      }
    }
    
    object `apply(EmptyWord) method returns MatcherFactory` {
      
      val mtf = be (empty)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("be (empty)")
        mt.toString should be ("be (empty)")
      }
      
      val leftList = List.empty[Int]
      val mr = mt(leftList)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (leftList + " was not empty"),
          'negatedFailureMessage (leftList + " was empty"),
          'midSentenceFailureMessage (leftList + " was not empty"),
          'midSentenceNegatedFailureMessage (leftList + " was empty"),
          'rawFailureMessage ("{0} was not empty"),
          'rawNegatedFailureMessage ("{0} was empty"),
          'rawMidSentenceFailureMessage ("{0} was not empty"),
          'rawMidSentenceNegatedFailureMessage ("{0} was empty"),
          'failureMessageArgs(Vector(leftList)),
          'negatedFailureMessageArgs(Vector(leftList)),
          'midSentenceFailureMessageArgs(Vector(leftList)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (leftList + " was empty"),
          'negatedFailureMessage (leftList + " was not empty"),
          'midSentenceFailureMessage (leftList + " was empty"),
          'midSentenceNegatedFailureMessage (leftList + " was not empty"),
          'rawFailureMessage ("{0} was empty"),
          'rawNegatedFailureMessage ("{0} was not empty"),
          'rawMidSentenceFailureMessage ("{0} was empty"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not empty"),
          'failureMessageArgs(Vector(leftList)),
          'negatedFailureMessageArgs(Vector(leftList)),
          'midSentenceFailureMessageArgs(Vector(leftList)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList))    
        )
      }
    }
    
    object `apply(DefinedWord) method returns MatcherFactory` {
      
      val mtf = be (defined)
      val mt = mtf.matcher[Option[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("be (defined)")
        mt.toString should be ("be (defined)")
      }
      
      val leftOption = Some(1)
      val mr = mt(leftOption)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (leftOption + " was not defined"),
          'negatedFailureMessage (leftOption + " was defined"),
          'midSentenceFailureMessage (leftOption + " was not defined"),
          'midSentenceNegatedFailureMessage (leftOption + " was defined"),
          'rawFailureMessage ("{0} was not defined"),
          'rawNegatedFailureMessage ("{0} was defined"),
          'rawMidSentenceFailureMessage ("{0} was not defined"),
          'rawMidSentenceNegatedFailureMessage ("{0} was defined"),
          'failureMessageArgs(Vector(leftOption)),
          'negatedFailureMessageArgs(Vector(leftOption)),
          'midSentenceFailureMessageArgs(Vector(leftOption)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftOption))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (leftOption + " was defined"),
          'negatedFailureMessage (leftOption + " was not defined"),
          'midSentenceFailureMessage (leftOption + " was defined"),
          'midSentenceNegatedFailureMessage (leftOption + " was not defined"),
          'rawFailureMessage ("{0} was defined"),
          'rawNegatedFailureMessage ("{0} was not defined"),
          'rawMidSentenceFailureMessage ("{0} was defined"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not defined"),
          'failureMessageArgs(Vector(leftOption)),
          'negatedFailureMessageArgs(Vector(leftOption)),
          'midSentenceFailureMessageArgs(Vector(leftOption)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftOption))    
        )
      }
    }
  }
}