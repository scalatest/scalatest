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

class BeEqualEqualWordSpec extends Spec with FileMocks {
  
  object `BeEqualEqualWord ` {
    
    def `should have pretty toString` {
      be_==.toString should be ("be_==")
    }

    def `be_== < 3 should not compile` {
      "be_== < 3" shouldNot compile
    }

    def `be_== > 3 should not compile` {
      "be_== > 3" shouldNot compile
    }

    def `be_== <= 3 should not compile` {
      "be_== <= 3" shouldNot compile
    }

    def `be_== >= 3 should not compile` {
      "be_== >= 3" shouldNot compile
    }

    def `be_== === "cheese" should not compile` {
      "\"cheese\" should be_== === \"cheese\"" shouldNot compile
    }

    def `be_== a(Symbol) should not compile` {
      "be_== a ('file)" shouldNot compile
    }

    def `be_== a(BePropertyMatcher) should not compile` {
      "case class MyFile(\n" +
      "  val name: String,\n" +
      "  val file: Boolean,\n" +
      "  val isDirectory: Boolean\n" +
      ")\n\n" +
      "class FileBePropertyMatcher extends BePropertyMatcher[MyFile] {\n" +
      "  def apply(file: MyFile) = {\n" +
      "    new BePropertyMatchResult(file.file, \"file\")\n" +
      "  }\n" +
      "}\n" +
      "\n" +
      "val myFile = MyFile(\"test\", true, false)\n" +
      "val file = new FileBePropertyMatcher\n" +
      "\n" +
      "val mt = be_== a (file)" shouldNot compile
    }

    def `be_== a(AMatcher) should not compile` {
      "case class MyFile(\n" +
      "  val name: String,\n" +
      "  val file: Boolean,\n" +
      "  val isDirectory: Boolean\n" +
      ")\n" +
      "\n" +
      "val file = AMatcher[MyFile](\"file\") { _.file  }\n" +
      "val myFile = MyFile(\"test\", true, false)\n" +
      "\n" +
      "val mt = be_== a (file)" shouldNot compile
    }

    def `be_== an(Symbol) should not compile` {
      "be_== an ('file)" shouldNot compile
    }

    def `be_== an(BePropertyMatcher) should not compile` {
      "case class MyFile(\n" +
      "  val name: String,\n" +
      "  val file: Boolean,\n" +
      "  val isDirectory: Boolean\n" +
      ")\n\n" +
      "class FileBePropertyMatcher extends BePropertyMatcher[MyFile] {\n" +
      "  def apply(file: MyFile) = {\n" +
      "    new BePropertyMatchResult(file.file, \"file\")\n" +
      "  }\n" +
      "}\n" +
      "\n" +
      "val myFile = MyFile(\"test\", true, false)\n" +
      "val file = new FileBePropertyMatcher\n" +
      "\n" +
      "val mt = be_== an (file)" shouldNot compile
    }

    def `be_== an(AnMatcher) should not compile` {
      "case class MyFile(\n" +
      "  val name: String,\n" +
      "  val file: Boolean,\n" +
      "  val isDirectory: Boolean\n" +
      ")\n\n" +
      "val file = AnMatcher[MyFile](\"file\") { _.file  }\n" +
      "val myFile = MyFile(\"test\", true, false)\n\n" +
      "val mt = be_== an (file)" shouldNot compile
    }

    def `be_== +- 0.2 should not compile` {
      "val spread = 7.1 +- 0.2\n" +
      "7.0 be_== (spread)" shouldNot compile
    }

    def `be_== theSameInstanceAs(AnyRef) should not compile` {
      "case class MyFile(\n" +
      "  val name: String,\n" +
      "  val file: Boolean,\n" +
      "  val isDirectory: Boolean\n" +
      ")\n\n" +
      "val myFileLeft = MyFile(\"left\", true, false)\n" +
      "val myFileRight = MyFile(\"right\", true, false)\n\n" +
      "be_== theSameInstanceAs (myFileRight)" shouldNot compile
    }

    object `apply(Boolean) method returns Matcher` {
      val mt = be_== (true)

      def `should have pretty toString` {
        mt.toString should be ("be_== (true)")
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
      val mt = be_== (null)

      def `should have pretty toString` {
        mt.toString should be ("be_== (null)")
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

    object `when used with Symbol should use == to check for equality to the passed in Symbol` {
      val mt = be_== ('file)

      def `should have pretty toString` {
        mt.toString should be ("be_== ('file)")
      }

      val mr = mt(fileMock)

      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (fileMock + " was not equal to " + 'file),
          'negatedFailureMessage (fileMock + " was equal to " + 'file),
          'midSentenceFailureMessage (fileMock + " was not equal to " + 'file),
          'midSentenceNegatedFailureMessage (fileMock + " was equal to " + 'file),
          'rawFailureMessage ("{0} was not equal to {1}"),
          'rawNegatedFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was equal to {1}"),
          'failureMessageArgs(Vector(fileMock, 'file)),
          'negatedFailureMessageArgs(Vector(fileMock, 'file)),
          'midSentenceFailureMessageArgs(Vector(fileMock, 'file)),
          'midSentenceNegatedFailureMessageArgs(Vector(fileMock, 'file))
        )
      }

      val nmr = mr.negated

      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (fileMock + " was equal to " + 'file),
          'negatedFailureMessage (fileMock + " was not equal to " + 'file),
          'midSentenceFailureMessage (fileMock + " was equal to " + 'file),
          'midSentenceNegatedFailureMessage (fileMock + " was not equal to " + 'file),
          'rawFailureMessage ("{0} was equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not equal to {1}"),
          'failureMessageArgs(Vector(fileMock, 'file)),
          'negatedFailureMessageArgs(Vector(fileMock, 'file)),
          'midSentenceFailureMessageArgs(Vector(fileMock, 'file)),
          'midSentenceNegatedFailureMessageArgs(Vector(fileMock, 'file))
        )
      }
    }

    object `when used with BeMatcher should use == to check for equality to the passed in BeMatcher` {
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

      val mt = be_== (odd)

      def `should have pretty toString` {
        mt.toString should be ("be_== (" + odd + ")")
      }

      val mr = mt(1)

      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("1 was not equal to " + odd),
          'negatedFailureMessage ("1 was equal to " + odd),
          'midSentenceFailureMessage ("1 was not equal to " + odd),
          'midSentenceNegatedFailureMessage ("1 was equal to " + odd),
          'rawFailureMessage ("{0} was not equal to {1}"),
          'rawNegatedFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was equal to {1}"),
          'failureMessageArgs(Vector(1, odd)),
          'negatedFailureMessageArgs(Vector(1, odd)),
          'midSentenceFailureMessageArgs(Vector(1, odd)),
          'midSentenceNegatedFailureMessageArgs(Vector(1, odd))
        )
      }

      val nmr = mr.negated

      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("1 was equal to " + odd),
          'negatedFailureMessage ("1 was not equal to " + odd),
          'midSentenceFailureMessage ("1 was equal to " + odd),
          'midSentenceNegatedFailureMessage ("1 was not equal to " + odd),
          'rawFailureMessage ("{0} was equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not equal to {1}"),
          'failureMessageArgs(Vector(1, odd)),
          'negatedFailureMessageArgs(Vector(1, odd)),
          'midSentenceFailureMessageArgs(Vector(1, odd)),
          'midSentenceNegatedFailureMessageArgs(Vector(1, odd))
        )
      }
    }

    object `when used with BePropertyMatcher should use == to check for equality to the passed in BePropertyMatcher` {

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

      val mt = be_== (file)

      def `should have pretty toString` {
        mt.toString should be ("be_== (" + file + ")")
      }

      val mr = mt(myFile)

      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (myFile + " was not equal to " + file),
          'negatedFailureMessage (myFile + " was equal to " + file),
          'midSentenceFailureMessage (myFile + " was not equal to " + file),
          'midSentenceNegatedFailureMessage (myFile + " was equal to " + file),
          'rawFailureMessage ("{0} was not equal to {1}"),
          'rawNegatedFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was equal to {1}"),
          'failureMessageArgs(Vector(myFile, file)),
          'negatedFailureMessageArgs(Vector(myFile, file)),
          'midSentenceFailureMessageArgs(Vector(myFile, file)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, file))
        )
      }

      val nmr = mr.negated

      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (myFile + " was equal to " + file),
          'negatedFailureMessage (myFile + " was not equal to " + file),
          'midSentenceFailureMessage (myFile + " was equal to " + file),
          'midSentenceNegatedFailureMessage (myFile + " was not equal to " + file),
          'rawFailureMessage ("{0} was equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not equal to {1}"),
          'failureMessageArgs(Vector(myFile, file)),
          'negatedFailureMessageArgs(Vector(myFile, file)),
          'midSentenceFailureMessageArgs(Vector(myFile, file)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, file))
        )
      }
    }

    object `apply(Any) method returns MatcherFactory` {

      case class MyFile(
                         val name: String,
                         val file: Boolean,
                         val isDirectory: Boolean
                         )

      val myFileLeft = MyFile("test left", true, false)
      val myFileRight = MyFile("test right", true, false)

      val mt = be_== (myFileRight)

      def `should have pretty toString` {
        mt.toString should be ("be_== (" + myFileRight + ")")
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

    object `when used with SortedWord should use == to check for equality to the passed in SortedWord` {

      val mt = be_== (sorted)

      def `should have pretty toString` {
        mt.toString should be ("be_== (" + sorted + ")")
      }

      val leftList = List(1, 2, 3)
      val mr = mt(leftList)

      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (leftList + " was not equal to " + sorted),
          'negatedFailureMessage (leftList + " was equal to " + sorted),
          'midSentenceFailureMessage (leftList + " was not equal to " + sorted),
          'midSentenceNegatedFailureMessage (leftList + " was equal to " + sorted),
          'rawFailureMessage ("{0} was not equal to {1}"),
          'rawNegatedFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was equal to {1}"),
          'failureMessageArgs(Vector(leftList, sorted)),
          'negatedFailureMessageArgs(Vector(leftList, sorted)),
          'midSentenceFailureMessageArgs(Vector(leftList, sorted)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList, sorted))
        )
      }

      val nmr = mr.negated

      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (leftList + " was equal to " + sorted),
          'negatedFailureMessage (leftList + " was not equal to " + sorted),
          'midSentenceFailureMessage (leftList + " was equal to " + sorted),
          'midSentenceNegatedFailureMessage (leftList + " was not equal to " + sorted),
          'rawFailureMessage ("{0} was equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not equal to {1}"),
          'failureMessageArgs(Vector(leftList, sorted)),
          'negatedFailureMessageArgs(Vector(leftList, sorted)),
          'midSentenceFailureMessageArgs(Vector(leftList, sorted)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList, sorted))
        )
      }
    }

    def `be_== definedAt (8) should not compile` {
      "be_== definedAt (8)" shouldNot compile
    }

    object `when used with ResultOfDefinedAt should use == to check for equality to the passed in ResultOfDefinedAt` {

      val fraction = new PartialFunction[Int, Int] {
        def apply(d: Int) = 42 / d
        def isDefinedAt(d: Int) = d != 0
      }

      val resultOfDefinedAt = new ResultOfDefinedAt(8)

      val mt = be_== (resultOfDefinedAt)

      def `should have pretty toString` {
        mt.toString should be ("be_== (definedAt (8))")
      }

      val mr = mt(fraction)

      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (fraction + " was not equal to " + resultOfDefinedAt),
          'negatedFailureMessage (fraction + " was equal to " + resultOfDefinedAt),
          'midSentenceFailureMessage (fraction + " was not equal to " + resultOfDefinedAt),
          'midSentenceNegatedFailureMessage (fraction + " was equal to " + resultOfDefinedAt),
          'rawFailureMessage ("{0} was not equal to {1}"),
          'rawNegatedFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was equal to {1}"),
          'failureMessageArgs(Vector(fraction, resultOfDefinedAt)),
          'negatedFailureMessageArgs(Vector(fraction, resultOfDefinedAt)),
          'midSentenceFailureMessageArgs(Vector(fraction, resultOfDefinedAt)),
          'midSentenceNegatedFailureMessageArgs(Vector(fraction, resultOfDefinedAt))
        )
      }

      val nmr = mr.negated

      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (fraction + " was equal to " + resultOfDefinedAt),
          'negatedFailureMessage (fraction + " was not equal to " + resultOfDefinedAt),
          'midSentenceFailureMessage (fraction + " was equal to " + resultOfDefinedAt),
          'midSentenceNegatedFailureMessage (fraction + " was not equal to " + resultOfDefinedAt),
          'rawFailureMessage ("{0} was equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not equal to {1}"),
          'failureMessageArgs(Vector(fraction, resultOfDefinedAt)),
          'negatedFailureMessageArgs(Vector(fraction, resultOfDefinedAt)),
          'midSentenceFailureMessageArgs(Vector(fraction, resultOfDefinedAt)),
          'midSentenceNegatedFailureMessageArgs(Vector(fraction, resultOfDefinedAt))
        )
      }
    }

    object `when used with ResultOfATypeInvocation should use == to check for equality to the passed in ResultOfATypeInvocation` {

      case class MyFile(
                         val name: String,
                         val file: Boolean,
                         val isDirectory: Boolean
                         )

      val clazz = classOf[MyFile]
      val resultOfAType = new ResultOfATypeInvocation(clazz)

      val mt = be_== (resultOfAType)

      def `should have pretty toString` {
        mt.toString should be ("be_== (a [" + clazz.getName + "])")
      }

      val myFile = new MyFile("test", true, false)

      val mr = mt(myFile)

      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (myFile + " was not equal to " + resultOfAType),
          'negatedFailureMessage (myFile + " was equal to " + resultOfAType),
          'midSentenceFailureMessage (myFile + " was not equal to " + resultOfAType),
          'midSentenceNegatedFailureMessage (myFile + " was equal to " + resultOfAType),
          'rawFailureMessage ("{0} was not equal to {1}"),
          'rawNegatedFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was equal to {1}"),
          'failureMessageArgs(Vector(myFile, resultOfAType)),
          'negatedFailureMessageArgs(Vector(myFile, resultOfAType)),
          'midSentenceFailureMessageArgs(Vector(myFile, resultOfAType)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, resultOfAType))
        )
      }

      val nmr = mr.negated

      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (myFile + " was equal to " + resultOfAType),
          'negatedFailureMessage (myFile + " was not equal to " + resultOfAType),
          'midSentenceFailureMessage (myFile + " was equal to " + resultOfAType),
          'midSentenceNegatedFailureMessage (myFile + " was not equal to " + resultOfAType),
          'rawFailureMessage ("{0} was equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not equal to {1}"),
          'failureMessageArgs(Vector(myFile, resultOfAType)),
          'negatedFailureMessageArgs(Vector(myFile, resultOfAType)),
          'midSentenceFailureMessageArgs(Vector(myFile, resultOfAType)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, resultOfAType))
        )
      }
    }

    object `when used with ResultOfAnTypeInvocation should use == to check for equality to the passed in ResultOfAnTypeInvocation` {

      case class MyFile(
                         val name: String,
                         val file: Boolean,
                         val isDirectory: Boolean
                         )

      val clazz = classOf[MyFile]
      val resultOfAnType = new ResultOfAnTypeInvocation(clazz)

      val mt = be_== (resultOfAnType)

      def `should have pretty toString` {
        mt.toString should be ("be_== (an [" + clazz.getName + "])")
      }

      val myFile = new MyFile("test", true, false)

      val mr = mt(myFile)

      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (myFile + " was not equal to " + resultOfAnType),
          'negatedFailureMessage (myFile + " was equal to " + resultOfAnType),
          'midSentenceFailureMessage (myFile + " was not equal to " + resultOfAnType),
          'midSentenceNegatedFailureMessage (myFile + " was equal to " + resultOfAnType),
          'rawFailureMessage ("{0} was not equal to {1}"),
          'rawNegatedFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was equal to {1}"),
          'failureMessageArgs(Vector(myFile, resultOfAnType)),
          'negatedFailureMessageArgs(Vector(myFile, resultOfAnType)),
          'midSentenceFailureMessageArgs(Vector(myFile, resultOfAnType)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, resultOfAnType))
        )
      }

      val nmr = mr.negated

      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (myFile + " was equal to " + resultOfAnType),
          'negatedFailureMessage (myFile + " was not equal to " + resultOfAnType),
          'midSentenceFailureMessage (myFile + " was equal to " + resultOfAnType),
          'midSentenceNegatedFailureMessage (myFile + " was not equal to " + resultOfAnType),
          'rawFailureMessage ("{0} was equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not equal to {1}"),
          'failureMessageArgs(Vector(myFile, resultOfAnType)),
          'negatedFailureMessageArgs(Vector(myFile, resultOfAnType)),
          'midSentenceFailureMessageArgs(Vector(myFile, resultOfAnType)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, resultOfAnType))
        )
      }
    }

    object `when used with ReadableWord should use == to check for equality to the passed in ReadableWord` {

      class MyFile {
        def isReadable: Boolean = true
      }

      val mt = be_== (readable)

      def `should have pretty toString` {
        mt.toString should be ("be_== (readable)")
      }

      val myFile = new MyFile
      val mr = mt(myFile)

      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (myFile + " was not equal to " + readable),
          'negatedFailureMessage (myFile + " was equal to " + readable),
          'midSentenceFailureMessage (myFile + " was not equal to " + readable),
          'midSentenceNegatedFailureMessage (myFile + " was equal to " + readable),
          'rawFailureMessage ("{0} was not equal to {1}"),
          'rawNegatedFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was equal to {1}"),
          'failureMessageArgs(Vector(myFile, readable)),
          'negatedFailureMessageArgs(Vector(myFile, readable)),
          'midSentenceFailureMessageArgs(Vector(myFile, readable)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, readable))
        )
      }

      val nmr = mr.negated

      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (myFile + " was equal to " + readable),
          'negatedFailureMessage (myFile + " was not equal to " + readable),
          'midSentenceFailureMessage (myFile + " was equal to " + readable),
          'midSentenceNegatedFailureMessage (myFile + " was not equal to " + readable),
          'rawFailureMessage ("{0} was equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not equal to {1}"),
          'failureMessageArgs(Vector(myFile, readable)),
          'negatedFailureMessageArgs(Vector(myFile, readable)),
          'midSentenceFailureMessageArgs(Vector(myFile, readable)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, readable))
        )
      }
    }

    object `when used with WritableWord should use == to check for equality to the passed in WritableWord` {

      class MyFile {
        def isWritable: Boolean = true
      }

      val mt = be_== (writable)

      def `should have pretty toString` {
        mt.toString should be ("be_== (writable)")
      }

      val myFile = new MyFile
      val mr = mt(myFile)

      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (myFile + " was not equal to " + writable),
          'negatedFailureMessage (myFile + " was equal to " + writable),
          'midSentenceFailureMessage (myFile + " was not equal to " + writable),
          'midSentenceNegatedFailureMessage (myFile + " was equal to " + writable),
          'rawFailureMessage ("{0} was not equal to {1}"),
          'rawNegatedFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was equal to {1}"),
          'failureMessageArgs(Vector(myFile, writable)),
          'negatedFailureMessageArgs(Vector(myFile, writable)),
          'midSentenceFailureMessageArgs(Vector(myFile, writable)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, writable))
        )
      }

      val nmr = mr.negated

      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (myFile + " was equal to " + writable),
          'negatedFailureMessage (myFile + " was not equal to " + writable),
          'midSentenceFailureMessage (myFile + " was equal to " + writable),
          'midSentenceNegatedFailureMessage (myFile + " was not equal to " + writable),
          'rawFailureMessage ("{0} was equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not equal to {1}"),
          'failureMessageArgs(Vector(myFile, writable)),
          'negatedFailureMessageArgs(Vector(myFile, writable)),
          'midSentenceFailureMessageArgs(Vector(myFile, writable)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, writable))
        )
      }
    }

    object `when used with EmptyWord should use == to check for equality to the passed in EmptyWord` {

      val mt = be_== (empty)

      def `should have pretty toString` {
        mt.toString should be ("be_== (empty)")
      }

      val leftList = List.empty[Int]
      val mr = mt(leftList)

      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (leftList + " was not equal to " + empty),
          'negatedFailureMessage (leftList + " was equal to " + empty),
          'midSentenceFailureMessage (leftList + " was not equal to " + empty),
          'midSentenceNegatedFailureMessage (leftList + " was equal to " + empty),
          'rawFailureMessage ("{0} was not equal to {1}"),
          'rawNegatedFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was equal to {1}"),
          'failureMessageArgs(Vector(leftList, empty)),
          'negatedFailureMessageArgs(Vector(leftList, empty)),
          'midSentenceFailureMessageArgs(Vector(leftList, empty)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList, empty))
        )
      }

      val nmr = mr.negated

      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (leftList + " was equal to " + empty),
          'negatedFailureMessage (leftList + " was not equal to " + empty),
          'midSentenceFailureMessage (leftList + " was equal to " + empty),
          'midSentenceNegatedFailureMessage (leftList + " was not equal to " + empty),
          'rawFailureMessage ("{0} was equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not equal to {1}"),
          'failureMessageArgs(Vector(leftList, empty)),
          'negatedFailureMessageArgs(Vector(leftList, empty)),
          'midSentenceFailureMessageArgs(Vector(leftList, empty)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList, empty))
        )
      }
    }

    object `when used with DefinedWord should use == to check for equality to the passed in DefinedWord` {

      val mt = be_== (defined)

      def `should have pretty toString` {
        mt.toString should be ("be_== (defined)")
      }

      val leftOption = Some(1)
      val mr = mt(leftOption)

      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (leftOption + " was not equal to " + defined),
          'negatedFailureMessage (leftOption + " was equal to " + defined),
          'midSentenceFailureMessage (leftOption + " was not equal to " + defined),
          'midSentenceNegatedFailureMessage (leftOption + " was equal to " + defined),
          'rawFailureMessage ("{0} was not equal to {1}"),
          'rawNegatedFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was equal to {1}"),
          'failureMessageArgs(Vector(leftOption, defined)),
          'negatedFailureMessageArgs(Vector(leftOption, defined)),
          'midSentenceFailureMessageArgs(Vector(leftOption, defined)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftOption, defined))
        )
      }

      val nmr = mr.negated

      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (leftOption + " was equal to " + defined),
          'negatedFailureMessage (leftOption + " was not equal to " + defined),
          'midSentenceFailureMessage (leftOption + " was equal to " + defined),
          'midSentenceNegatedFailureMessage (leftOption + " was not equal to " + defined),
          'rawFailureMessage ("{0} was equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not equal to {1}"),
          'failureMessageArgs(Vector(leftOption, defined)),
          'negatedFailureMessageArgs(Vector(leftOption, defined)),
          'midSentenceFailureMessageArgs(Vector(leftOption, defined)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftOption, defined))
        )
      }
    }
  }
}