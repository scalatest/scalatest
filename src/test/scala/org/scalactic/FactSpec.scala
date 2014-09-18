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
import Inside._

class FactResultSpec extends FreeSpec with Matchers with PrettyMethods {

  "A Fact" - {
    val fact = No("1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2")
    "can be negated" in {
      fact should equal (No("1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2"))
      !fact should equal (Yes("1 equaled 2", "1 did not equal 2", "1 equaled 2", "1 did not equal 2"))
      val fact2 = Yes("{0} did not equal null", "The reference equaled null", "{0} did not equal null", "the reference equaled null", Vector("howdy"), Vector.empty)
      fact2 should have (
        'failureMessage ("\"howdy\" did not equal null"),
        'negatedFailureMessage ("The reference equaled null"),
        'midSentenceFailureMessage ("\"howdy\" did not equal null"),
        'midSentenceNegatedFailureMessage ("the reference equaled null"),
        'rawFailureMessage ("{0} did not equal null"),
        'rawNegatedFailureMessage ("The reference equaled null"),
        'rawMidSentenceFailureMessage ("{0} did not equal null"),
        'rawMidSentenceNegatedFailureMessage ("the reference equaled null"),
        'failureMessageArgs(Vector("howdy")),
        'negatedFailureMessageArgs(Vector.empty),
        'midSentenceFailureMessageArgs(Vector("howdy")),
        'midSentenceNegatedFailureMessageArgs(Vector.empty)
      )
      val fact2Negated = !fact2
       fact2Negated should equal (No("The reference equaled null", "{0} did not equal null", "the reference equaled null", "{0} did not equal null", Vector.empty, Vector("howdy")))
      fact2Negated should have (
        'failureMessage ("The reference equaled null"),
        'negatedFailureMessage ("\"howdy\" did not equal null"),
        'midSentenceFailureMessage ("the reference equaled null"),
        'midSentenceNegatedFailureMessage ("\"howdy\" did not equal null"),
        'rawFailureMessage ("The reference equaled null"),
        'rawNegatedFailureMessage ("{0} did not equal null"),
        'rawMidSentenceFailureMessage ("the reference equaled null"),
        'rawMidSentenceNegatedFailureMessage ("{0} did not equal null"),
        'failureMessageArgs(Vector.empty),
        'negatedFailureMessageArgs(Vector("howdy")),
        'midSentenceFailureMessageArgs(Vector.empty),
        'midSentenceNegatedFailureMessageArgs(Vector("howdy"))
      )
    }
    "should construct localized strings from the raw strings and args" in {
      val fact = No("{0} did not equal {1}", "{0} equaled {1}", "{0} did not equal {1}", "{0} equaled {1}", Vector(1, 2), Vector(1, 2))
      fact should have (
        'failureMessage ("1 did not equal 2"),
        'negatedFailureMessage ("1 equaled 2"),
        'midSentenceFailureMessage ("1 did not equal 2"),
        'midSentenceNegatedFailureMessage ("1 equaled 2"),
        'rawFailureMessage ("{0} did not equal {1}"),
        'rawNegatedFailureMessage ("{0} equaled {1}"),
        'rawMidSentenceFailureMessage ("{0} did not equal {1}"),
        'rawMidSentenceNegatedFailureMessage ("{0} equaled {1}"),
        'failureMessageArgs(Vector(1, 2)),
        'negatedFailureMessageArgs(Vector(1, 2)),
        'midSentenceFailureMessageArgs(Vector(1, 2)),
        'midSentenceNegatedFailureMessageArgs(Vector(1, 2))
      )
    }

    "should use midSentenceFailureMessageArgs to construct midSentenceFailureMessage" in {
      val fact = No("{0} did not equal {1}", "{0} equaled {1}", "{0} did not equal {1}", "{0} equaled {1}", Vector.empty, Vector.empty, Vector(1, 2), Vector.empty, Prettifier.default)
      fact.midSentenceFailureMessage should be ("1 did not equal 2")
    }

    "should use midSentenceNegatedFailureMessageArgs to construct midSentenceNegatedFailureMessage" in {
      val fact = No("{0} did not equal {1}", "{0} equaled {1}", "{0} did not equal {1}", "{0} equaled {1}", Vector.empty, Vector.empty, Vector.empty, Vector(1, 2), Prettifier.default)
      fact.midSentenceNegatedFailureMessage should be ("1 equaled 2")
    }
  }

  "The MatchResult companion object factory method" - {
    "that takes two strings should work correctly" in {
      val fact = Yes("one", "two")
      fact should have (
        'failureMessage ("one"),
        'negatedFailureMessage ("two"),
        'midSentenceFailureMessage ("one"),
        'midSentenceNegatedFailureMessage ("two"),
        'rawFailureMessage ("one"),
        'rawNegatedFailureMessage ("two"),
        'rawMidSentenceFailureMessage ("one"),
        'rawMidSentenceNegatedFailureMessage ("two"),
        'failureMessageArgs(Vector.empty),
        'negatedFailureMessageArgs(Vector.empty),
        'midSentenceFailureMessageArgs(Vector.empty),
        'midSentenceNegatedFailureMessageArgs(Vector.empty)
      )
      val ms = No("aaa", "bbb")
      ms should have (
        'failureMessage ("aaa"),
        'negatedFailureMessage ("bbb"),
        'midSentenceFailureMessage ("aaa"),
        'midSentenceNegatedFailureMessage ("bbb"),
        'rawFailureMessage ("aaa"),
        'rawNegatedFailureMessage ("bbb"),
        'rawMidSentenceFailureMessage ("aaa"),
        'rawMidSentenceNegatedFailureMessage ("bbb"),
        'failureMessageArgs(Vector.empty),
        'negatedFailureMessageArgs(Vector.empty),
        'midSentenceFailureMessageArgs(Vector.empty),
        'midSentenceNegatedFailureMessageArgs(Vector.empty)
      )
    }
    "that takes four strings should work correctly" in {
      val fact = Yes("one", "two", "three", "four")
      fact should have (
        'failureMessage ("one"),
        'negatedFailureMessage ("two"),
        'midSentenceFailureMessage ("three"),
        'midSentenceNegatedFailureMessage ("four"),
        'rawFailureMessage ("one"),
        'rawNegatedFailureMessage ("two"),
        'rawMidSentenceFailureMessage ("three"),
        'rawMidSentenceNegatedFailureMessage ("four"),
        'failureMessageArgs(Vector.empty),
        'negatedFailureMessageArgs(Vector.empty),
        'midSentenceFailureMessageArgs(Vector.empty),
        'midSentenceNegatedFailureMessageArgs(Vector.empty)
      )
      val ms = No("aaa", "bbb", "ccc", "ddd")
      ms should have (
        'failureMessage ("aaa"),
        'negatedFailureMessage ("bbb"),
        'midSentenceFailureMessage ("ccc"),
        'midSentenceNegatedFailureMessage ("ddd"),
        'rawFailureMessage ("aaa"),
        'rawNegatedFailureMessage ("bbb"),
        'rawMidSentenceFailureMessage ("ccc"),
        'rawMidSentenceNegatedFailureMessage ("ddd"),
        'failureMessageArgs(Vector.empty),
        'negatedFailureMessageArgs(Vector.empty),
        'midSentenceFailureMessageArgs(Vector.empty),
        'midSentenceNegatedFailureMessageArgs(Vector.empty)
      )
    }
    "that takes four strings and two IndexedSeqs should work correctly" in {
      val fact = Yes("one", "two", "three", "four", Vector(42), Vector(42.0))
      fact should have (
        'failureMessage ("one"),
        'negatedFailureMessage ("two"),
        'midSentenceFailureMessage ("three"),
        'midSentenceNegatedFailureMessage ("four"),
        'rawFailureMessage ("one"),
        'rawNegatedFailureMessage ("two"),
        'rawMidSentenceFailureMessage ("three"),
        'rawMidSentenceNegatedFailureMessage ("four"),
        'failureMessageArgs(Vector(42)),
        'negatedFailureMessageArgs(Vector(42.0)),
        'midSentenceFailureMessageArgs(Vector(42)),
        'midSentenceNegatedFailureMessageArgs(Vector(42.0))
      )
      val ms = No("aaa", "bbb", "ccc", "ddd", Vector("ho", "he"), Vector("foo", "fie"))
      ms should have (
        'failureMessage ("aaa"),
        'negatedFailureMessage ("bbb"),
        'midSentenceFailureMessage ("ccc"),
        'midSentenceNegatedFailureMessage ("ddd"),
        'rawFailureMessage ("aaa"),
        'rawNegatedFailureMessage ("bbb"),
        'rawMidSentenceFailureMessage ("ccc"),
        'rawMidSentenceNegatedFailureMessage ("ddd"),
        'failureMessageArgs(Vector("ho", "he")),
        'negatedFailureMessageArgs(Vector("foo", "fie")),
        'midSentenceFailureMessageArgs(Vector("ho", "he")),
        'midSentenceNegatedFailureMessageArgs(Vector("foo", "fie"))
      )
    }
    "that takes two strings and one IndexedSeq should work correctly" in {
      val fact = Yes("one", "two", Vector(42))
      fact should have (
        'failureMessage ("one"),
        'negatedFailureMessage ("two"),
        'midSentenceFailureMessage ("one"),
        'midSentenceNegatedFailureMessage ("two"),
        'rawFailureMessage ("one"),
        'rawNegatedFailureMessage ("two"),
        'rawMidSentenceFailureMessage ("one"),
        'rawMidSentenceNegatedFailureMessage ("two"),
        'failureMessageArgs(Vector(42)),
        'negatedFailureMessageArgs(Vector(42)),
        'midSentenceFailureMessageArgs(Vector(42)),
        'midSentenceNegatedFailureMessageArgs(Vector(42))
      )
      val ms = No("aaa", "bbb", Vector("ho", "he"))
      ms should have (
        'failureMessage ("aaa"),
        'negatedFailureMessage ("bbb"),
        'midSentenceFailureMessage ("aaa"),
        'midSentenceNegatedFailureMessage ("bbb"),
        'rawFailureMessage ("aaa"),
        'rawNegatedFailureMessage ("bbb"),
        'rawMidSentenceFailureMessage ("aaa"),
        'rawMidSentenceNegatedFailureMessage ("bbb"),
        'failureMessageArgs(Vector("ho", "he")),
        'negatedFailureMessageArgs(Vector("ho", "he")),
        'midSentenceFailureMessageArgs(Vector("ho", "he")),
        'midSentenceNegatedFailureMessageArgs(Vector("ho", "he"))
      )
    }
    "that takes two strings and two IndexedSeqs should work correctly" in {
      val fact = Yes("one", "two", Vector(42), Vector(42.0))
      fact should have (
        'failureMessage ("one"),
        'negatedFailureMessage ("two"),
        'midSentenceFailureMessage ("one"),
        'midSentenceNegatedFailureMessage ("two"),
        'rawFailureMessage ("one"),
        'rawNegatedFailureMessage ("two"),
        'rawMidSentenceFailureMessage ("one"),
        'rawMidSentenceNegatedFailureMessage ("two"),
        'failureMessageArgs(Vector(42)),
        'negatedFailureMessageArgs(Vector(42.0)),
        'midSentenceFailureMessageArgs(Vector(42)),
        'midSentenceNegatedFailureMessageArgs(Vector(42.0))
      )
      val ms = No("aaa", "bbb", Vector("ho", "he"), Vector("foo", "fie"))
      ms should have (
        'failureMessage ("aaa"),
        'negatedFailureMessage ("bbb"),
        'midSentenceFailureMessage ("aaa"),
        'midSentenceNegatedFailureMessage ("bbb"),
        'rawFailureMessage ("aaa"),
        'rawNegatedFailureMessage ("bbb"),
        'rawMidSentenceFailureMessage ("aaa"),
        'rawMidSentenceNegatedFailureMessage ("bbb"),
        'failureMessageArgs(Vector("ho", "he")),
        'negatedFailureMessageArgs(Vector("foo", "fie")),
        'midSentenceFailureMessageArgs(Vector("ho", "he")),
        'midSentenceNegatedFailureMessageArgs(Vector("foo", "fie"))
      )
    }
    "that takes four strings and four IndexedSeqs should work correctly" in {
      val fact = Yes("one", "two", "three", "four", Vector(1), Vector(2), Vector(3), Vector(4))
      fact should have (
        'failureMessage ("one"),
        'negatedFailureMessage ("two"),
        'midSentenceFailureMessage ("three"),
        'midSentenceNegatedFailureMessage ("four"),
        'rawFailureMessage ("one"),
        'rawNegatedFailureMessage ("two"),
        'rawMidSentenceFailureMessage ("three"),
        'rawMidSentenceNegatedFailureMessage ("four"),
        'failureMessageArgs(Vector(1)),
        'negatedFailureMessageArgs(Vector(2)),
        'midSentenceFailureMessageArgs(Vector(3)),
        'midSentenceNegatedFailureMessageArgs(Vector(4))
      )
      val ms = No("aaa", "bbb", "ccc", "ddd", Vector('A'), Vector('B'), Vector('C'), Vector('D'))
      ms should have (
        'failureMessage ("aaa"),
        'negatedFailureMessage ("bbb"),
        'midSentenceFailureMessage ("ccc"),
        'midSentenceNegatedFailureMessage ("ddd"),
        'rawFailureMessage ("aaa"),
        'rawNegatedFailureMessage ("bbb"),
        'rawMidSentenceFailureMessage ("ccc"),
        'rawMidSentenceNegatedFailureMessage ("ddd"),
        'failureMessageArgs(Vector('A')),
        'negatedFailureMessageArgs(Vector('B')),
        'midSentenceFailureMessageArgs(Vector('C')),
        'midSentenceNegatedFailureMessageArgs(Vector('D'))
      )
    }
  }

/*
  "The MatchResult obtained from and-ing two Matchers" - {
    /*
      scala> be > 'b' and be > 'd'
      res0: org.scalatest.matchers.Matcher[Char] = <function1>
    */
    "should be lazy about constructing strings" - {
      /*
        scala> res0('a')
        res1: org.scalatest.matchers.MatchResult = MatchResult(false,'a' was not greater than 'b','a' was greater than 'b','a' was not greater than 'b','a' was greater than 'b',Vector(),Vector())
      */
      "for No && No" in {
        val leftSideNo = No(Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'), Prettifier.default)
        val rightSideNo = No(Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'), Prettifier.default)
        val fact = leftSideNo && rightSideNo
        fact shouldBe a [No]
        fact.rawFailureMessage should be (Resources("wasNotGreaterThan"))
        fact.rawNegatedFailureMessage should be (Resources("wasGreaterThan"))
        fact.rawMidSentenceFailureMessage should be (Resources("wasNotGreaterThan"))
        fact.rawMidSentenceNegatedFailureMessage should be (Resources("wasGreaterThan"))
        fact.failureMessage should be (Resources("wasNotGreaterThan", 'a'.pretty, 'b'.pretty))
        fact.negatedFailureMessage should be (Resources("wasGreaterThan", 'a'.pretty, 'b'.pretty))
        fact.midSentenceFailureMessage should be (Resources("wasNotGreaterThan", 'a'.pretty, 'b'.pretty))
        fact.midSentenceNegatedFailureMessage should be (Resources("wasGreaterThan", 'a'.pretty, 'b'.pretty))
        fact.failureMessageArgs should be (Vector('a', 'b'))
        fact.negatedFailureMessageArgs should be (Vector('a', 'b'))
      }

       /*
         scala> be > 'b' and be < 'd'
         res4: org.scalatest.matchers.Matcher[Char] = <function1>

         scala> res4('a')
         res5: org.scalatest.matchers.MatchResult = MatchResult(false,'a' was not greater than 'b','a' was greater than 'b','a' was not greater than 'b','a' was greater than 'b',Vector(),Vector())
         
         val m = be > 'b' and be < 'd'
         val fact = m('a')
       */
      "for No && Yes" in {
        val leftSideNo = No(Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'), Prettifier.default)
        val rightSideYes = Yes(Resources("wasNotLessThan"), Resources("wasLessThan"), Resources("wasNotLessThan"), Resources("wasLessThan"), Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'), Prettifier.default)
        val fact = leftSideNo && rightSideYes
        fact shouldBe a [No]
        fact.rawFailureMessage should be (Resources("wasNotGreaterThan"))
        fact.rawNegatedFailureMessage should be (Resources("wasGreaterThan"))
        fact.rawMidSentenceFailureMessage should be (Resources("wasNotGreaterThan"))
        fact.rawMidSentenceNegatedFailureMessage should be (Resources("wasGreaterThan"))
        fact.failureMessage should be (Resources("wasNotGreaterThan", 'a'.pretty, 'b'.pretty))
        fact.negatedFailureMessage should be (Resources("wasGreaterThan", 'a'.pretty, 'b'.pretty))
        fact.midSentenceFailureMessage should be (Resources("wasNotGreaterThan", 'a'.pretty, 'b'.pretty))
        fact.midSentenceNegatedFailureMessage should be (Resources("wasGreaterThan", 'a'.pretty, 'b'.pretty))
        fact.failureMessageArgs should be (Vector('a', 'b'))
        fact.negatedFailureMessageArgs should be (Vector('a', 'b'))
      }

        /*
          scala> res0('c')
          res2: org.scalatest.matchers.MatchResult = MatchResult(false,'c' was greater than 'b', but 'c' was not greater than 'd','c' was greater than 'b', and 'c' was greater than 'd','c' was greater than 'b', but 'c' was not greater than 'd','c' was greater than 'b', and 'c' was greater than 'd',Vector(),Vector())

          val left = be > 'b'
          val right = be > 'd'
          val m = left and right
          val fact = m('c')
        */
      "for Yes && No" in {
        val leftSideYes = Yes(Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Vector('c', 'b'),Vector('c', 'b'),Vector('c', 'b'),Vector('c', 'b'), Prettifier.default)
        val rightSideNo = No(Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Vector('c', 'd'),Vector('c', 'd'),Vector('c', 'd'),Vector('c', 'd'), Prettifier.default)
        val fact = leftSideYes && rightSideNo
        fact shouldBe a [No]
        fact.rawFailureMessage should be (Resources("commaBut"))
        fact.rawNegatedFailureMessage should be (Resources("commaAnd"))
        fact.rawMidSentenceFailureMessage should be (Resources("commaBut"))
        fact.rawMidSentenceNegatedFailureMessage should be (Resources("commaAnd"))
        fact.failureMessage should be (Resources("commaBut", Resources("wasGreaterThan", 'c'.pretty, 'b'.pretty), Resources("wasNotGreaterThan", 'c'.pretty, 'd'.pretty)))
        fact.negatedFailureMessage should be (Resources("commaAnd", Resources("wasGreaterThan", 'c'.pretty, 'b'.pretty), Resources("wasGreaterThan", 'c'.pretty, 'd'.pretty)))
        fact.midSentenceFailureMessage should be (Resources("commaBut", Resources("wasGreaterThan", 'c'.pretty, 'b'.pretty), Resources("wasNotGreaterThan", 'c'.pretty, 'd'.pretty)))
        fact.midSentenceNegatedFailureMessage should be (Resources("commaAnd", Resources("wasGreaterThan", 'c'.pretty, 'b'.pretty), Resources("wasGreaterThan", 'c'.pretty, 'd'.pretty)))
        fact.failureMessageArgs should be (Vector(NegatedFailureMessage(left('c')), MidSentenceFailureMessage(right('c'))))
        fact.negatedFailureMessageArgs should be (Vector(NegatedFailureMessage(left('c')), MidSentenceNegatedFailureMessage(right('c'))))
        fact.midSentenceFailureMessageArgs should be (Vector(MidSentenceNegatedFailureMessage(left('c')), MidSentenceFailureMessage(right('c'))))
        fact.midSentenceNegatedFailureMessageArgs should be (Vector(MidSentenceNegatedFailureMessage(left('c')), MidSentenceNegatedFailureMessage(right('c'))))
      }

        /*
          scala> res0('e')
          res3: org.scalatest.matchers.MatchResult = MatchResult(true,'e' was greater than 'b', but 'e' was not greater than 'd','e' was greater than 'b', and 'e' was greater than 'd','e' was greater than 'b', but 'e' was not greater than 'd','e' was greater than 'b', and 'e' was greater than 'd',Vector(),Vector())

          val left = be > 'b'
          val right = be > 'd'
          val m = left and right
          val fact = m('e')
        */
      "for Yes && Yes" in {
        val leftSideYes = Yes(Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'), Prettifier.default)
        val rightSideYes = Yes(Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'), Prettifier.default)
        val fact = leftSideYes && rightSideYes
        fact shouldBe a [No]
        fact.rawFailureMessage should be (Resources("commaBut"))
        fact.rawNegatedFailureMessage should be (Resources("commaAnd"))
        fact.rawMidSentenceFailureMessage should be (Resources("commaBut"))
        fact.rawMidSentenceNegatedFailureMessage should be (Resources("commaAnd"))
        fact.failureMessage should be (Resources("commaBut", Resources("wasGreaterThan", 'e'.pretty, 'b'.pretty), Resources("wasNotGreaterThan", 'e'.pretty, 'd'.pretty)))
        fact.negatedFailureMessage should be (Resources("commaAnd", Resources("wasGreaterThan", 'e'.pretty, 'b'.pretty), Resources("wasGreaterThan", 'e'.pretty, 'd'.pretty)))
        fact.midSentenceFailureMessage should be (Resources("commaBut", Resources("wasGreaterThan", 'e'.pretty, 'b'.pretty), Resources("wasNotGreaterThan", 'e'.pretty, 'd'.pretty))) 
        fact.midSentenceNegatedFailureMessage should be (Resources("commaAnd", Resources("wasGreaterThan", 'e'.pretty, 'b'.pretty), Resources("wasGreaterThan", 'e'.pretty, 'd'.pretty)))
        fact.failureMessageArgs should be (Vector(NegatedFailureMessage(left('e')), MidSentenceFailureMessage(right('e'))))
        fact.negatedFailureMessageArgs should be (Vector(NegatedFailureMessage(left('e')), MidSentenceNegatedFailureMessage(right('e'))))
        fact.midSentenceFailureMessageArgs should be (Vector(MidSentenceNegatedFailureMessage(left('e')), MidSentenceFailureMessage(right('e'))))
        fact.midSentenceNegatedFailureMessageArgs should be (Vector(MidSentenceNegatedFailureMessage(left('e')), MidSentenceNegatedFailureMessage(right('e'))))
      }
    }
  }
  "The MatchResult obtained from or-ing two Matchers" - {
    /*
      scala> be > 'b' or be > 'd'
      res0: org.scalatest.matchers.Matcher[Char] = <function1>
    */
    "should be lazy about constructing strings" - {

      /*
      scala> res0('a')
      res1: org.scalatest.matchers.MatchResult = MatchResult(false,'a' was not greater than 'b', and 'a' was not greater than 'd','a' was not greater than 'b', and 'a' was greater than 'd','a' was not greater than 'b', and 'a' was not greater than 'd','a' was not greater than 'b', and 'a' was greater than 'd',Vector(),Vector())

      val left = be > 'b'
      val right = be > 'd'
      val m = left or right
      val fact = m('a')
      */
      "for No || No" in {
        val leftSideNo = No(Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'), Prettifier.default)
        val rightSideNo = No(Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'), Prettifier.default)
        val fact = leftSideNo || rightSideNo
        fact shouldBe a [No]
        fact.rawFailureMessage should be (Resources("commaAnd"))
        fact.rawNegatedFailureMessage should be (Resources("commaAnd"))
        fact.rawMidSentenceFailureMessage should be (Resources("commaAnd"))
        fact.rawMidSentenceNegatedFailureMessage should be (Resources("commaAnd"))
        fact.failureMessage should be (Resources("commaAnd", Resources("wasNotGreaterThan", 'a'.pretty, 'b'.pretty), Resources("wasNotGreaterThan", 'a'.pretty, 'd'.pretty)))
        fact.negatedFailureMessage should be (Resources("commaAnd", Resources("wasNotGreaterThan", 'a'.pretty, 'b'.pretty), Resources("wasGreaterThan", 'a'.pretty, 'd'.pretty)))
        fact.midSentenceFailureMessage should be (Resources("commaAnd", Resources("wasNotGreaterThan", 'a'.pretty, 'b'.pretty), Resources("wasNotGreaterThan", 'a'.pretty, 'd'.pretty)))
        fact.midSentenceNegatedFailureMessage should be (Resources("commaAnd", Resources("wasNotGreaterThan", 'a'.pretty, 'b'.pretty), Resources("wasGreaterThan", 'a'.pretty, 'd'.pretty)))
        fact.failureMessageArgs should be (Vector(FailureMessage(left('a')), MidSentenceFailureMessage(right('a'))))
        fact.negatedFailureMessageArgs should be (Vector(FailureMessage(left('a')), MidSentenceNegatedFailureMessage(right('a'))))
        fact.midSentenceFailureMessageArgs should be (Vector(MidSentenceFailureMessage(left('a')), MidSentenceFailureMessage(right('a'))))
        fact.midSentenceNegatedFailureMessageArgs should be (Vector(MidSentenceFailureMessage(left('a')), MidSentenceNegatedFailureMessage(right('a'))))
       }

      /*
        scala> be > 'b' or be < 'd'
        res4: org.scalatest.matchers.Matcher[Char] = <function1>

        scala> res4('a')
        res5: org.scalatest.matchers.MatchResult = MatchResult(true,'a' was not greater than 'b', and 'a' was not less than 'd','a' was not greater than 'b', and 'a' was less than 'd','a' was not greater than 'b', and 'a' was not less than 'd','a' was not greater than 'b', and 'a' was less than 'd',Vector(),Vector())

        val left = be > 'b'
        val right = be < 'd'
        val m = left or right
        val fact = m('a')
      */
      "for No || Yes" in {
        val leftSideNo = No(Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'), Prettifier.default)
        val rightSideYes = Yes(Resources("wasNotLessThan"), Resources("wasLessThan"), Resources("wasNotLessThan"), Resources("wasLessThan"), Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'), Prettifier.default)
        val fact = leftSideNo || rightSideYes
        fact shouldBe a [No]
        fact.rawFailureMessage should be (Resources("commaAnd"))
        fact.rawNegatedFailureMessage should be (Resources("commaAnd"))
        fact.rawMidSentenceFailureMessage should be (Resources("commaAnd"))
        fact.rawMidSentenceNegatedFailureMessage should be (Resources("commaAnd"))
        fact.failureMessage should be (Resources("commaAnd", Resources("wasNotGreaterThan", 'a'.pretty, 'b'.pretty), Resources("wasNotLessThan", 'a'.pretty, 'd'.pretty)))
        fact.negatedFailureMessage should be (Resources("commaAnd", Resources("wasNotGreaterThan", 'a'.pretty, 'b'.pretty), Resources("wasLessThan", 'a'.pretty, 'd'.pretty)))
        fact.midSentenceFailureMessage should be (Resources("commaAnd", Resources("wasNotGreaterThan", 'a'.pretty, 'b'.pretty), Resources("wasNotLessThan", 'a'.pretty, 'd'.pretty)))
        fact.midSentenceNegatedFailureMessage should be (Resources("commaAnd", Resources("wasNotGreaterThan", 'a'.pretty, 'b'.pretty), Resources("wasLessThan", 'a'.pretty, 'd'.pretty)))
        fact.failureMessageArgs should be (Vector(FailureMessage(left('a')), MidSentenceFailureMessage(right('a'))))
        fact.negatedFailureMessageArgs should be (Vector(FailureMessage(left('a')), MidSentenceNegatedFailureMessage(right('a'))))
        fact.midSentenceFailureMessageArgs should be (Vector(MidSentenceFailureMessage(left('a')), MidSentenceFailureMessage(right('a'))))
        fact.midSentenceNegatedFailureMessageArgs should be (Vector(MidSentenceFailureMessage(left('a')), MidSentenceNegatedFailureMessage(right('a'))))
      }

      /*
      scala> res0('c')
      res2: org.scalatest.matchers.MatchResult = MatchResult(true,'c' was greater than 'b','c' was not greater than 'b','c' was greater than 'b','c' was not greater than 'b',Vector(),Vector())

      val m = be > 'b' or be > 'd'
      val fact = m('c')
      */
      "for Yes || No" in {
        val leftSideYes = Yes(Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Vector('c', 'b'),Vector('c', 'b'),Vector('c', 'b'),Vector('c', 'b'), Prettifier.default)
        val rightSideNo = No(Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Vector('c', 'd'),Vector('c', 'd'),Vector('c', 'd'),Vector('c', 'd'), Prettifier.default)
        val fact = leftSideYes || rightSideNo
        fact shouldBe a [Yes]
        fact.rawFailureMessage should be (Resources("wasGreaterThan"))
        fact.rawNegatedFailureMessage should be (Resources("wasNotGreaterThan"))
        fact.rawMidSentenceFailureMessage should be (Resources("wasGreaterThan"))
        fact.rawMidSentenceNegatedFailureMessage should be (Resources("wasNotGreaterThan"))
        fact.failureMessage should be (Resources("wasGreaterThan", 'c'.pretty, 'b'.pretty))
        fact.negatedFailureMessage should be (Resources("wasNotGreaterThan", 'c'.pretty, 'b'.pretty))
        fact.midSentenceFailureMessage should be (Resources("wasGreaterThan", 'c'.pretty, 'b'.pretty))
        fact.midSentenceNegatedFailureMessage should be (Resources("wasNotGreaterThan", 'c'.pretty, 'b'.pretty))
        fact.failureMessageArgs should be (Vector('c', 'b'))
        fact.negatedFailureMessageArgs should be (Vector('c', 'b'))
      }

      /*
      scala> res0('e')
      res3: org.scalatest.matchers.MatchResult = MatchResult(true,'e' was greater than 'b','e' was not greater than 'b','e' was greater than 'b','e' was not greater than 'b',Vector(),Vector())
       val m = be > 'b' or be > 'd'
       val fact = m('e')
      */
      "for Yes || Yes" in {
        val leftSideYes = Yes(Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'), Prettifier.default)
        val rightSideYes = Yes(Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Resources("wasNotGreaterThan"), Resources("wasGreaterThan"), Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'), Prettifier.default)
        val fact = leftSideYes || rightSideYes
        fact shouldBe a [Yes]
        fact.rawFailureMessage should be (Resources("wasGreaterThan"))
        fact.rawNegatedFailureMessage should be (Resources("wasNotGreaterThan"))
        fact.rawMidSentenceFailureMessage should be (Resources("wasGreaterThan"))
        fact.rawMidSentenceNegatedFailureMessage should be (Resources("wasNotGreaterThan"))
        fact.failureMessage should be (Resources("wasGreaterThan", 'e'.pretty, 'b'.pretty))
        fact.negatedFailureMessage should be (Resources("wasNotGreaterThan", 'e'.pretty, 'b'.pretty))
        fact.midSentenceFailureMessage should be (Resources("wasGreaterThan", 'e'.pretty, 'b'.pretty))
        fact.midSentenceNegatedFailureMessage should be (Resources("wasNotGreaterThan", 'e'.pretty, 'b'.pretty))
        fact.failureMessageArgs should be (Vector('e', 'b'))
        fact.negatedFailureMessageArgs should be (Vector('e', 'b'))
      }
    }
  }
*/
}
