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
package org.scalatest.matchers

import org.scalatest._
import Inside._
import org.scalactic.{PrettyMethods, Prettifier}

class MatchResultSpec extends FreeSpec with Matchers with PrettyMethods {

  "A MatchResult" - {
    val mr = MatchResult(false, "1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2")
    "can be negated" in {
      mr should equal (MatchResult(false, "1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2"))
      mr.negated should equal (MatchResult(true, "1 equaled 2", "1 did not equal 2", "1 equaled 2", "1 did not equal 2"))
      val mr2 = MatchResult(false, "{0} did not equal null", "The reference equaled null", "{0} did not equal null", "the reference equaled null", Vector("howdy"), Vector.empty)
      mr2 should have (
        'matches (false),
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
      val mr2Negated = mr2.negated
       mr2Negated should equal (MatchResult(true, "The reference equaled null", "{0} did not equal null", "the reference equaled null", "{0} did not equal null", Vector.empty, Vector("howdy")))
      mr2Negated should have (
        'matches (true),
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
    "can be pattern matched via an extractor for the failureMessage if it doesn't match" in {
      inside (mr) { case MatchFailed(failureMessage) => 
        failureMessage should be ("1 did not equal 2")
      }
    }
    "can be pattern matched via an extractor for the negatedFailureMessage if it does match" in {
      inside (mr.negated) { case MatchSucceeded(negatedFailureMessage) => 
        negatedFailureMessage should be ("1 did not equal 2")
      }
    }
    "should construct localized strings from the raw strings and args" in {
      val mr = MatchResult(false, "{0} did not equal {1}", "{0} equaled {1}", "{0} did not equal {1}", "{0} equaled {1}", Vector(1, 2), Vector(1, 2))
      mr should have (
        'matches (false),
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
      val mr = MatchResult(false, "{0} did not equal {1}", "{0} equaled {1}", "{0} did not equal {1}", "{0} equaled {1}", Vector.empty, Vector.empty, Vector(1, 2), Vector.empty, Prettifier.default)
      mr.midSentenceFailureMessage should be ("1 did not equal 2")
    }

    "should use midSentenceNegatedFailureMessageArgs to construct midSentenceNegatedFailureMessage" in {
      val mr = MatchResult(false, "{0} did not equal {1}", "{0} equaled {1}", "{0} did not equal {1}", "{0} equaled {1}", Vector.empty, Vector.empty, Vector.empty, Vector(1, 2), Prettifier.default)
      mr.midSentenceNegatedFailureMessage should be ("1 equaled 2")
    }
  }

  "The MatchResult companion object factory method" - {
    "that takes two strings should work correctly" in {
      val mr = MatchResult(true, "one", "two")
      mr should have (
        'matches (true),
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
      val ms = MatchResult(false, "aaa", "bbb")
      ms should have (
        'matches (false),
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
      val mr = MatchResult(true, "one", "two", "three", "four")
      mr should have (
        'matches (true),
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
      val ms = MatchResult(false, "aaa", "bbb", "ccc", "ddd")
      ms should have (
        'matches (false),
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
      val mr = MatchResult(true, "one", "two", "three", "four", Vector(42), Vector(42.0))
      mr should have (
        'matches (true),
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
      val ms = MatchResult(false, "aaa", "bbb", "ccc", "ddd", Vector("ho", "he"), Vector("foo", "fie"))
      ms should have (
        'matches (false),
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
      val mr = MatchResult(true, "one", "two", Vector(42))
      mr should have (
        'matches (true),
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
      val ms = MatchResult(false, "aaa", "bbb", Vector("ho", "he"))
      ms should have (
        'matches (false),
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
      val mr = MatchResult(true, "one", "two", Vector(42), Vector(42.0))
      mr should have (
        'matches (true),
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
      val ms = MatchResult(false, "aaa", "bbb", Vector("ho", "he"), Vector("foo", "fie"))
      ms should have (
        'matches (false),
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
      val mr = MatchResult(true, "one", "two", "three", "four", Vector(1), Vector(2), Vector(3), Vector(4))
      mr should have (
        'matches (true),
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
      val ms = MatchResult(false, "aaa", "bbb", "ccc", "ddd", Vector('A'), Vector('B'), Vector('C'), Vector('D'))
      ms should have (
        'matches (false),
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

  "The MatchResult obtained from ScalaTest matchers should have localized raw strings and args" - {
    "for be > 'b'" in {
      val m = be > 'b'
      m('a') should have (
        'matches (false),
        'rawFailureMessage (Resources.rawWasNotGreaterThan),
        'rawNegatedFailureMessage (Resources.rawWasGreaterThan),
        'rawMidSentenceFailureMessage (Resources.rawWasNotGreaterThan),
        'rawMidSentenceNegatedFailureMessage (Resources.rawWasGreaterThan),
        'failureMessageArgs(Vector('a', 'b')),
        'negatedFailureMessageArgs(Vector('a', 'b'))
      )
    }
    "for be < 'b'" in {
      val m = be < 'b'
      m('c') should have (
        'matches (false),
        'rawFailureMessage (Resources.rawWasNotLessThan),
        'rawNegatedFailureMessage (Resources.rawWasLessThan),
        'rawMidSentenceFailureMessage (Resources.rawWasNotLessThan),
        'rawMidSentenceNegatedFailureMessage (Resources.rawWasLessThan),
        'failureMessageArgs(Vector('c', 'b')),
        'negatedFailureMessageArgs(Vector('c', 'b'))
      )
    }
  }

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
      "for false and false" in {
        val m = be > 'b' and be > 'd'
        val mr = m('a')
        mr.matches shouldBe false
        mr.rawFailureMessage should be (Resources.rawWasNotGreaterThan)
        mr.rawNegatedFailureMessage should be (Resources.rawWasGreaterThan)
        mr.rawMidSentenceFailureMessage should be (Resources.rawWasNotGreaterThan)
        mr.rawMidSentenceNegatedFailureMessage should be (Resources.rawWasGreaterThan)
        mr.failureMessage should be (Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty))
        mr.negatedFailureMessage should be (Resources.wasGreaterThan('a'.pretty, 'b'.pretty))
        mr.midSentenceFailureMessage should be (Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty))
        mr.midSentenceNegatedFailureMessage should be (Resources.wasGreaterThan('a'.pretty, 'b'.pretty))
        mr.failureMessageArgs should be (Vector('a', 'b'))
        mr.negatedFailureMessageArgs should be (Vector('a', 'b'))
      }

       /*
         scala> be > 'b' and be < 'd'
         res4: org.scalatest.matchers.Matcher[Char] = <function1>

         scala> res4('a')
         res5: org.scalatest.matchers.MatchResult = MatchResult(false,'a' was not greater than 'b','a' was greater than 'b','a' was not greater than 'b','a' was greater than 'b',Vector(),Vector())
       */
      "for false and true" in {
        val m = be > 'b' and be < 'd'
        val mr = m('a')
        mr.matches shouldBe false
        mr.rawFailureMessage should be (Resources.rawWasNotGreaterThan)
        mr.rawNegatedFailureMessage should be (Resources.rawWasGreaterThan)
        mr.rawMidSentenceFailureMessage should be (Resources.rawWasNotGreaterThan)
        mr.rawMidSentenceNegatedFailureMessage should be (Resources.rawWasGreaterThan)
        mr.failureMessage should be (Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty))
        mr.negatedFailureMessage should be (Resources.wasGreaterThan('a'.pretty, 'b'.pretty))
        mr.midSentenceFailureMessage should be (Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty))
        mr.midSentenceNegatedFailureMessage should be (Resources.wasGreaterThan('a'.pretty, 'b'.pretty))
        mr.failureMessageArgs should be (Vector('a', 'b'))
        mr.negatedFailureMessageArgs should be (Vector('a', 'b'))
      }

        /*
          scala> res0('c')
          res2: org.scalatest.matchers.MatchResult = MatchResult(false,'c' was greater than 'b', but 'c' was not greater than 'd','c' was greater than 'b', and 'c' was greater than 'd','c' was greater than 'b', but 'c' was not greater than 'd','c' was greater than 'b', and 'c' was greater than 'd',Vector(),Vector())
        */
      "for true and false" in {
        val left = be > 'b'
        val right = be > 'd'
        val m = left and right
        val mr = m('c')
        mr.matches shouldBe false
        mr.rawFailureMessage should be (Resources.rawCommaBut)
        mr.rawNegatedFailureMessage should be (Resources.rawCommaAnd)
        mr.rawMidSentenceFailureMessage should be (Resources.rawCommaBut)
        mr.rawMidSentenceNegatedFailureMessage should be (Resources.rawCommaAnd)
        mr.failureMessage should be (Resources.commaBut(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('c'.pretty, 'd'.pretty)))
        mr.negatedFailureMessage should be (Resources.commaAnd(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasGreaterThan('c'.pretty, 'd'.pretty)))
        mr.midSentenceFailureMessage should be (Resources.commaBut(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('c'.pretty, 'd'.pretty)))
        mr.midSentenceNegatedFailureMessage should be (Resources.commaAnd(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasGreaterThan('c'.pretty, 'd'.pretty)))
        mr.failureMessageArgs should be (Vector(NegatedFailureMessage(left('c')), MidSentenceFailureMessage(right('c'))))
        mr.negatedFailureMessageArgs should be (Vector(NegatedFailureMessage(left('c')), MidSentenceNegatedFailureMessage(right('c'))))
        mr.midSentenceFailureMessageArgs should be (Vector(MidSentenceNegatedFailureMessage(left('c')), MidSentenceFailureMessage(right('c'))))
        mr.midSentenceNegatedFailureMessageArgs should be (Vector(MidSentenceNegatedFailureMessage(left('c')), MidSentenceNegatedFailureMessage(right('c'))))
      }

        /*
          scala> res0('e')
          res3: org.scalatest.matchers.MatchResult = MatchResult(true,'e' was greater than 'b', but 'e' was not greater than 'd','e' was greater than 'b', and 'e' was greater than 'd','e' was greater than 'b', but 'e' was not greater than 'd','e' was greater than 'b', and 'e' was greater than 'd',Vector(),Vector())
        */
      "for true and true" in {
        val left = be > 'b'
        val right = be > 'd'
        val m = left and right
        val mr = m('e')
        mr.matches shouldBe true
        mr.rawFailureMessage should be (Resources.rawCommaBut)
        mr.rawNegatedFailureMessage should be (Resources.rawCommaAnd)
        mr.rawMidSentenceFailureMessage should be (Resources.rawCommaBut)
        mr.rawMidSentenceNegatedFailureMessage should be (Resources.rawCommaAnd)
        mr.failureMessage should be (Resources.commaBut(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('e'.pretty, 'd'.pretty)))
        mr.negatedFailureMessage should be (Resources.commaAnd(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasGreaterThan('e'.pretty, 'd'.pretty)))
        mr.midSentenceFailureMessage should be (Resources.commaBut(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('e'.pretty, 'd'.pretty))) 
        mr.midSentenceNegatedFailureMessage should be (Resources.commaAnd(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasGreaterThan('e'.pretty, 'd'.pretty)))
        mr.failureMessageArgs should be (Vector(NegatedFailureMessage(left('e')), MidSentenceFailureMessage(right('e'))))
        mr.negatedFailureMessageArgs should be (Vector(NegatedFailureMessage(left('e')), MidSentenceNegatedFailureMessage(right('e'))))
        mr.midSentenceFailureMessageArgs should be (Vector(MidSentenceNegatedFailureMessage(left('e')), MidSentenceFailureMessage(right('e'))))
        mr.midSentenceNegatedFailureMessageArgs should be (Vector(MidSentenceNegatedFailureMessage(left('e')), MidSentenceNegatedFailureMessage(right('e'))))
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
      */
      "for false or false" in {
        val left = be > 'b'
        val right = be > 'd'
        val m = left or right
        val mr = m('a')
        mr.matches shouldBe false
        mr.rawFailureMessage should be (Resources.rawCommaAnd)
        mr.rawNegatedFailureMessage should be (Resources.rawCommaAnd)
        mr.rawMidSentenceFailureMessage should be (Resources.rawCommaAnd)
        mr.rawMidSentenceNegatedFailureMessage should be (Resources.rawCommaAnd)
        mr.failureMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('a'.pretty, 'd'.pretty)))
        mr.negatedFailureMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasGreaterThan('a'.pretty, 'd'.pretty)))
        mr.midSentenceFailureMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('a'.pretty, 'd'.pretty)))
        mr.midSentenceNegatedFailureMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasGreaterThan('a'.pretty, 'd'.pretty)))
        mr.failureMessageArgs should be (Vector(FailureMessage(left('a')), MidSentenceFailureMessage(right('a'))))
        mr.negatedFailureMessageArgs should be (Vector(FailureMessage(left('a')), MidSentenceNegatedFailureMessage(right('a'))))
        mr.midSentenceFailureMessageArgs should be (Vector(MidSentenceFailureMessage(left('a')), MidSentenceFailureMessage(right('a'))))
        mr.midSentenceNegatedFailureMessageArgs should be (Vector(MidSentenceFailureMessage(left('a')), MidSentenceNegatedFailureMessage(right('a'))))
       }

      /*
        scala> be > 'b' or be < 'd'
        res4: org.scalatest.matchers.Matcher[Char] = <function1>

        scala> res4('a')
        res5: org.scalatest.matchers.MatchResult = MatchResult(true,'a' was not greater than 'b', and 'a' was not less than 'd','a' was not greater than 'b', and 'a' was less than 'd','a' was not greater than 'b', and 'a' was not less than 'd','a' was not greater than 'b', and 'a' was less than 'd',Vector(),Vector())
      */
      "for false or true" in {
        val left = be > 'b'
        val right = be < 'd'
        val m = left or right
        val mr = m('a')
        mr.matches shouldBe true
        mr.rawFailureMessage should be (Resources.rawCommaAnd)
        mr.rawNegatedFailureMessage should be (Resources.rawCommaAnd)
        mr.rawMidSentenceFailureMessage should be (Resources.rawCommaAnd)
        mr.rawMidSentenceNegatedFailureMessage should be (Resources.rawCommaAnd)
        mr.failureMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotLessThan('a'.pretty, 'd'.pretty)))
        mr.negatedFailureMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasLessThan('a'.pretty, 'd'.pretty)))
        mr.midSentenceFailureMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotLessThan('a'.pretty, 'd'.pretty)))
        mr.midSentenceNegatedFailureMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasLessThan('a'.pretty, 'd'.pretty)))
        mr.failureMessageArgs should be (Vector(FailureMessage(left('a')), MidSentenceFailureMessage(right('a'))))
        mr.negatedFailureMessageArgs should be (Vector(FailureMessage(left('a')), MidSentenceNegatedFailureMessage(right('a'))))
        mr.midSentenceFailureMessageArgs should be (Vector(MidSentenceFailureMessage(left('a')), MidSentenceFailureMessage(right('a'))))
        mr.midSentenceNegatedFailureMessageArgs should be (Vector(MidSentenceFailureMessage(left('a')), MidSentenceNegatedFailureMessage(right('a'))))
      }

      /*
      scala> res0('c')
      res2: org.scalatest.matchers.MatchResult = MatchResult(true,'c' was greater than 'b','c' was not greater than 'b','c' was greater than 'b','c' was not greater than 'b',Vector(),Vector())
      */
      "for true or false" in {
        val m = be > 'b' or be > 'd'
        val mr = m('c')
        mr.matches shouldBe true
        mr.rawFailureMessage should be (Resources.rawWasNotGreaterThan)
        mr.rawNegatedFailureMessage should be (Resources.rawWasGreaterThan)
        mr.rawMidSentenceFailureMessage should be (Resources.rawWasNotGreaterThan)
        mr.rawMidSentenceNegatedFailureMessage should be (Resources.rawWasGreaterThan)
        mr.failureMessage should be (Resources.wasNotGreaterThan('c'.pretty, 'b'.pretty))
        mr.negatedFailureMessage should be (Resources.wasGreaterThan('c'.pretty, 'b'.pretty))
        mr.midSentenceFailureMessage should be (Resources.wasNotGreaterThan('c'.pretty, 'b'.pretty))
        mr.midSentenceNegatedFailureMessage should be (Resources.wasGreaterThan('c'.pretty, 'b'.pretty))
        mr.failureMessageArgs should be (Vector('c', 'b'))
        mr.negatedFailureMessageArgs should be (Vector('c', 'b'))
      }

      /*
      scala> res0('e')
      res3: org.scalatest.matchers.MatchResult = MatchResult(true,'e' was greater than 'b','e' was not greater than 'b','e' was greater than 'b','e' was not greater than 'b',Vector(),Vector())
      */
      "for true or true" in {
        val m = be > 'b' or be > 'd'
        val mr = m('e')
        mr.matches shouldBe true
        mr.rawFailureMessage should be (Resources.rawWasNotGreaterThan)
        mr.rawNegatedFailureMessage should be (Resources.rawWasGreaterThan)
        mr.rawMidSentenceFailureMessage should be (Resources.rawWasNotGreaterThan)
        mr.rawMidSentenceNegatedFailureMessage should be (Resources.rawWasGreaterThan)
        mr.failureMessage should be (Resources.wasNotGreaterThan('e'.pretty, 'b'.pretty))
        mr.negatedFailureMessage should be (Resources.wasGreaterThan('e'.pretty, 'b'.pretty))
        mr.midSentenceFailureMessage should be (Resources.wasNotGreaterThan('e'.pretty, 'b'.pretty))
        mr.midSentenceNegatedFailureMessage should be (Resources.wasGreaterThan('e'.pretty, 'b'.pretty))
        mr.failureMessageArgs should be (Vector('e', 'b'))
        mr.negatedFailureMessageArgs should be (Vector('e', 'b'))
      }
    }
  }
}
 
