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
      mr2.matches shouldBe (false)
      mr2.failureMessage shouldBe ("\"howdy\" did not equal null")
      mr2.negatedFailureMessage shouldBe ("The reference equaled null")
      mr2.midSentenceFailureMessage shouldBe ("\"howdy\" did not equal null")
      mr2.midSentenceNegatedFailureMessage shouldBe ("the reference equaled null")
      mr2.rawFailureMessage shouldBe ("{0} did not equal null")
      mr2.rawNegatedFailureMessage shouldBe ("The reference equaled null")
      mr2.rawMidSentenceFailureMessage shouldBe ("{0} did not equal null")
      mr2.rawMidSentenceNegatedFailureMessage shouldBe ("the reference equaled null")
      mr2.failureMessageArgs shouldBe (Vector("howdy"))
      mr2.negatedFailureMessageArgs shouldBe (Vector.empty)
      mr2.midSentenceFailureMessageArgs shouldBe (Vector("howdy"))
      mr2.midSentenceNegatedFailureMessageArgs shouldBe (Vector.empty)

      val mr2Negated = mr2.negated
      mr2Negated should equal (MatchResult(true, "The reference equaled null", "{0} did not equal null", "the reference equaled null", "{0} did not equal null", Vector.empty, Vector("howdy")))
      mr2Negated.matches shouldBe (true)
      mr2Negated.failureMessage shouldBe ("The reference equaled null")
      mr2Negated.negatedFailureMessage shouldBe ("\"howdy\" did not equal null")
      mr2Negated.midSentenceFailureMessage shouldBe ("the reference equaled null")
      mr2Negated.midSentenceNegatedFailureMessage shouldBe ("\"howdy\" did not equal null")
      mr2Negated.rawFailureMessage shouldBe ("The reference equaled null")
      mr2Negated.rawNegatedFailureMessage shouldBe ("{0} did not equal null")
      mr2Negated.rawMidSentenceFailureMessage shouldBe ("the reference equaled null")
      mr2Negated.rawMidSentenceNegatedFailureMessage shouldBe ("{0} did not equal null")
      mr2Negated.failureMessageArgs shouldBe (Vector.empty)
      mr2Negated.negatedFailureMessageArgs shouldBe (Vector("howdy"))
      mr2Negated.midSentenceFailureMessageArgs shouldBe (Vector.empty)
      mr2Negated.midSentenceNegatedFailureMessageArgs shouldBe (Vector("howdy"))
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
      mr.matches shouldBe (false)
      mr.failureMessage shouldBe ("1 did not equal 2")
      mr.negatedFailureMessage shouldBe ("1 equaled 2")
      mr.midSentenceFailureMessage shouldBe ("1 did not equal 2")
      mr.midSentenceNegatedFailureMessage shouldBe ("1 equaled 2")
      mr.rawFailureMessage shouldBe ("{0} did not equal {1}")
      mr.rawNegatedFailureMessage shouldBe ("{0} equaled {1}")
      mr.rawMidSentenceFailureMessage shouldBe ("{0} did not equal {1}")
      mr.rawMidSentenceNegatedFailureMessage shouldBe ("{0} equaled {1}")
      mr.failureMessageArgs shouldBe (Vector(1, 2))
      mr.negatedFailureMessageArgs shouldBe (Vector(1, 2))
      mr.midSentenceFailureMessageArgs shouldBe (Vector(1, 2))
      mr.midSentenceNegatedFailureMessageArgs shouldBe (Vector(1, 2))
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
      mr.matches shouldBe (true)
      mr.failureMessage shouldBe ("one")
      mr.negatedFailureMessage shouldBe ("two")
      mr.midSentenceFailureMessage shouldBe ("one")
      mr.midSentenceNegatedFailureMessage shouldBe ("two")
      mr.rawFailureMessage shouldBe ("one")
      mr.rawNegatedFailureMessage shouldBe ("two")
      mr.rawMidSentenceFailureMessage shouldBe ("one")
      mr.rawMidSentenceNegatedFailureMessage shouldBe ("two")
      mr.failureMessageArgs shouldBe (Vector.empty)
      mr.negatedFailureMessageArgs shouldBe (Vector.empty)
      mr.midSentenceFailureMessageArgs shouldBe (Vector.empty)
      mr.midSentenceNegatedFailureMessageArgs shouldBe (Vector.empty)

      val ms = MatchResult(false, "aaa", "bbb")
      ms.matches shouldBe (false)
      ms.failureMessage shouldBe ("aaa")
      ms.negatedFailureMessage shouldBe ("bbb")
      ms.midSentenceFailureMessage shouldBe ("aaa")
      ms.midSentenceNegatedFailureMessage shouldBe ("bbb")
      ms.rawFailureMessage shouldBe ("aaa")
      ms.rawNegatedFailureMessage shouldBe ("bbb")
      ms.rawMidSentenceFailureMessage shouldBe ("aaa")
      ms.rawMidSentenceNegatedFailureMessage shouldBe ("bbb")
      ms.failureMessageArgs shouldBe (Vector.empty)
      ms.negatedFailureMessageArgs shouldBe (Vector.empty)
      ms.midSentenceFailureMessageArgs shouldBe (Vector.empty)
      ms.midSentenceNegatedFailureMessageArgs shouldBe (Vector.empty)
    }
    "that takes four strings should work correctly" in {
      val mr = MatchResult(true, "one", "two", "three", "four")
      mr.matches shouldBe (true)
      mr.failureMessage shouldBe ("one")
      mr.negatedFailureMessage shouldBe ("two")
      mr.midSentenceFailureMessage shouldBe ("three")
      mr.midSentenceNegatedFailureMessage shouldBe ("four")
      mr.rawFailureMessage shouldBe ("one")
      mr.rawNegatedFailureMessage shouldBe ("two")
      mr.rawMidSentenceFailureMessage shouldBe ("three")
      mr.rawMidSentenceNegatedFailureMessage shouldBe ("four")
      mr.failureMessageArgs shouldBe (Vector.empty)
      mr.negatedFailureMessageArgs shouldBe (Vector.empty)
      mr.midSentenceFailureMessageArgs shouldBe (Vector.empty)
      mr.midSentenceNegatedFailureMessageArgs shouldBe (Vector.empty)

      val ms = MatchResult(false, "aaa", "bbb", "ccc", "ddd")
      ms.matches shouldBe (false)
      ms.failureMessage shouldBe ("aaa")
      ms.negatedFailureMessage shouldBe ("bbb")
      ms.midSentenceFailureMessage shouldBe ("ccc")
      ms.midSentenceNegatedFailureMessage shouldBe ("ddd")
      ms.rawFailureMessage shouldBe ("aaa")
      ms.rawNegatedFailureMessage shouldBe ("bbb")
      ms.rawMidSentenceFailureMessage shouldBe ("ccc")
      ms.rawMidSentenceNegatedFailureMessage shouldBe ("ddd")
      ms.failureMessageArgs shouldBe (Vector.empty)
      ms.negatedFailureMessageArgs shouldBe (Vector.empty)
      ms.midSentenceFailureMessageArgs shouldBe (Vector.empty)
      ms.midSentenceNegatedFailureMessageArgs shouldBe (Vector.empty)
    }
    "that takes four strings and two IndexedSeqs should work correctly" in {
      val mr = MatchResult(true, "one", "two", "three", "four", Vector(42), Vector(42.0))
      mr.matches shouldBe (true)
      mr.failureMessage shouldBe ("one")
      mr.negatedFailureMessage shouldBe ("two")
      mr.midSentenceFailureMessage shouldBe ("three")
      mr.midSentenceNegatedFailureMessage shouldBe ("four")
      mr.rawFailureMessage shouldBe ("one")
      mr.rawNegatedFailureMessage shouldBe ("two")
      mr.rawMidSentenceFailureMessage shouldBe ("three")
      mr.rawMidSentenceNegatedFailureMessage shouldBe ("four")
      mr.failureMessageArgs shouldBe (Vector(42))
      mr.negatedFailureMessageArgs shouldBe (Vector(42.0))
      mr.midSentenceFailureMessageArgs shouldBe (Vector(42))
      mr.midSentenceNegatedFailureMessageArgs shouldBe (Vector(42.0))

      val ms = MatchResult(false, "aaa", "bbb", "ccc", "ddd", Vector("ho", "he"), Vector("foo", "fie"))
      ms.matches shouldBe (false)
      ms.failureMessage shouldBe ("aaa")
      ms.negatedFailureMessage shouldBe ("bbb")
      ms.midSentenceFailureMessage shouldBe ("ccc")
      ms.midSentenceNegatedFailureMessage shouldBe ("ddd")
      ms.rawFailureMessage shouldBe ("aaa")
      ms.rawNegatedFailureMessage shouldBe ("bbb")
      ms.rawMidSentenceFailureMessage shouldBe ("ccc")
      ms.rawMidSentenceNegatedFailureMessage shouldBe ("ddd")
      ms.failureMessageArgs shouldBe (Vector("ho", "he"))
      ms.negatedFailureMessageArgs shouldBe (Vector("foo", "fie"))
      ms.midSentenceFailureMessageArgs shouldBe (Vector("ho", "he"))
      ms.midSentenceNegatedFailureMessageArgs shouldBe (Vector("foo", "fie"))
    }
    "that takes two strings and one IndexedSeq should work correctly" in {
      val mr = MatchResult(true, "one", "two", Vector(42))
      mr.matches shouldBe (true)
      mr.failureMessage shouldBe ("one")
      mr.negatedFailureMessage shouldBe ("two")
      mr.midSentenceFailureMessage shouldBe ("one")
      mr.midSentenceNegatedFailureMessage shouldBe ("two")
      mr.rawFailureMessage shouldBe ("one")
      mr.rawNegatedFailureMessage shouldBe ("two")
      mr.rawMidSentenceFailureMessage shouldBe ("one")
      mr.rawMidSentenceNegatedFailureMessage shouldBe ("two")
      mr.failureMessageArgs shouldBe (Vector(42))
      mr.negatedFailureMessageArgs shouldBe (Vector(42))
      mr.midSentenceFailureMessageArgs shouldBe (Vector(42))
      mr.midSentenceNegatedFailureMessageArgs shouldBe (Vector(42))

      val ms = MatchResult(false, "aaa", "bbb", Vector("ho", "he"))
      ms.matches shouldBe (false)
      ms.failureMessage shouldBe ("aaa")
      ms.negatedFailureMessage shouldBe ("bbb")
      ms.midSentenceFailureMessage shouldBe ("aaa")
      ms.midSentenceNegatedFailureMessage shouldBe ("bbb")
      ms.rawFailureMessage shouldBe ("aaa")
      ms.rawNegatedFailureMessage shouldBe ("bbb")
      ms.rawMidSentenceFailureMessage shouldBe ("aaa")
      ms.rawMidSentenceNegatedFailureMessage shouldBe ("bbb")
      ms.failureMessageArgs shouldBe (Vector("ho", "he"))
      ms.negatedFailureMessageArgs shouldBe (Vector("ho", "he"))
      ms.midSentenceFailureMessageArgs shouldBe (Vector("ho", "he"))
      ms.midSentenceNegatedFailureMessageArgs shouldBe (Vector("ho", "he"))
    }
    "that takes two strings and two IndexedSeqs should work correctly" in {
      val mr = MatchResult(true, "one", "two", Vector(42), Vector(42.0))
      mr.matches shouldBe (true)
      mr.failureMessage shouldBe ("one")
      mr.negatedFailureMessage shouldBe ("two")
      mr.midSentenceFailureMessage shouldBe ("one")
      mr.midSentenceNegatedFailureMessage shouldBe ("two")
      mr.rawFailureMessage shouldBe ("one")
      mr.rawNegatedFailureMessage shouldBe ("two")
      mr.rawMidSentenceFailureMessage shouldBe ("one")
      mr.rawMidSentenceNegatedFailureMessage shouldBe ("two")
      mr.failureMessageArgs shouldBe (Vector(42))
      mr.negatedFailureMessageArgs shouldBe (Vector(42.0))
      mr.midSentenceFailureMessageArgs shouldBe (Vector(42))
      mr.midSentenceNegatedFailureMessageArgs shouldBe (Vector(42.0))

      val ms = MatchResult(false, "aaa", "bbb", Vector("ho", "he"), Vector("foo", "fie"))
      ms.matches shouldBe (false)
      ms.failureMessage shouldBe ("aaa")
      ms.negatedFailureMessage shouldBe ("bbb")
      ms.midSentenceFailureMessage shouldBe ("aaa")
      ms.midSentenceNegatedFailureMessage shouldBe ("bbb")
      ms.rawFailureMessage shouldBe ("aaa")
      ms.rawNegatedFailureMessage shouldBe ("bbb")
      ms.rawMidSentenceFailureMessage shouldBe ("aaa")
      ms.rawMidSentenceNegatedFailureMessage shouldBe ("bbb")
      ms.failureMessageArgs shouldBe (Vector("ho", "he"))
      ms.negatedFailureMessageArgs shouldBe (Vector("foo", "fie"))
      ms.midSentenceFailureMessageArgs shouldBe (Vector("ho", "he"))
      ms.midSentenceNegatedFailureMessageArgs shouldBe (Vector("foo", "fie"))

    }
    "that takes four strings and four IndexedSeqs should work correctly" in {
      val mr = MatchResult(true, "one", "two", "three", "four", Vector(1), Vector(2), Vector(3), Vector(4))
      mr.matches shouldBe (true)
      mr.failureMessage shouldBe ("one")
      mr.negatedFailureMessage shouldBe ("two")
      mr.midSentenceFailureMessage shouldBe ("three")
      mr.midSentenceNegatedFailureMessage shouldBe ("four")
      mr.rawFailureMessage shouldBe ("one")
      mr.rawNegatedFailureMessage shouldBe ("two")
      mr.rawMidSentenceFailureMessage shouldBe ("three")
      mr.rawMidSentenceNegatedFailureMessage shouldBe ("four")
      mr.failureMessageArgs shouldBe (Vector(1))
      mr.negatedFailureMessageArgs shouldBe (Vector(2))
      mr.midSentenceFailureMessageArgs shouldBe (Vector(3))
      mr.midSentenceNegatedFailureMessageArgs shouldBe (Vector(4))

      val ms = MatchResult(false, "aaa", "bbb", "ccc", "ddd", Vector('A'), Vector('B'), Vector('C'), Vector('D'))
      ms.matches shouldBe (false)
      ms.failureMessage shouldBe ("aaa")
      ms.negatedFailureMessage shouldBe ("bbb")
      ms.midSentenceFailureMessage shouldBe ("ccc")
      ms.midSentenceNegatedFailureMessage shouldBe ("ddd")
      ms.rawFailureMessage shouldBe ("aaa")
      ms.rawNegatedFailureMessage shouldBe ("bbb")
      ms.rawMidSentenceFailureMessage shouldBe ("ccc")
      ms.rawMidSentenceNegatedFailureMessage shouldBe ("ddd")
      ms.failureMessageArgs shouldBe (Vector('A'))
      ms.negatedFailureMessageArgs shouldBe (Vector('B'))
      ms.midSentenceFailureMessageArgs shouldBe (Vector('C'))
      ms.midSentenceNegatedFailureMessageArgs shouldBe (Vector('D'))

    }
  }

  "The MatchResult obtained from ScalaTest matchers should have localized raw strings and args" - {
    "for be > 'b'" in {
      val m = be > 'b'
      m('a').matches shouldBe (false)
      m('a').rawFailureMessage shouldBe (Resources.rawWasNotGreaterThan)
      m('a').rawNegatedFailureMessage shouldBe (Resources.rawWasGreaterThan)
      m('a').rawMidSentenceFailureMessage shouldBe (Resources.rawWasNotGreaterThan)
      m('a').rawMidSentenceNegatedFailureMessage shouldBe (Resources.rawWasGreaterThan)
      m('a').failureMessageArgs shouldBe (Vector('a', 'b'))
      m('a').negatedFailureMessageArgs shouldBe (Vector('a', 'b'))
    }
    "for be < 'b'" in {
      val m = be < 'b'
      m('c').matches shouldBe (false)
      m('c').rawFailureMessage shouldBe (Resources.rawWasNotLessThan)
      m('c').rawNegatedFailureMessage shouldBe (Resources.rawWasLessThan)
      m('c').rawMidSentenceFailureMessage shouldBe (Resources.rawWasNotLessThan)
      m('c').rawMidSentenceNegatedFailureMessage shouldBe (Resources.rawWasLessThan)
      m('c').failureMessageArgs shouldBe (Vector('c', 'b'))
      m('c').negatedFailureMessageArgs shouldBe (Vector('c', 'b'))
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
 
