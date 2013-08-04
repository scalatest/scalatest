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
import Inside._

class MatchResultSpec extends FreeSpec with Matchers {

  "A MatchResult" - {
    val mr = MatchResult(false, "1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2")
    "can be negated" in {
      mr should equal (MatchResult(false, "1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2"))
      mr.negated should equal (MatchResult(true, "1 equaled 2", "1 did not equal 2", "1 equaled 2", "1 did not equal 2"))
      val mr2 = MatchResult(false, "{0} did not equal null", "The reference equaled null", "{0} did not equal null", "the reference equaled null", Vector("howdy"), Vector.empty)
      mr2 should have (
        'matches (false),
        'failureMessage ("howdy did not equal null"),
        'negatedFailureMessage ("The reference equaled null"),
        'midSentenceFailureMessage ("howdy did not equal null"),
        'midSentenceNegatedFailureMessage ("the reference equaled null"),
        'rawFailureMessage ("{0} did not equal null"),
        'rawNegatedFailureMessage ("The reference equaled null"),
        'rawMidSentenceFailureMessage ("{0} did not equal null"),
        'rawMidSentenceNegatedFailureMessage ("the reference equaled null"),
        'failureMessageArgs(Vector("howdy")),
        'negatedFailureMessageArgs(Vector.empty)
      )
      val mr2Negated = mr2.negated
       mr2Negated should equal (MatchResult(true, "The reference equaled null", "{0} did not equal null", "the reference equaled null", "{0} did not equal null", Vector.empty, Vector("howdy")))
      mr2Negated should have (
        'matches (true),
        'failureMessage ("The reference equaled null"),
        'negatedFailureMessage ("howdy did not equal null"),
        'midSentenceFailureMessage ("the reference equaled null"),
        'midSentenceNegatedFailureMessage ("howdy did not equal null"),
        'rawFailureMessage ("The reference equaled null"),
        'rawNegatedFailureMessage ("{0} did not equal null"),
        'rawMidSentenceFailureMessage ("the reference equaled null"),
        'rawMidSentenceNegatedFailureMessage ("{0} did not equal null"),
        'failureMessageArgs(Vector.empty),
        'negatedFailureMessageArgs(Vector("howdy"))
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
        'negatedFailureMessageArgs(Vector(1, 2))
      )
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
        'negatedFailureMessageArgs(Vector.empty)
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
        'negatedFailureMessageArgs(Vector.empty)
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
        'negatedFailureMessageArgs(Vector.empty)
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
        'negatedFailureMessageArgs(Vector.empty)
      )
    }
    "that takes six strings should work correctly" in {
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
        'negatedFailureMessageArgs(Vector(42.0))
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
        'negatedFailureMessageArgs(Vector("foo", "fie"))
      )
    }
  }

  "The MatchResult obtained from and-ing two Matchers" - {
    "should be lazy about constructing strings" - {
      "for false and false" is pending
       // scala> be > 'c' and be > 'd'
       // res4: org.scalatest.matchers.Matcher[Char] = <function1>

       // scala> res4('a')
       // res5: org.scalatest.matchers.MatchResult = MatchResult(false,'a' was not greater than 'c','a' was greater than 'c','a' was not greater than 'c','a' was greater than 'c',Vector(),Vector())

      "for false and true" is pending
      "for true and false" is pending
      "for true and true" is pending
    }
  }
  "The MatchResult obtained from or-ing two Matchers" - {
    "should be lazy about constructing strings" - {
      "for false and false" is pending
      "for false and true" is pending
      "for true and false" is pending
      "for true and true" is pending
    }
  }
/*
I'll need to add Prettifier to MatchResult. Because need to do that lazily too. This means
I can actually have a withPrettifier construct that replaces the Prettifier, which also means
that the TestFailedException and TestCanceledException should have this lazy too. Else it will be too late.
I think this is added to matchResult as two more IndexedSeqs:
failureMessagePrettifiers(Vector[Prettifier])
negatedFailureMessagePrettifiers(Vector[Prettifier])

Or maybe make the args have a prettifer in the args itself:

failureMessageArgs: IndexedSeq[(Any, Prettifier)]

diffString is another one. This one requires two strings. So first you prettify, then you ... well that's not
how I have been doing it. First I diffed, then I prettified. Wierd. Trouble is that it requires two strings. So
maybe this is a Boolean flag. I'd need it for each of the four things:
diffFailureMessageStrings
diffNegatedFailureMessageStrings
// Ah, I just need two flags. regular will hold for mid-sentence. By default these can be false. OK. Just put
// them last.
And I think what I'd do is look for two args. And if it is two and exactly two, we'd do the diffstrings thing
at that point, then plug that result into the algo with the prettifiers of course. But I think that diffstrings
should just not make any change if a [ or ] is found in the string. Else it is even more confusing.
Also, the diffing could be something done by reporters. If there's color, it could be done in color instead
of []'s. But if nocolor, then ... Yes. I quite like that.

maybe it is a lazy val on TestFailedException, TestCanceledException

So question is do I make Prettifier vectors or use a tuple, or a bundle?
case class PrettyArg(arg: Any, prettifier: Prettifier)

Oh, and the prettifier can be a scalautils thing, because it can be used in production code
for debug messages. println(x.pretty)

No, that doesn't make sense. There's one Prettifier for a MatchResult. Oops! But then how would
I combine them? Yes, can't. 

What if PrettyArg was more a wrapper:

case class PrettyArg(val arg: Any, prettify: Prettifier) {
  override def toString: String = prettify(arg)
}

OK. Then it can also compose another prettifier:

case class PrettyArg(val arg: Any, prettify: Prettifier) {
  override def toString: String = prettify(arg) 
  def 
}

Almost want to say if this other guy has prettified, then ...?

Maybe a prettifier has an isDefinedAt method. But that's wrong.

Well, I think it is time to write a test.

Then I hmm.

Seems the ScalaUtils thing would simply be an implicit Prettifier:

trait PrettyMethods {

  implicit val defaultPrettifier = Prettifier.default

  implicit class Prettyizer(o: Any)(implicit prettify: Prettifier) {
    def pretty: String = prettify(o)
  }
}
object PrettyMethods extends PrettyMethods

*/
}
 
