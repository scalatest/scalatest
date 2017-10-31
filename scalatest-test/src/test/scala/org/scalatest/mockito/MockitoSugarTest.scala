package org.scalatest.mockito

import org.mockito.stubbing.Answer
import org.scalatest.words.MatcherWords
import org.scalatest.WordSpec

class MockitoSugarTest extends WordSpec with MatcherWords
{
  private val withSugar = new MockitoSugar {}

  "An answer sugar" when {
    "returns None" should {
      "create an `org.mockito.stubbing.Answer[Option[_]]` by provided result" in {
        assert(withSugar.answer({ _ => None }).isInstanceOf[Answer[Option[_]]])
      }
    }
    "returns Seq" should {
      "create an `org.mockito.stubbing.Answer[Seq[_]]` by provided result" in {
        assert(withSugar.answer({ _ => Seq() }).isInstanceOf[Answer[Seq[_]]])
      }
    }
    "returns Any" should {
      "create an `org.mockito.stubbing.Answer[_]` by provided result" in {
        assert(withSugar.answer({ _ => 0.asInstanceOf[Any] }).isInstanceOf[Answer[_]])
      }
    }
  }
}
