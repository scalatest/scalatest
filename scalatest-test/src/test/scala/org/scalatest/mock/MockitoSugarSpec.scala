package org.scalatest.mock

import org.scalatest.{Matchers, FlatSpec}
import org.mockito.BDDMockito._

class MockitoSugarSpec extends FlatSpec with Matchers with MockitoSugar {

  class Service {
    def method(input: String): Int = 0
  }

  "The MockitoSugar trait" should "provide sugar for invoking with methods that take matchers" in {
    val serviceMock = mock[Service]
    given(serviceMock.method(any[String])) willReturn 42
    serviceMock.method("hello") shouldEqual 42
    serviceMock.method("world") shouldEqual 42
  }

}
