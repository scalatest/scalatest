package org.scalatest.time

import org.scalatest.matchers.{HavePropertyMatcher, HavePropertyMatchResult}

trait SpanMatchers {

  def totalNanos(expectedValue: Long) =
    HavePropertyMatcher { (span: Span) =>
      HavePropertyMatchResult(
        span.totalNanos == expectedValue,
        "totalNanos",
        expectedValue,
        span.totalNanos
      )
    }

  def millisPart(expectedValue: Long) =
    HavePropertyMatcher { (span: Span) =>
      HavePropertyMatchResult(
        span.millisPart == expectedValue,
        "millisPart",
        expectedValue,
        span.millisPart
      )
    }

  def nanosPart(expectedValue: Long) =
    HavePropertyMatcher { (span: Span) =>
      HavePropertyMatchResult(
        span.nanosPart == expectedValue,
        "nanosPart",
        expectedValue,
        span.nanosPart
      )
    }

}
