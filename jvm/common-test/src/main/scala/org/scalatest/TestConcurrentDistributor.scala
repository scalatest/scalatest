package org.scalatest

import org.scalatest.SharedHelpers.SilentReporter
import java.util.concurrent.ExecutorService

class TestConcurrentDistributor(execService: ExecutorService) extends tools.ConcurrentDistributor(Args(reporter = SilentReporter), execService) {
  override def apply(suite: Suite, tracker: Tracker): Unit = {
    throw new UnsupportedOperationException("Please use apply with args.")
  }
}
