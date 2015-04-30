package org.scalatest

import org.scalatest.SharedHelpers.SilentReporter
import java.util.concurrent.Executors

class TestConcurrentDistributor(poolSize: Int) extends tools.ConcurrentDistributor(Args(reporter = SilentReporter), Executors.newFixedThreadPool(poolSize)) {
  override def apply(suite: Suite, tracker: Tracker) {
    throw new UnsupportedOperationException("Please use apply with args.")
  }
}