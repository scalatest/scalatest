package org.scalatest

import _root_.org.scalatest.tools.SuiteRunner

class TestConcurrentDistributor(poolSize: Int) extends Distributor {
  def apply(suite: Suite, tracker: Tracker) {
    throw new UnsupportedOperationException("Please use apply with args.")
  }

  def apply(suite: Suite, args: Args): Status = {
    if (suite == null)
      throw new NullPointerException("suite is null")
    if (args == null)
      throw new NullPointerException("args is null")
    val status = new ScalaTestStatefulStatus
    val suiteRunner = new SuiteRunner(suite, args, status)
    suiteRunner.run()
    status
  }

  def waitUntilDone() = Unit
}