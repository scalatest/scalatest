package org.scalatest.tools

import org.scalatest.{Args, RunningSuite, Status}
import org.scalatest.funspec.AnyFunSpec

class RunningSuitesSpec extends AnyFunSpec {

  var runningSuites: List[RunningSuite] = _

  override def run(testName: Option[String], args: Args): Status = {
    this.runningSuites = args.runningSuites
    super.run(testName, args)
  }

  it("Args.runningSuites contains the current running suite") {
    runningSuites.find(_.className == this.getClass.getName) match {
      case Some(thisSuite) =>
        if (thisSuite.isSingleton) {
          assert(thisSuite.lazyHandle() == this)
        } else {
          cancel("Running in a worker process")
        }
      case None =>
        fail("Expected Args.runningSuites to contain the current suite")
    }
  }

  it("Args.runningSuites contains other suites if not running under testOnly") {
    if (runningSuites.size == 1) {
      cancel(s"Running in testOnly. $runningSuites")
    } else {
      assert(runningSuites.size > 1)
      assert(runningSuites == runningSuites.distinct)
    }
  }

}
