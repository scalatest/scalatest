package org.scalatest

import matchers.ShouldMatchers
import prop.TableDrivenPropertyChecks

class ArgsSpec extends WordSpec with TableDrivenPropertyChecks with ShouldMatchers with SharedHelpers with SeveredStackTraces {
  "The Args constructor" should {
    "throw NullPointerExcepion when passed a null" in {

      val rep = SilentReporter
      val stp = Stopper.default
      val flt = Filter()
      val cnf = ConfigMap.empty
      val dst = None
      val trk = new Tracker
      val sty = Set.empty[String]
      
      val invalidCombos =
        Table(
          ("reporter", "stopper", "filter", "configMap", "distributor", "tracker", "chosenStyles"),
          (      null,       stp,      flt,         cnf,           dst,       trk,            sty),
          (       rep,      null,      flt,         cnf,           dst,       trk,            sty),
          (       rep,       stp,     null,         cnf,           dst,       trk,            sty),
          (       rep,       stp,      flt,        null,           dst,       trk,            sty),
          (       rep,       stp,      flt,         cnf,           null,      trk,            sty),
          (       rep,       stp,      flt,         cnf,           dst,      null,            sty),
          (       rep,       stp,      flt,         cnf,           dst,       trk,           null)
        )

      forAll (invalidCombos) { (reporter, stopper, filter, configMap, distributor, tracker, chosenStyles) =>
        evaluating {
          Args(reporter, stopper, filter, configMap, distributor, tracker, chosenStyles)
        } should produce [NullPointerException]
      }
    }
  }

  "The deprecated run method" should {
    "call the new run method" in {
      class MySuite extends Suite {
        var newRunGotCalled = false
        override def run(testName: Option[String], args: Args): Status = {
          newRunGotCalled = true
          SucceededStatus
        }
      }
      val s = new MySuite
      s.run(None, SilentReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker)
      assert(s.newRunGotCalled)
    }
  }
}
