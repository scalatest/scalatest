package org.scalatest.tools

import org.scalatest._
import events._

class DashboardReporterSpec extends Spec with SharedHelpers {

  object `DashboardReporter ` {
    
    def `should work with error message that contains '<' and '>' symbol` {
      val tempDir = createTempDirectory()
      val ordinal = new Ordinal(123)
      val rep = new DashboardReporter(tempDir.getAbsolutePath, 0)
      rep(RunStarting(ordinal.next, 1, ConfigMap.empty))
      rep(SuiteStarting(ordinal.next, "TestSuite", "com.test.TestSuite", Some("com.test.TestSuite")))
      rep(TestStarting(ordinal.next, "TestSuite", "com.test.TestSuite", Some("com.test.TestSuite"), "test 1", "test 1"))
      rep(TestFailed(ordinal.next, "a test using <function1> failed", "TestSuite", "com.test.TestSuite", Some("com.test.TestSuite"),
                     "test 1", "test 1", collection.immutable.IndexedSeq.empty[RecordableEvent], Some(new RuntimeException("a <function1> caused the problem"))))
      rep(SuiteCompleted(ordinal.next, "TestSuite", "com.test.TestSuite", Some("com.test.TestSuite")))
      rep(RunCompleted(ordinal.next))
    }
    
  }
  
}