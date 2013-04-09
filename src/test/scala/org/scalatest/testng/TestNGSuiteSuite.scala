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
package org.scalatest.testng {

  import org.scalatest._
  import org.scalatest.jmock._
  import testng.testpackage._
  import org.jmock.Mockery
  import org.jmock.Expectations
  import org.hamcrest.core.IsAnything
  import org.scalatest.events._
  import org.scalatest.mock.JMockCycle
  import org.scalatest.mock.JMockCycleFixture
  import org.scalatest.fixture

  class TestNGSuiteSuite extends fixture.FunSuite with JMockCycleFixture with SuiteExpectations {

    test("Reporter should be notified when test passes") { cycle => import cycle._

      val reporter = mock[Reporter]

      expecting { e =>
        expectSingleTestToPass(e, reporter)
      }

      whenExecuting {
        val status = new ScalaTestStatefulStatus
        (new SuccessTestNGSuite()).runTestNG(reporter, new Tracker, status)
        status.setCompleted()
      }
    }

    test("Reporter should be notified when test fails") { cycle => import cycle._

      val reporter = mock[Reporter]

      expecting { e =>
        expectSingleTestToFail(e, reporter)
      }

      whenExecuting {
        val status = new ScalaTestStatefulStatus
        (new FailureTestNGSuite()).runTestNG(reporter, new Tracker, status)
        status.setCompleted()
      }
    }

    test("If a test fails due to an exception, Report should have the exception") { () =>
      
      val testReporter = new TestReporter

      // when
      val status = new ScalaTestStatefulStatus
      (new FailureTestNGSuite()).runTestNG(testReporter, new Tracker, status)
      status.setCompleted

      // then
      testReporter.lastEvent match {
        case Some(TestFailed(_, _, _, _, _, _, _, _, throwable, _, _, _, _, _, _, _)) =>
          assert(throwable.get.getMessage === "fail")
        case _ => fail()
      }
    }

    test("Report should be generated for each invocation") { cycle => import cycle._
      
      val reporter = mock[Reporter]

      // expect reporter gets 10 passing reports because invocationCount = 10
      expecting { e =>
        expectNTestsToPass(e, 10, reporter)
      }

      // when runnning the suite with method that has invocationCount = 10")
      whenExecuting {
        val status = new ScalaTestStatefulStatus
        (new TestNGSuiteWithInvocationCount()).runTestNG(reporter, new Tracker, status)
        status.setCompleted()
      }
    }

    test("Reporter should be notified when test is skipped") { cycle => import cycle._

      val reporter = mock[Reporter]

      // expect a single test should fail, followed by a single test being skipped
      expecting { e => import e._
        never(reporter).apply(`with`(new IsAnything[SuiteStarting]))
        one(reporter).apply(`with`(new IsAnything[TestStarting]))
        one(reporter).apply(`with`(new IsAnything[TestFailed]))
        one(reporter).apply(`with`(new IsAnything[TestIgnored]))
        never(reporter).apply(`with`(new IsAnything[SuiteCompleted]))
      }

      // when runnning the suite with a test that should fail and a test that should be skipped
      whenExecuting {
        val status = new ScalaTestStatefulStatus
        (new SuiteWithSkippedTest()).runTestNG(reporter, new Tracker, status)
        status.setCompleted()
      }
    }
    
    test("Only the correct method should be run when specifying a single method to run") { cycle => import cycle._
      
      val reporter = mock[Reporter]

      expecting { e =>
        expectSingleTestToPass(e, reporter)
      }

      whenExecuting {
        val status = new ScalaTestStatefulStatus
        (new SuiteWithTwoTests()).runTestNG("testThatPasses", reporter, new Tracker, status)
        status.setCompleted()
      }
    }

    test("Report for failing tests should include rerunner") { () =>
      
      val testReporter = new TestReporter

      // when - run the failing suite
      val status = new ScalaTestStatefulStatus
      new FailureTestNGSuite().runTestNG(testReporter, new Tracker, status)
      status.setCompleted()

      // then get rerunnable from the event 
      testReporter.lastEvent match {
        case Some(TestFailed(_, _, _, _, _, _, _, _, _, _, _, _, rerunnable, _, _, _)) =>
          assert(rerunnable.isDefined)
        case _ => fail()
      }
    }

    test("Report for passing tests should include rerunner") { () =>
      
      val testReporter = new TestReporter

      // when - run the passing suite
      val status = new ScalaTestStatefulStatus
      new SuccessTestNGSuite().runTestNG(testReporter, new Tracker, status)
      status.setCompleted()

      // then get rerunner from report 
      val rerunner = testReporter.lastEvent.get.asInstanceOf[TestSucceeded].rerunner
      assert(rerunner != None)
      assert(rerunner.get === classOf[SuccessTestNGSuite].getName)
    }
    
    
    test("infoProvided should be available for BeforeMethod/Class/Suite annotations") { () =>
      // this needs to be written after i figure out the mock integration
    }     
    
    test("infoProvided should be available for AfterMethod/Class/Suite annotations") { () =>
      // this needs to be written after i figure out the mock integration
    }     
  }

  package testpackage {
    
    import org.testng.annotations._
    
    class FailureTestNGSuite extends TestNGSuite {
      @Test def testThatFails() { throw new Exception("fail") }
    }
    
    class SuccessTestNGSuite extends TestNGSuite {
      @Test def testThatPasses() {}
    }
    
    class TestNGSuiteWithInvocationCount extends TestNGSuite {
      @Test(invocationCount = 10) def testThatPassesTenTimes() {}
    }
    
    class SuiteWithSkippedTest extends TestNGSuite {
      @Test(groups = Array("run")) def dependeeThatFails() { throw new Exception("fail") }
      @Test(dependsOnGroups = Array("run")) def depender() {}
    } 

    class SuiteWithTwoTests extends TestNGSuite {
      @Test def testThatPasses() {}
      @Test def anotherTestThatPasses() {}
    }      
    
    class SuiteWithBeforeAndAfterAnnotations extends TestNGSuite {
    }
  }
}


