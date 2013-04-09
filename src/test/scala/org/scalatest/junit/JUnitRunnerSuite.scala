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

package org.scalatest.junit {

  import org.scalatest._

  // Put fixture suites in a subpackage, so they won't be discovered by
  // -m org.scalatest.junit when running the test target for this project.
  package helpers {

    import org.junit.runner.RunWith

    @RunWith(classOf[JUnitRunner])
    class EasySuite extends FunSuite {

      val runCount = 3
      val failedCount = 2
      val ignoreCount = 1

      test("JUnit ran this OK!") {
        assert(1 === 1)
      }

      test("JUnit ran this OK!, but it had a failure we hope") {
        assert(1 === 2)
      }

      test("bla bla bla") {
        assert(1 === 2)
      }

      ignore("I should be ignored") {
        assert(1 === 2)
      }
    }

    //
    // Blows up in beforeAll before any tests can be run.
    //
    @RunWith(classOf[JUnitRunner])
    class KerblooeySuite extends FunSuite with BeforeAndAfterAll {

      override def beforeAll() {
        throw new RuntimeException("kerblooey")
      }

      val runCount = 0
      val failedCount = 1
      val ignoreCount = 0

      test("JUnit ran this OK!") {
        assert(1 === 1)
      }

      test("JUnit ran this OK!, but it had a failure we hope") {
        assert(1 === 2)
      }

      ignore("I should be ignored") {
        assert(1 === 2)
      }
    }
  }

  import org.junit.runner.JUnitCore
  import org.junit.runner.Description
  import org.junit.runner.notification.Failure
  import org.junit.runner.notification.RunNotifier
  import org.scalatest.junit.helpers.EasySuite
  import org.scalatest.junit.helpers.KerblooeySuite

  class JUnitRunnerSuite extends FunSuite {

    test("That EasySuite gets run by JUnit given its RunWith annotation") {
      val result = JUnitCore.runClasses(classOf[EasySuite])
      val easySuite = new EasySuite
      assert(result.getRunCount === easySuite.runCount) // EasySuite has 3 tests (excluding the ignored one)
      assert(result.getFailureCount === easySuite.failedCount) // EasySuite has 2 tests that blow up
      assert(result.getIgnoreCount === easySuite.ignoreCount) // EasySuite has 1 ignored test
    }

    //
    // This is a regression test for a problem where a failure was
    // sometimes not reported in Jenkins when a beforeAll method threw
    // an exception.
    //
    // The fix was to catch and report the exception as a failure
    // from JUnitRunner.run, instead of allowing the exception to
    // propagate up.
    //
    test("a test failure is reported due to an exception thrown from " +
         "beforeAll when JUnitRunner.run is called directly")
    {
      val runNotifier =
        new RunNotifier {
          var methodInvocationCount = 0
          var passed: Option[Failure] = None
          override def fireTestFailure(failure: Failure) {
            methodInvocationCount += 1
            passed = Some(failure)
          }
        }

      (new JUnitRunner(classOf[KerblooeySuite])).run(runNotifier)

      assert(runNotifier.methodInvocationCount === 1)
      assert(runNotifier.passed.get.getDescription.getDisplayName ===
             "org.scalatest.junit.helpers.KerblooeySuite")
    }

    //
    // This test verifies that the fix tested above didn't break the
    // behavior seen when JUnit calls the JUnitRunner.
    //
    test("That a test failure is reported due to an exception thrown from " +
         "beforeAll when JUnitRunner is called from JUnit")
    {
      val result = JUnitCore.runClasses(classOf[KerblooeySuite])
      val kerblooeySuite = new KerblooeySuite
      assert(result.getRunCount === kerblooeySuite.runCount) 
      assert(result.getFailureCount === kerblooeySuite.failedCount) 
      assert(result.getIgnoreCount === kerblooeySuite.ignoreCount)
    }
  }
}
