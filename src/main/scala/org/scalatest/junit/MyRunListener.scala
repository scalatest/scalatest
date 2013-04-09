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
package org.scalatest.junit;

import org.scalatest._
import org.scalatest.Suite
import org.junit.runner.JUnitCore
import org.junit.runner.notification.RunListener
import org.junit.runner.notification.Failure
import org.junit.runner.Description
import org.junit.runner.Result
import org.scalatest.events._
import java.util.Collections
import java.util.HashSet
import java.util.regex.Pattern
import Suite.getIndentedTextForTest
import org.scalatest.events.TopOfMethod
import exceptions._

  private[junit] class MyRunListener(report: Reporter,
                                     config: Map[String, Any],
                                     theTracker: Tracker, 
                                     status: ScalaTestStatefulStatus)
  extends RunListener {
    val failedTests = Collections.synchronizedSet(new HashSet[String])
    def getTopOfMethod(className: String, methodName: String) = Some(TopOfMethod(className, "public void " + className + "." + methodName + "()"))

    override def testFailure(failure: Failure) {
      failedTests.add(failure.getDescription.getDisplayName)
      val (testName, testClass, testClassName) =
        parseTestDescription(failure.getDescription)
      val throwableOrNull = failure.getException
      val throwable =
        if (throwableOrNull != null)
          Some(throwableOrNull)
        else
          None

      val message =
        if (throwableOrNull != null)
          throwableOrNull.toString
        else
          Resources("jUnitTestFailed")

      val formatter = getIndentedTextForTest(testName, 1, true)
      val payload = 
        throwable match {
          case optPayload: PayloadField => 
            optPayload.payload
          case _ => 
            None
        }
      report(TestFailed(theTracker.nextOrdinal(), message, testClassName, testClass, Some(testClass), testName, testName, Vector.empty, throwable, None, Some(formatter), Some(SeeStackDepthException), None, payload))
      // TODO: can I add a duration?
    }

    override def testFinished(description: Description) {
      if (!failedTests.contains(description.getDisplayName)) {
        val (testName, testClass, testClassName) =
          parseTestDescription(description)
        val formatter = getIndentedTextForTest(testName, 1, true)
        report(TestSucceeded(theTracker.nextOrdinal(), testClassName, testClass, Some(testClass), testName, testName, Vector.empty, None, Some(formatter), getTopOfMethod(testClass, testName)))
        // TODO: can I add a duration?
      }
      else
        status.setFailed()
    }

    override def testIgnored(description: Description) {
      val (testName, testClass, testClassName) =
        parseTestDescription(description)
      val formatter = getIndentedTextForTest(testName, 1, true)
      report(TestIgnored(theTracker.nextOrdinal(), testClassName, testClass, Some(testClass), testName, testName, Some(formatter), getTopOfMethod(testClass, testName)))
    }

    override def testRunFinished(result: Result) {
      // don't report these - they get reported by Runner
    }

    override def testRunStarted(description: Description) {
      // don't report these - they get reported by Runner
    }

    override def testStarted(description: Description) {
      val (testName, testClass, testClassName) =
        parseTestDescription(description)
      report(TestStarting(theTracker.nextOrdinal(), testClassName, testClass, Some(testClass), testName, testName, Some(MotionToSuppress), getTopOfMethod(testClass, testName)))
    }

    //
    // Parses test name and suite name from description.  Returns them
    // as a tuple (testname, test class (fully qualified), test class name).
    //
    // The test descriptions I've seen have had the form testname(testclass).
    // This may need to be modified if other formats are discovered.
    //
    val TEST_DESCRIPTION_PATTERN = Pattern.compile("""^(.*)\((.*)\)""")
    private def parseTestDescription(description: Description):
    (String, String, String) = {
      val matcher =
        TEST_DESCRIPTION_PATTERN.matcher(description.getDisplayName)

      if (!matcher.find())
        throw new RuntimeException("unexpected displayName [" +
                                   description.getDisplayName + "]")

      val testName = matcher.group(1)
      val testClass = matcher.group(2)
      val testClassName = testClass.replaceAll(".*\\.", "")

      (testName, testClass, testClassName)
    }
  }
