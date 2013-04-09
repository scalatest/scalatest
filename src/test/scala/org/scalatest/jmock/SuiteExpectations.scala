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
package org.scalatest.jmock

import org.scalatest._
import scala.reflect.Manifest
import org.jmock.Expectations
import org.hamcrest.core.IsAnything
import org.scalatest.events._

trait SuiteExpectations {

  def expectSingleTestToPass(expectations: Expectations, reporter: Reporter) = expectNTestsToPass(expectations, 1, reporter)
  def expectSingleTestToFail(expectations: Expectations, reporter: Reporter) = expectNTestsToFail(expectations, 1, reporter)
  
  def expectNTestsToPass(expectations: Expectations, n: Int, reporter: Reporter) = {
    expectNTestsToRun(expectations, n, reporter) { 
      expectations.one(reporter).apply(expectations.`with`(new IsAnything[TestSucceeded]))
    }
  }
  
  def expectNTestsToFail(expectations: Expectations, n: Int, reporter: Reporter) = {
    expectNTestsToRun(expectations, n, reporter) { 
      expectations.one(reporter).apply(expectations.`with`(new IsAnything[TestFailed]))
    }
  }

  def expectNTestsToRun(expectations: Expectations, n: Int, reporter: Reporter)(f: => Unit) = {
    expectations.never(reporter).apply(expectations.`with`(new IsAnything[SuiteStarting]))
    for( i <- 1 to n ){
      expectations.one(reporter).apply(expectations.`with`(new IsAnything[TestStarting]))
      f
    }
    expectations.never(reporter).apply(expectations.`with`(new IsAnything[SuiteCompleted]))
  }
}

