/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest

import org.scalatest.prop.Tables
// SKIP-SCALATESTJS,NATIVE-START
import org.scalatestplus.junit.JUnit3Suite
import org.scalatestplus.junit.JUnitSuite
import org.scalatestplus.testng.TestNGSuite
import org.scalatest.refspec.RefSpec
// SKIP-SCALATESTJS,NATIVE-END

trait MethodSuiteExamples extends Tables {
  type FixtureServices

  // SKIP-SCALATESTJS,NATIVE-START
  def spec: RefSpec with FixtureServices
  def junit3Suite: JUnit3Suite with FixtureServices
  def junitSuite: JUnitSuite with FixtureServices
  def testngSuite: TestNGSuite with FixtureServices
  // SKIP-SCALATESTJS,NATIVE-END
  
  def examples =
    Table[Suite with FixtureServices](
      // SKIP-SCALATESTJS,NATIVE-START
      "suite",
      spec,
      junit3Suite, 
      junitSuite,
      testngSuite
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY  "suite"
    )
}
