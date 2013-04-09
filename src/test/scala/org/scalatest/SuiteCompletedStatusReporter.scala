/*
 * Copyright 2001-2009 Artima, Inc.
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

import org.scalatest._
import events.{SuiteCompleted, Event}

/*
java -Dorg.scalatest.BigSuite.size=5 -Dorg.scalatest.SuiteCompletedStatusReporter.max=100 -classpath scalatest-1.0-SNAPSHOT.jar:/usr/artima/scala/lib/scala-library.jar org.scalatest.tools.Runner -c4 -p "scalatest-1.0-SNAPSHOT-tests.jar" -oNCXEHLO -r org.scalatest.SuiteCompletedStatusReporter -s org.scalatest.BigSuite -s org.scalatest.BigSuite -s org.scalatest.BigSuite -s org.scalatest.BigSuite -s org.scalatest.BigSuite

BigSuite.size determines how many suites will be in each BigSuite tree. I haven't taken time to figure out the function, but it looks like this:
size => number of suites in the tree
1 => 2
2 => 5
3 => 16
4 => 65
5 => 326
6 => 1957
7 => 13700

Each -s org.scalatest.BigSuite will create one BigSuite instance using the size specified by the property.

By saying -r org.scalatest.SuiteCompletedStatusReporter, you get a custom reporter that prints out a duration note to the standard output
for every <configurable number> of SuiteCompleted events it receives. It defaults to 10, and can be set via the
-Dorg.scalatest.SuiteCompletedStatusReporter.max=100 setting.

So the knobs we can turn are:

-cN N is the number of threads in the thread pool
-Dorg.scalatest.BigSuite.size=M, M determines the number of suites in the tree via some mysterious function
-s org.scalatest.BigSuite..., repeating this gets you more instances of these trees sized by M
-Dorg.scalatest.SuiteCompletedStatusReporter.max=X, where X is the number of SuiteCompleted events between duration notes
*/
class SuiteCompletedStatusReporter extends Reporter {
  var count = 0
  val max =
    try {
      System.getProperty("org.scalatest.SuiteCompletedStatusReporter.max", "10").toInt
    }
    catch {
      case _: NumberFormatException => 10
    }
  var startTime = System.currentTimeMillis
  override def apply(event: Event) {
    event match {
      case e: SuiteCompleted =>
        count += 1
        if (count > max) {
          val duration = System.currentTimeMillis - startTime
          println(max.toString + " more SuiteCompleted events received in: " + duration + " ms")
          count = 0
          startTime = System.currentTimeMillis
        }
      case _ =>
    }
  }
}
