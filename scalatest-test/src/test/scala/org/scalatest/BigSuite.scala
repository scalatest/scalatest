/*
 * Copyright 2001-2013 Artima, Inc.
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

import events._
import Suite.formatterForSuiteAborted
import Suite.formatterForSuiteCompleted
import Suite.formatterForSuiteStarting

/*
java -Dorg.scalatest.BigSuite.size=5 -Dorg.scalatest.SuiteCompletedStatusReporter.max=100 -classpath scalatest-1.0-CLICKDEMO.jar:/usr/artima/scala/lib/scala-library.jar org.scalatest.tools.Runner -c4 -p "scalatest-1.0-CLICKDEMO-tests.jar" -oNCXEHLO -r org.scalatest.SuiteCompletedStatusReporter -s org.scalatest.BigSuite -s org.scalatest.BigSuite -s org.scalatest.BigSuite -s org.scalatest.BigSuite -s org.scalatest.BigSuite

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
class BigSuite(nestedSuiteCount: Option[Int]) extends FunSpec { thisSuite =>

  override def nestedSuites: collection.immutable.IndexedSeq[Suite] = {

    def makeList(remaining: Int, soFar: List[Suite], nestedCount: Int): List[Suite] = {
      if (remaining == 0) soFar
      else makeList(remaining - 1, (new BigSuite(Some(nestedCount - 1)) :: soFar), nestedCount)
    }

    val nsList = nestedSuiteCount match {
      case None =>
        val sizeString = System.getProperty("org.scalatest.BigSuite.size", "0")
        val size =
          try {
            sizeString.toInt
          }
          catch {
            case e: NumberFormatException => 0
          }
        makeList(size, Nil, size)
      case Some(n) =>
        if (n == 0) List()
        else {
          makeList(n, Nil, n)
        }
    }

    Vector.empty ++ nsList
  }

  it("test number 1") {
    val someFailures = System.getProperty("org.scalatest.BigSuite.someFailures", "")
    nestedSuiteCount match {
      case Some(0) if someFailures == "true" => assert(1 + 1 === 3)
      case _ => assert(1 + 1 === 2)
    }
  }

  it("test number 2") {
    assert(1 + 1 === 2)
  }

  it("test number 3") {
    assert(1 + 1 === 2)
  }

  it("test number 4") {
    assert(1 + 1 === 2)
  }

  it("test number 5") {
    assert(1 + 1 === 2)
  }

  it("test number 6") {
    assert(1 + 1 === 2)
  }

  it("test number 7") {
    assert(1 + 1 === 2)
  }

  it("test number 8") {
    assert(1 + 1 === 2)
  }

  it("test number 9") {
    assert(1 + 1 === 2)
  }

  it("test number 10") {
    assert(1 + 1 === 2)
  }

  it("test number 11") {
    assert(1 + 1 === 2)
  }

  it("test number 12") {
    assert(1 + 1 === 2)
  }

  it("test number 13") {
    assert(1 + 1 === 2)
  }

  it("test number 14") {
    assert(1 + 1 === 2)
  }

  it("test number 15") {
    assert(1 + 1 === 2)
  }

  it("test number 16") {
    assert(1 + 1 === 2)
  }

  it("test number 17") {
    assert(1 + 1 === 2)
  }

  it("test number 18") {
    assert(1 + 1 === 2)
  }

  it("test number 19") {
    assert(1 + 1 === 2)
  }

  it("test number 20") {
    assert(1 + 1 === 2)
  }

  it("test number 21") {
    assert(1 + 1 === 2)
  }

  it("test number 22") {
    assert(1 + 1 === 2)
  }

  it("test number 23") {
    assert(1 + 1 === 2)
  }

  it("test number 24") {
    assert(1 + 1 === 2)
  }

  it("test number 25") {
    assert(1 + 1 === 2)
  }

  it("test number 26") {
    assert(1 + 1 === 2)
  }

  it("test number 27") {
    assert(1 + 1 === 2)
  }

  it("test number 28") {
    assert(1 + 1 === 2)
  }

  it("test number 29") {
    assert(1 + 1 === 2)
  }

  it("test number 30") {
    assert(1 + 1 === 2)
  }

  it("test number 31") {
    assert(1 + 1 === 2)
  }

  it("test number 32") {
    assert(1 + 1 === 2)
  }

  it("test number 33") {
    assert(1 + 1 === 2)
  }

  it("test number 34") {
    assert(1 + 1 === 2)
  }

  it("test number 35") {
    assert(1 + 1 === 2)
  }

  it("test number 36") {
    assert(1 + 1 === 2)
  }

  it("test number 37") {
    assert(1 + 1 === 2)
  }

  it("test number 38") {
    assert(1 + 1 === 2)
  }

  it("test number 39") {
    assert(1 + 1 === 2)
  }

  it("test number 40") {
    assert(1 + 1 === 2)
  }

  it("test number 41") {
    assert(1 + 1 === 2)
  }

  it("test number 42") {
    assert(1 + 1 === 2)
  }

  it("test number 43") {
    assert(1 + 1 === 2)
  }

  it("test number 44") {
    assert(1 + 1 === 2)
  }

  it("test number 45") {
    assert(1 + 1 === 2)
  }

  it("test number 46") {
    assert(1 + 1 === 2)
  }

  it("test number 47") {
    assert(1 + 1 === 2)
  }

  it("test number 48") {
    assert(1 + 1 === 2)
  }

  it("test number 49") {
    assert(1 + 1 === 2)
  }

  it("test number 50") {
    assert(1 + 1 === 2)
  }

  it("test number 51") {
    assert(1 + 1 === 2)
  }

  it("test number 52") {
    assert(1 + 1 === 2)
  }

  it("test number 53") {
    assert(1 + 1 === 2)
  }

  it("test number 54") {
    assert(1 + 1 === 2)
  }

  it("test number 55") {
    assert(1 + 1 === 2)
  }

  it("test number 56") {
    assert(1 + 1 === 2)
  }

  it("test number 57") {
    assert(1 + 1 === 2)
  }

  it("test number 58") {
    assert(1 + 1 === 2)
  }

  it("test number 59") {
    assert(1 + 1 === 2)
  }

  it("test number 60") {
    assert(1 + 1 === 2)
  }

  it("test number 61") {
    assert(1 + 1 === 2)
  }

  it("test number 62") {
    assert(1 + 1 === 2)
  }

  it("test number 63") {
    assert(1 + 1 === 2)
  }

  it("test number 64") {
    assert(1 + 1 === 2)
  }

  it("test number 65") {
    assert(1 + 1 === 2)
  }

  it("test number 66") {
    assert(1 + 1 === 2)
  }

  it("test number 67") {
    assert(1 + 1 === 2)
  }

  it("test number 68") {
    assert(1 + 1 === 2)
  }

  it("test number 69") {
    assert(1 + 1 === 2)
  }

  it("test number 70") {
    assert(1 + 1 === 2)
  }

  it("test number 71") {
    assert(1 + 1 === 2)
  }

  it("test number 72") {
    assert(1 + 1 === 2)
  }

  it("test number 73") {
    assert(1 + 1 === 2)
  }

  it("test number 74") {
    assert(1 + 1 === 2)
  }

  it("test number 75") {
    assert(1 + 1 === 2)
  }

  it("test number 76") {
    assert(1 + 1 === 2)
  }

  it("test number 77") {
    assert(1 + 1 === 2)
  }

  it("test number 78") {
    assert(1 + 1 === 2)
  }

  it("test number 79") {
    assert(1 + 1 === 2)
  }

  it("test number 80") {
    assert(1 + 1 === 2)
  }

  it("test number 81") {
    assert(1 + 1 === 2)
  }

  it("test number 82") {
    assert(1 + 1 === 2)
  }

  it("test number 83") {
    assert(1 + 1 === 2)
  }

  it("test number 84") {
    assert(1 + 1 === 2)
  }

  it("test number 85") {
    assert(1 + 1 === 2)
  }

  it("test number 86") {
    assert(1 + 1 === 2)
  }

  it("test number 87") {
    assert(1 + 1 === 2)
  }

  it("test number 88") {
    assert(1 + 1 === 2)
  }

  it("test number 89") {
    assert(1 + 1 === 2)
  }

  it("test number 90") {
    assert(1 + 1 === 2)
  }

  it("test number 91") {
    assert(1 + 1 === 2)
  }

  it("test number 92") {
    assert(1 + 1 === 2)
  }

  it("test number 93") {
    assert(1 + 1 === 2)
  }

  it("test number 94") {
    assert(1 + 1 === 2)
  }

  it("test number 95") {
    assert(1 + 1 === 2)
  }

  it("test number 96") {
    assert(1 + 1 === 2)
  }

  it("test number 97") {
    assert(1 + 1 === 2)
  }

  it("test number 98") {
    assert(1 + 1 === 2)
  }

  it("test number 99") {
    assert(1 + 1 === 2)
  }

  it("test number 100") {
    assert(1 + 1 === 2)
  }
}
