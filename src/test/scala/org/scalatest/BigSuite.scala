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
class BigSuite(nestedSuiteCount: Option[Int]) extends Suite { thisSuite =>

  //def this() = this(None)
  
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

  def testNumber1() {
    val someFailures = System.getProperty("org.scalatest.BigSuite.someFailures", "")
    nestedSuiteCount match {
      case Some(0) if someFailures == "true" => assert(1 + 1 === 3)
      case _ => assert(1 + 1 === 2)
    }
  }

  def testNumber2() {
    assert(1 + 1 === 2)
  }

  def testNumber3() {
    assert(1 + 1 === 2)
  }

  def testNumber4() {
    assert(1 + 1 === 2)
  }

  def testNumber5() {
    assert(1 + 1 === 2)
  }

  def testNumber6() {
    assert(1 + 1 === 2)
  }

  def testNumber7() {
    assert(1 + 1 === 2)
  }

  def testNumber8() {
    assert(1 + 1 === 2)
  }

  def testNumber9() {
    assert(1 + 1 === 2)
  }

  def testNumber10() {
    assert(1 + 1 === 2)
  }

  def testNumber11() {
    assert(1 + 1 === 2)
  }

  def testNumber12() {
    assert(1 + 1 === 2)
  }

  def testNumber13() {
    assert(1 + 1 === 2)
  }

  def testNumber14() {
    assert(1 + 1 === 2)
  }

  def testNumber15() {
    assert(1 + 1 === 2)
  }

  def testNumber16() {
    assert(1 + 1 === 2)
  }

  def testNumber17() {
    assert(1 + 1 === 2)
  }

  def testNumber18() {
    assert(1 + 1 === 2)
  }

  def testNumber19() {
    assert(1 + 1 === 2)
  }

  def testNumber20() {
    assert(1 + 1 === 2)
  }

  def testNumber21() {
    assert(1 + 1 === 2)
  }

  def testNumber22() {
    assert(1 + 1 === 2)
  }

  def testNumber23() {
    assert(1 + 1 === 2)
  }

  def testNumber24() {
    assert(1 + 1 === 2)
  }

  def testNumber25() {
    assert(1 + 1 === 2)
  }

  def testNumber26() {
    assert(1 + 1 === 2)
  }

  def testNumber27() {
    assert(1 + 1 === 2)
  }

  def testNumber28() {
    assert(1 + 1 === 2)
  }

  def testNumber29() {
    assert(1 + 1 === 2)
  }

  def testNumber30() {
    assert(1 + 1 === 2)
  }

  def testNumber31() {
    assert(1 + 1 === 2)
  }

  def testNumber32() {
    assert(1 + 1 === 2)
  }

  def testNumber33() {
    assert(1 + 1 === 2)
  }

  def testNumber34() {
    assert(1 + 1 === 2)
  }

  def testNumber35() {
    assert(1 + 1 === 2)
  }

  def testNumber36() {
    assert(1 + 1 === 2)
  }

  def testNumber37() {
    assert(1 + 1 === 2)
  }

  def testNumber38() {
    assert(1 + 1 === 2)
  }

  def testNumber39() {
    assert(1 + 1 === 2)
  }

  def testNumber40() {
    assert(1 + 1 === 2)
  }

  def testNumber41() {
    assert(1 + 1 === 2)
  }

  def testNumber42() {
    assert(1 + 1 === 2)
  }

  def testNumber43() {
    assert(1 + 1 === 2)
  }

  def testNumber44() {
    assert(1 + 1 === 2)
  }

  def testNumber45() {
    assert(1 + 1 === 2)
  }

  def testNumber46() {
    assert(1 + 1 === 2)
  }

  def testNumber47() {
    assert(1 + 1 === 2)
  }

  def testNumber48() {
    assert(1 + 1 === 2)
  }

  def testNumber49() {
    assert(1 + 1 === 2)
  }

  def testNumber50() {
    assert(1 + 1 === 2)
  }

  def testNumber51() {
    assert(1 + 1 === 2)
  }

  def testNumber52() {
    assert(1 + 1 === 2)
  }

  def testNumber53() {
    assert(1 + 1 === 2)
  }

  def testNumber54() {
    assert(1 + 1 === 2)
  }

  def testNumber55() {
    assert(1 + 1 === 2)
  }

  def testNumber56() {
    assert(1 + 1 === 2)
  }

  def testNumber57() {
    assert(1 + 1 === 2)
  }

  def testNumber58() {
    assert(1 + 1 === 2)
  }

  def testNumber59() {
    assert(1 + 1 === 2)
  }

  def testNumber60() {
    assert(1 + 1 === 2)
  }

  def testNumber61() {
    assert(1 + 1 === 2)
  }

  def testNumber62() {
    assert(1 + 1 === 2)
  }

  def testNumber63() {
    assert(1 + 1 === 2)
  }

  def testNumber64() {
    assert(1 + 1 === 2)
  }

  def testNumber65() {
    assert(1 + 1 === 2)
  }

  def testNumber66() {
    assert(1 + 1 === 2)
  }

  def testNumber67() {
    assert(1 + 1 === 2)
  }

  def testNumber68() {
    assert(1 + 1 === 2)
  }

  def testNumber69() {
    assert(1 + 1 === 2)
  }

  def testNumber70() {
    assert(1 + 1 === 2)
  }

  def testNumber71() {
    assert(1 + 1 === 2)
  }

  def testNumber72() {
    assert(1 + 1 === 2)
  }

  def testNumber73() {
    assert(1 + 1 === 2)
  }

  def testNumber74() {
    assert(1 + 1 === 2)
  }

  def testNumber75() {
    assert(1 + 1 === 2)
  }

  def testNumber76() {
    assert(1 + 1 === 2)
  }

  def testNumber77() {
    assert(1 + 1 === 2)
  }

  def testNumber78() {
    assert(1 + 1 === 2)
  }

  def testNumber79() {
    assert(1 + 1 === 2)
  }

  def testNumber80() {
    assert(1 + 1 === 2)
  }

  def testNumber81() {
    assert(1 + 1 === 2)
  }

  def testNumber82() {
    assert(1 + 1 === 2)
  }

  def testNumber83() {
    assert(1 + 1 === 2)
  }

  def testNumber84() {
    assert(1 + 1 === 2)
  }

  def testNumber85() {
    assert(1 + 1 === 2)
  }

  def testNumber86() {
    assert(1 + 1 === 2)
  }

  def testNumber87() {
    assert(1 + 1 === 2)
  }

  def testNumber88() {
    assert(1 + 1 === 2)
  }

  def testNumber89() {
    assert(1 + 1 === 2)
  }

  def testNumber90() {
    assert(1 + 1 === 2)
  }

  def testNumber91() {
    assert(1 + 1 === 2)
  }

  def testNumber92() {
    assert(1 + 1 === 2)
  }

  def testNumber93() {
    assert(1 + 1 === 2)
  }

  def testNumber94() {
    assert(1 + 1 === 2)
  }

  def testNumber95() {
    assert(1 + 1 === 2)
  }

  def testNumber96() {
    assert(1 + 1 === 2)
  }

  def testNumber97() {
    assert(1 + 1 === 2)
  }

  def testNumber98() {
    assert(1 + 1 === 2)
  }

  def testNumber99() {
    assert(1 + 1 === 2)
  }

  def testNumber100() {
    assert(1 + 1 === 2)
  }
}
