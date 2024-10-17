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

import SharedHelpers._
import org.scalatest.OutcomeOf.outcomeOf
import org.scalatest.events.LineInFile
import scala.concurrent.Future
import events.InfoProvided
import events.Ordinal
// SKIP-SCALATESTJS,NATIVE-START
import concurrent.Eventually._
// SKIP-SCALATESTJS,NATIVE-END
import org.scalactic._
import org.scalatest.funsuite.AsyncFunSuite

class AsyncEngineSpec extends flatspec.AnyFlatSpec with matchers.should.Matchers {

  "AsyncEngine.getTestNamePrefix" should "return empty string for Trunk" in {
    val engine = new AsyncEngine("concurrentFunSuiteBundleMod", "FunSuite")
    import engine._
    getTestNamePrefix(Trunk) should be ("")
  }

  it should "return empty string for direct children of Trunk" in {
    val engine = new AsyncEngine("concurrentFunSuiteBundleMod", "FunSuite")
    import engine._
    val child = DescriptionBranch(Trunk, "Catherine", Some("child prefix"), None)
    Trunk.subNodes ::= child
    getTestNamePrefix(child) should be ("Catherine child prefix")
  }

  it should "return the parent's description name for DescriptionBranch grandchildren of trunk" in {
    val engine = new AsyncEngine("concurrentFunSuiteBundleMod", "FunSuite")
    import engine._
    val child = DescriptionBranch(Trunk, "child", Some("child prefix"), None)
    Trunk.subNodes ::= child
    val grandchild = DescriptionBranch(child, "grandchild", None, None)
    child.subNodes ::= grandchild
    getTestNamePrefix(grandchild) should be ("child child prefix grandchild")
  }

  "Engine.getTestName" should "return the prefix, a space, and the testText" in {
    val engine = new AsyncEngine("concurrentFunSuiteBundleMod", "FunSuite")
    import engine._
    val child = DescriptionBranch(Trunk, "child", Some("child prefix"), None)
    Trunk.subNodes ::= child
    val grandchild = DescriptionBranch(child, "grandchild", None, None)
    child.subNodes ::= grandchild
    getTestName("howdy there", grandchild) should be ("child child prefix grandchild howdy there")
  }
  "Engine.getIndentationLevelForNode" should "return the indentation level for a test" in {
    val engine = new AsyncEngine("concurrentFunSuiteBundleMod", "FunSuite")
    import engine._
    val child = DescriptionBranch(Trunk, "child", Some("child prefix"), None)
    Trunk.subNodes ::= child
    val childTest = TestLeaf(Trunk, "child test", "child test", () => PastAsyncTestHolder(Succeeded), None, Some(source.Position.here))
    Trunk.subNodes ::= childTest
    val grandchild = DescriptionBranch(child, "grandchild", None, None)
    child.subNodes ::= grandchild
    val grandchildTest = TestLeaf(child, "grandchild test", "grandchild test", () => PastAsyncTestHolder(Succeeded), None, Some(source.Position.here))
    child.subNodes ::= grandchildTest
    val greatGrandchildTest = TestLeaf(grandchild, "great-grandchild test", "great-grandchild test", () => PastAsyncTestHolder(Succeeded), None, Some(source.Position.here))
    grandchild.subNodes ::= greatGrandchildTest
    Trunk.indentationLevel should be (0)
    child.indentationLevel should be (0)
    childTest.indentationLevel should be (0)
    grandchild.indentationLevel should be (1)
    grandchildTest.indentationLevel should be (1)
    greatGrandchildTest.indentationLevel should be (2)
  }

  def pathEngine = {
    import scala.collection.mutable.ListBuffer
    val engine = new AsyncEngine("concurrentFunSuiteBundleMod", "FunSuite")
    engine.registerNestedBranch("Given an empty list", None, {
      val list = ListBuffer[Int]() 
      engine.registerNestedBranch("when 1 is inserted", None, {
        list += 1 
        engine.registerAsyncTest("then the list has only 1 in it", () => {
          list should be (ListBuffer(1)) 
          list.clear()
          PastAsyncTestHolder(Succeeded)
        }, "Anything", None, None, source.Position.here)
        engine.registerAsyncTest("then the list length = 1", () => {
          PastAsyncTestHolder(outcomeOf { list.length should be (1) })
        }, "Anything", None, None, source.Position.here)
      }, "Anything", None, source.Position.here)
      engine.registerNestedBranch("when 2 is inserted", None, {
        list += 2
        engine.registerAsyncTest("then the list has only 2 in it", () => {
          PastAsyncTestHolder(outcomeOf { list should be (ListBuffer(2)) })
        }, "Anything", None, None, source.Position.here)
      }, "Anything", None, source.Position.here)
    }, "Anything", None, source.Position.here)
    engine
  }
  
  "Engine.getPathForTest" should "throw IAE for not existing task" in {
    val engine = pathEngine
    intercept[IllegalArgumentException] { 
      engine.testPath("Invalid test name") 
    } 
  }
  
  "Engine.getPathForTest" should "return correct path for test" in {
    val engine = pathEngine
    engine.testPath("Given an empty list when 1 is inserted then the list has only 1 in it") should be (List(0, 0, 0))
    engine.testPath("Given an empty list when 1 is inserted then the list length = 1") should be (List(0, 0, 1))
    engine.testPath("Given an empty list when 2 is inserted then the list has only 2 in it") should be (List(0, 1, 0))
  }

  // SKIP-SCALATESTJS,NATIVE-START
  "AsyncEngine" should "abort a suite if an exception that should cause an abort is thrown in a test" in {
    val ex = new OutOfMemoryError("I meant to do that!")
    class MySpec extends AsyncFunSuite {
      test("should abort the suite") {
        Future.failed(ex)
      }
    }
    val s = new MySpec
    val myReporter = new EventRecordingReporter
    val status = s.run(None, Args(myReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
    myReporter.suiteCompletedEventsReceived should have size 0
    eventually { status.unreportedException shouldBe defined }
  // SKIP-SCALATESTJS,NATIVE-END
    
/*
    // THings like OutOfMemoryError in a test didn't cause a suite abort, it killed the thread all the
    // way back up. Suite aborts were caused by before or after code that died. Now we have a new problem
    // in async of what do we do when a test dies with OutOfMemoryError. Can't just propagate it back.
    // Unless there's someplace we can throw it, maybe have to report SuiteAborted. 
    class SuiteThatAborts extends AsyncFunSuite {
      // SKIP-SCALATESTJS,NATIVE-START
      implicit def executionContext = scala.concurrent.ExecutionContext.Implicits.global
      // SKIP-SCALATESTJS,NATIVE-END
      //SCALATESTJS,NATIVE-ONLY implicit def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow
      test("should abort this suite") {
        Future.failed(ex)
      }
    }

    class MyFunSuite extends FunSuite {
      override def nestedSuites = Vector(new SuiteThatAborts {})
    }

    val myFunSuite = new MyFunSuite
    val mySecondReporter = new SuiteDurationReporter
    myFunSuite.run(None, Args(mySecondReporter, Stopper.default, Filter(), ConfigMap.empty, None, new Tracker(new Ordinal(99))))
    eventually { assert(mySecondReporter.suiteAbortedWasFiredAndHadADuration) }
*/
  // SKIP-SCALATESTJS,NATIVE-START
  }
  // SKIP-SCALATESTJS,NATIVE-END

}
