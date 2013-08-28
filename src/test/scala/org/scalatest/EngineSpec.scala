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

import SharedHelpers._
import events.InfoProvided
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.events.LineInFile
import org.scalatest.exceptions._
import org.scalatest.OutcomeOf.outcomeOf

class EngineSpec extends FlatSpec with ShouldMatchers {

  "EngineSpec.getTestNamePrefix" should "return empty string for Trunk" in {
    val engine = new Engine("concurrentFunSuiteBundleMod", "FunSuite")
    import engine._
    getTestNamePrefix(Trunk) should be ("")
  }

  it should "return empty string for direct children of Trunk" in {
    val engine = new Engine("concurrentFunSuiteBundleMod", "FunSuite")
    import engine._
    val child = DescriptionBranch(Trunk, "Catherine", Some("child prefix"), None)
    Trunk.subNodes ::= child
    getTestNamePrefix(child) should be ("Catherine child prefix")
  }

  it should "return the parent's description name for DescriptionBranch grandchildren of trunk" in {
    val engine = new Engine("concurrentFunSuiteBundleMod", "FunSuite")
    import engine._
    val child = DescriptionBranch(Trunk, "child", Some("child prefix"), None)
    Trunk.subNodes ::= child
    val grandchild = DescriptionBranch(child, "grandchild", None, None)
    child.subNodes ::= grandchild
    getTestNamePrefix(grandchild) should be ("child child prefix grandchild")
  }

  "EngineSpec.getTestName" should "return the prefix, a space, and the testText" in {
    val engine = new Engine("concurrentFunSuiteBundleMod", "FunSuite")
    import engine._
    val child = DescriptionBranch(Trunk, "child", Some("child prefix"), None)
    Trunk.subNodes ::= child
    val grandchild = DescriptionBranch(child, "grandchild", None, None)
    child.subNodes ::= grandchild
    getTestName("howdy there", grandchild) should be ("child child prefix grandchild howdy there")
  }
  "EngineSpec.getIndentationLevelForNode" should "return the indentation level for a test" in {
    val engine = new Engine("concurrentFunSuiteBundleMod", "FunSuite")
    import engine._
    val child = DescriptionBranch(Trunk, "child", Some("child prefix"), None)
    Trunk.subNodes ::= child
    val childTest = TestLeaf(Trunk, "child test", "child test", () => Succeeded, None)
    Trunk.subNodes ::= childTest
    val grandchild = DescriptionBranch(child, "grandchild", None, None)
    child.subNodes ::= grandchild
    val grandchildTest = TestLeaf(child, "grandchild test", "grandchild test", () => Succeeded, None)
    child.subNodes ::= grandchildTest
    val greatGrandchildTest = TestLeaf(grandchild, "great-grandchild test", "great-grandchild test", () => Succeeded, None)
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
    val engine = new Engine("concurrentFunSuiteBundleMod", "FunSuite")
    engine.registerNestedBranch("Given an empty list", None, {
      val list = ListBuffer[Int]() 
      engine.registerNestedBranch("when 1 is inserted", None, {
        list += 1 
        engine.registerTest("then the list has only 1 in it", () => {
          list should be (ListBuffer(1)) 
          list.clear()
          Succeeded
        }, "Anything", "Anything", "Anything", 1, 0, None, None, None)
        engine.registerTest("then the list length = 1", () => {
          outcomeOf { list.length should be (1) }
        }, "Anything", "Anything", "Anything", 1, 0, None, None, None)
      }, "Anything", "Anything", "Anything", 1, 0, None)
      engine.registerNestedBranch("when 2 is inserted", None, {
        list += 2
        engine.registerTest("then the list has only 2 in it", () => {
          outcomeOf { list should be (ListBuffer(2)) }
        }, "Anything", "Anything", "Anything", 1, 0, None, None, None)
      }, "Anything", "Anything", "Anything", 1, 0, None)
    }, "Anything", "Anything", "Anything", 1, 0, None)
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
}
