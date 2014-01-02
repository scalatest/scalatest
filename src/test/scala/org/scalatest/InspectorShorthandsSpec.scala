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

import org.scalatest.prop.TableDrivenPropertyChecks
import collection.GenTraversable
import collection.GenSeq
import collection.GenMap
import collection.mutable.LinkedHashMap
import scala.annotation.tailrec

import matchers.HavePropertyMatcher
import matchers.HavePropertyMatchResult
import matchers.BePropertyMatcher
import matchers.BePropertyMatchResult
import org.scalautils.Equality
import SharedHelpers._
import FailureMessages.decorateToStringValue

class InspectorShorthandsSpec extends Spec with Matchers with TableDrivenPropertyChecks {

  def examples =
    Table[Set[Int] => GenTraversable[Int]](
      ("Fun"), 
      ((set: Set[Int]) => set), 
      ((set: Set[Int]) => set.toList), 
      ((set: Set[Int]) => set.toSeq), 
      ((set: Set[Int]) => set.toArray), 
      ((set: Set[Int]) => set.toIndexedSeq), 
      ((set: Set[Int]) => Vector(set.toSeq: _*)),
      ((set: Set[Int]) => set.par), 
      ((set: Set[Int]) => set.toList.par), 
      ((set: Set[Int]) => set.toSeq.par), 
      ((set: Set[Int]) => set.toIndexedSeq.par), 
      ((set: Set[Int]) => collection.mutable.Set(set.toSeq: _*)), 
      ((set: Set[Int]) => { val l = new collection.mutable.ListBuffer() ++= set; l }), 
      ((set: Set[Int]) => collection.mutable.Seq(set.toSeq: _*)), 
      ((set: Set[Int]) => collection.mutable.IndexedSeq(set.toSeq: _*)), 
      ((set: Set[Int]) => collection.mutable.Set(set.toSeq: _*).par), 
      ((set: Set[Int]) => { val l = new collection.mutable.ListBuffer() ++= set; l }.par), 
      ((set: Set[Int]) => collection.mutable.Seq(set.toSeq: _*).par), 
      ((set: Set[Int]) => collection.mutable.IndexedSeq(set.toSeq: _*).par) 
    )
    
  def nullableExamples = 
    Table[Set[String] => GenTraversable[String]](
      ("Fun"), 
      ((set: Set[String]) => set), 
      ((set: Set[String]) => set.toList), 
      ((set: Set[String]) => set.toSeq), 
      ((set: Set[String]) => set.toArray[String]), 
      ((set: Set[String]) => set.toIndexedSeq), 
      ((set: Set[String]) => Vector(set.toSeq: _*)),
      ((set: Set[String]) => set.par), 
      ((set: Set[String]) => set.toList.par), 
      ((set: Set[String]) => set.toSeq.par), 
      ((set: Set[String]) => set.toIndexedSeq.par), 
      ((set: Set[String]) => collection.mutable.Set(set.toSeq: _*)), 
      ((set: Set[String]) => { val l = new collection.mutable.ListBuffer() ++= set; l }), 
      ((set: Set[String]) => collection.mutable.Seq(set.toSeq: _*)), 
      ((set: Set[String]) => collection.mutable.IndexedSeq(set.toSeq: _*)), 
      ((set: Set[String]) => collection.mutable.Set(set.toSeq: _*).par), 
      ((set: Set[String]) => { val l = new collection.mutable.ListBuffer() ++= set; l }.par), 
      ((set: Set[String]) => collection.mutable.Seq(set.toSeq: _*).par), 
      ((set: Set[String]) => collection.mutable.IndexedSeq(set.toSeq: _*).par) 
    )
    
  def traversableExamples = 
    Table[Set[Set[String]] => GenTraversable[GenTraversable[String]]](
      ("Fun"), 
      ((set: Set[Set[String]]) => set), 
      ((set: Set[Set[String]]) => set.toList), 
      ((set: Set[Set[String]]) => set.toSeq), 
      ((set: Set[Set[String]]) => set.toArray[GenTraversable[String]]), 
      ((set: Set[Set[String]]) => set.toIndexedSeq), 
      ((set: Set[Set[String]]) => Vector(set.toSeq: _*)),
      ((set: Set[Set[String]]) => set.par), 
      ((set: Set[Set[String]]) => set.toList.par), 
      ((set: Set[Set[String]]) => set.toSeq.par), 
      ((set: Set[Set[String]]) => set.toIndexedSeq.par), 
      ((set: Set[Set[String]]) => collection.mutable.Set(set.toSeq: _*)), 
      ((set: Set[Set[String]]) => { val l = new collection.mutable.ListBuffer() ++= set; l }), 
      ((set: Set[Set[String]]) => collection.mutable.Seq(set.toSeq: _*)), 
      ((set: Set[Set[String]]) => collection.mutable.IndexedSeq.empty ++ set), 
      ((set: Set[Set[String]]) => collection.mutable.Set(set.toSeq: _*).par), 
      ((set: Set[Set[String]]) => { val l = new collection.mutable.ListBuffer() ++= set; l }.par), 
      ((set: Set[Set[String]]) => collection.mutable.Seq(set.toSeq: _*).par), 
      ((set: Set[Set[String]]) => collection.mutable.IndexedSeq(set.toSeq: _*).par) 
    )
    
  def seqExamples = 
    Table[Set[GenSeq[String]] => GenTraversable[GenSeq[String]]](
      ("Fun"), 
      ((set: Set[GenSeq[String]]) => set), 
      ((set: Set[GenSeq[String]]) => set.toList), 
      ((set: Set[GenSeq[String]]) => set.toSeq), 
      ((set: Set[GenSeq[String]]) => set.toArray[GenSeq[String]]), 
      ((set: Set[GenSeq[String]]) => set.toIndexedSeq), 
      ((set: Set[GenSeq[String]]) => Vector(set.toSeq: _*)),
      ((set: Set[GenSeq[String]]) => set.par), 
      ((set: Set[GenSeq[String]]) => set.toList.par), 
      ((set: Set[GenSeq[String]]) => set.toSeq.par), 
      ((set: Set[GenSeq[String]]) => set.toIndexedSeq.par), 
      ((set: Set[GenSeq[String]]) => collection.mutable.Set(set.toSeq: _*)), 
      ((set: Set[GenSeq[String]]) => { val l = new collection.mutable.ListBuffer() ++= set; l }), 
      ((set: Set[GenSeq[String]]) => collection.mutable.Seq(set.toSeq: _*)), 
      ((set: Set[GenSeq[String]]) => collection.mutable.IndexedSeq.empty ++ set), 
      ((set: Set[GenSeq[String]]) => collection.mutable.Set(set.toSeq: _*).par), 
      ((set: Set[GenSeq[String]]) => { val l = new collection.mutable.ListBuffer() ++= set; l }.par), 
      ((set: Set[GenSeq[String]]) => collection.mutable.Seq(set.toSeq: _*).par), 
      ((set: Set[GenSeq[String]]) => collection.mutable.IndexedSeq(set.toSeq: _*).par) 
    )
    
  def mapExamples = 
    Table[Set[GenMap[String, String]] => GenTraversable[GenMap[String, String]]](
      ("Fun"), 
      ((set: Set[GenMap[String, String]]) => set), 
      ((set: Set[GenMap[String, String]]) => set.toList), 
      ((set: Set[GenMap[String, String]]) => set.toSeq), 
      ((set: Set[GenMap[String, String]]) => set.toArray[GenMap[String, String]]), 
      ((set: Set[GenMap[String, String]]) => set.toIndexedSeq), 
      ((set: Set[GenMap[String, String]]) => Vector(set.toSeq: _*)),
      ((set: Set[GenMap[String, String]]) => set.par), 
      ((set: Set[GenMap[String, String]]) => set.toList.par), 
      ((set: Set[GenMap[String, String]]) => set.toSeq.par), 
      ((set: Set[GenMap[String, String]]) => set.toIndexedSeq.par), 
      ((set: Set[GenMap[String, String]]) => collection.mutable.Set(set.toSeq: _*)), 
      ((set: Set[GenMap[String, String]]) => { val l = new collection.mutable.ListBuffer() ++= set; l }), 
      ((set: Set[GenMap[String, String]]) => collection.mutable.Seq(set.toSeq: _*)), 
      ((set: Set[GenMap[String, String]]) => collection.mutable.IndexedSeq.empty ++ set), 
      ((set: Set[GenMap[String, String]]) => collection.mutable.Set(set.toSeq: _*).par), 
      ((set: Set[GenMap[String, String]]) => { val l = new collection.mutable.ListBuffer() ++= set; l }.par), 
      ((set: Set[GenMap[String, String]]) => collection.mutable.Seq(set.toSeq: _*).par), 
      ((set: Set[GenMap[String, String]]) => collection.mutable.IndexedSeq(set.toSeq: _*).par) 
    )
    
  object `all ` {
    
    def `should pass when all elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        all(col) should be < 4 
      }
    }
  
    def `should throw TestFailedException with correct stack depth and message when at least one element failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should not equal 2 
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index " + getIndex(col, 2) + ", 2 equaled 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in " + decorateToStringValue(col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 10))
            tfe.message should be (Some("2 equaled 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    
    def `should throw TestFailedException with correct stack depth and message when more than one element failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be < 2 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[Int](col, _ >= 2)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not less than 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was not less than 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'equal' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should equal (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, _ == 2)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " did not equal 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " did not equal 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }

    def `should throw TestFailedException with correct stack depth and message when 'shouldEqual' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) shouldEqual 2 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, _ == 2)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " did not equal 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " did not equal 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }

    def `should use Equality from 'shouldEqual'` {
      val xs = List(1, 1, 1)
      all (xs) shouldEqual 1 
      implicit val e = new Equality[Int] {
        def areEqual(a: Int, b: Any): Boolean = a != b
      }
      intercept[exceptions.TestFailedException] {
        all (xs) should equal (1) 
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'shouldEqual tolerance' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 4))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) shouldEqual 2 +- 1
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, i => i >= 1 && i <= 3)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " did not equal 2 plus or minus 1 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " did not equal 2 plus or minus 1"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }

    def `should throw TestFailedException with correct stack depth and message when 'shouldEqual null' failed` {
      val col: Set[String] = Set(null, null, "hi")
      val e2 = intercept[exceptions.TestFailedException] {
        all (col) shouldEqual null 
      }
      e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
      e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
      val firstViolation = getFirstNot[String](col, _ == null)
      e2.message should be (Some("'all' inspection failed, because: \n" +
                                  "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" did not equal null (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                  "in " + decorateToStringValue(col)))
      e2.getCause match {
        case tfe: exceptions.TestFailedException =>
          tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
          tfe.message should be (Some("\"" + firstViolation + "\" did not equal null"))
          tfe.getCause should be (null)
        case other => fail("Expected cause to be TestFailedException, but got: " + other)
      }
    }

    def `should use Equality from 'should equal'` {
      val xs = List(1, 1, 1)
      all (xs) should equal (1) 
      implicit val e = new Equality[Int] {
        def areEqual(a: Int, b: Any): Boolean = a != b
      }
      intercept[exceptions.TestFailedException] {
        all (xs) should equal (1) 
      }
    }
    
    def `should use Equality from 'should not equal'` {
      val xs = List(1, 1, 1)
      all (xs) should not equal (2) 
      implicit val e = new Equality[Int] {
        def areEqual(a: Int, b: Any): Boolean = a != b
      }
      intercept[exceptions.TestFailedException] {
        all (xs) should not equal (2) 
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not equal' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not equal (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, _ != 2)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " equaled 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " equaled 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, _ == 2)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not equal to 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was not equal to 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[Int](col, _ == 2)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was equal to 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was equal to 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be less than comparison' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be < 2 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, _ < 2)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not less than 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was not less than 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be less than comparison' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be < (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[Int](col, _ < 2)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was less than 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was less than 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be less than or equal comparison' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be <= 2 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, _ <= 2)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not less than or equal to 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was not less than or equal to 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be less than or equal comparison' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be <= (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[Int](col, _ <= 2)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was less than or equal to 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was less than or equal to 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be greater than comparison' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be > 2 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, _ > 2)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not greater than 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was not greater than 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be greater than comparison' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be > (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[Int](col, _ > 2)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was greater than 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was greater than 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be greater than or equal comparison' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be >= 2 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, _ >= 2)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not greater than or equal to 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was not greater than or equal to 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be greater than or equal comparison' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be >= (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[Int](col, _ >= 2)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was greater than or equal to 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was greater than or equal to 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }

    def `should throw TestFailedException with correct stack depth and message when 'be triple equal' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be === 2 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[Int](col, _ == 2)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not equal to 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was not equal to 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }

    def `should throw TestFailedException with correct stack depth and message when 'be not triple equal' failed` {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be === (2) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[Int](col, _ == 2)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was equal to 2 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was equal to 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be null' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("1", "2", "3"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be (null)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = col.head
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not null (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not null"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be null' failed` {
      forAll(nullableExamples) { colFun => 
        val col = 
          try {
            Some(colFun(Set("1", null, "3")))
          }
          catch {
            case iae: IllegalArgumentException =>
              // Some collection cannot contains null value, e.g. mutable.Set
              None
          }
        
        col match {
          case Some(col) => 
            val e2 = intercept[exceptions.TestFailedException] {
              all(col) should not be (null)
            }
            e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
            val firstViolation = getFirst[String](col, _ == null)
            e2.message should be (Some("'all' inspection failed, because: \n" +
                                       "  at index " + getIndex(col, firstViolation) + ", The reference was null (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                       "in " + decorateToStringValue(col)))
            e2.getCause match {
              case tfe: exceptions.TestFailedException =>
                tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
                tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
                tfe.message should be (Some("The reference was null"))
                tfe.getCause should be (null)
              case other => fail("Expected cause to be TestFailedException, but got: " + other)
            }
            
          case None => // Do nothing when the collection cannot contains null value.
        }
      }
    }
    
    def `should allow be symbol to work with arbitrary objects` {
      case class Person(name: String, happy: Boolean)
      all (List(Person("Fred", true), Person("Sally", true)) ) should be ('happy)
    }

    def `should throw TestFailedException with correct stack depth and message when 'be symbol' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be ('empty) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'shouldBe symbol' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all (col) shouldBe 'empty 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be symbol' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("peace 1", "", "peace 2"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be ('empty) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    class EmptyBePropertyMatcher extends BePropertyMatcher[String] {
      def apply(left: String) = BePropertyMatchResult(left.isEmpty, "empty")
    }
    val empty = new EmptyBePropertyMatcher()
    
    def `should throw TestFailedException with correct stack depth and message when 'be property' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be (empty)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be property' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be (empty)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be a symbol' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be a 'empty 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not a empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not a empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'shouldBe a symbol' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) shouldBe a ('empty) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not a empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not a empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be a symbol' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be a ('empty) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was a empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was a empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be a property' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be a empty
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not a empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not a empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be a property' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be a (empty)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was a empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was a empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be an symbol' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be an 'empty 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not an empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not an empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'shouldBe an symbol' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) shouldBe an ('empty) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not an empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not an empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be an symbol' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be an ('empty) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was an empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was an empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be an property' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be an empty
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not an empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not an empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be an property' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be an (empty)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was an empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was an empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'be theSameInstanceAs' failed` {
      val theInstance = "2"
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("1", "2", "3"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be theSameInstanceAs theInstance
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _ eq theInstance)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was not the same instance as \"" + theInstance + "\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was not the same instance as \"" + theInstance + "\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not be theSameInstanceAs' failed` {
      val theInstance = "2"
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("1", theInstance, "3"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be theSameInstanceAs (theInstance)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _ eq theInstance)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" was the same instance as \"" + theInstance + "\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" was the same instance as \"" + theInstance + "\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    class StringLengthMatcher(expectedValue: Int) extends HavePropertyMatcher[String, Int] {
      def apply(value: String) = {
        new HavePropertyMatchResult(value.length == expectedValue, "length", expectedValue, value.length)
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'have property' failed` {
      def length(expectedValue: Int) = new StringLengthMatcher(expectedValue)
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should have (length(0))
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", The length property had value " + firstViolation.length + ", instead of its expected value 0, on object \"" + firstViolation + "\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("The length property had value " + firstViolation.length + ", instead of its expected value 0, on object \"" + firstViolation + "\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not have property' failed` {
      def length(expectedValue: Int) = new StringLengthMatcher(expectedValue)
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not have length(5)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", The length property had its expected value 5, on object \"" + firstViolation + "\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("The length property had its expected value 5, on object \"" + firstViolation + "\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'have length' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should have length (0)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + FailureMessages("hadLengthInsteadOfExpectedLength", firstViolation, 5, 0) + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(FailureMessages("hadLengthInsteadOfExpectedLength", firstViolation, 5, 0)))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not have length' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not have length (0)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" had length 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" had length 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'have size' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should have size (0)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + FailureMessages("hadSizeInsteadOfExpectedSize", firstViolation, 5, 0) + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(FailureMessages("hadSizeInsteadOfExpectedSize", firstViolation, 5, 0)))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'not have size' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("", "boom!", ""))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not have size (0)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" had size 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" had size 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string startWith' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello A!", "hi B", "hello C"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should startWith ("hello")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.startsWith("hello"))
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" did not start with substring \"hello\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" did not start with substring \"hello\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string not startWith' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello A!", "hi B", "hello C"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not startWith ("hello")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.startsWith("hello"))
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" started with substring \"hello\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" started with substring \"hello\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string endWith' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello folks", "hi folks", "hai girls"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should endWith ("folks")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.endsWith("folks"))
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" did not end with substring \"folks\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" did not end with substring \"folks\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string not endWith' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello folks", "hi folks", "hai girls"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not endWith ("folks")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.endsWith("folks"))
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" ended with substring \"folks\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" ended with substring \"folks\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string include' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello folks!", "hi folks!", "hai girls!"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should include ("folks")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.indexOf("folks") >= 0)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" did not include substring \"folks\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" did not include substring \"folks\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string not include' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello folks!", "hi folks!", "hai girls!"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not include ("folks")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.indexOf("folks") >= 0)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" included substring \"folks\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" included substring \"folks\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string startWith regex' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello A!", "hi B", "hello C"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should startWith regex "hel*o"
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.startsWith("hello"))
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" did not start with a substring that matched the regular expression hel*o (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" did not start with a substring that matched the regular expression hel*o"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string not startWith regex' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello A!", "hi B", "hello C"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not startWith regex ("hel*o")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.startsWith("hello"))
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" started with a substring that matched the regular expression hel*o (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" started with a substring that matched the regular expression hel*o"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string endWith regex' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello folks!", "hi folks!", "hai girls!"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should endWith regex "folks!"
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.endsWith("folks!"))
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" did not end with a substring that matched the regular expression folks! (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" did not end with a substring that matched the regular expression folks!"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string not endWith regex' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello folks!", "hi folks!", "hai girls!"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not endWith regex ("folks!")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.endsWith("folks!"))
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" ended with a substring that matched the regular expression folks! (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" ended with a substring that matched the regular expression folks!"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string include regex' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello folks!", "hi folks!", "hai girls!"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should include regex "folks"
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.indexOf("folks") >= 0)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" did not include substring that matched regex folks (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" did not include substring that matched regex folks"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string not include regex' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("hello folks!", "hi folks!", "hai girls!"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not include regex ("folks")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.indexOf("folks") >= 0)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + "\" included substring that matched regex folks (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + "\" included substring that matched regex folks"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string fullyMatch regex' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("1.23", "-5", "8a"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should fullyMatch regex """(-)?(\d+)(\.\d*)?"""
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[String](col, _.matches("""(-)?(\d+)(\.\d*)?"""))
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + """" did not fully match the regular expression (-)?(\d+)(\.\d*)? (InspectorShorthandsSpec.scala:""" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + """" did not fully match the regular expression (-)?(\d+)(\.\d*)?"""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'string not fullyMatch regex' failed` {
      forAll(nullableExamples) { colFun => 
        val col = colFun(Set("1.23", "-5", "8a"))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not fullyMatch regex ("""(-)?(\d+)(\.\d*)?""")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[String](col, _.matches("""(-)?(\d+)(\.\d*)?"""))
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", \"" + firstViolation + """" fully matched the regular expression (-)?(\d+)(\.\d*)? (InspectorShorthandsSpec.scala:""" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"" + firstViolation + """" fully matched the regular expression (-)?(\d+)(\.\d*)?"""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'traversable be symbol' failed` {
      forAll(traversableExamples) { colFun => 
        val col = colFun(Set(Set(), Set("boom!"), Set()))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should be ('empty) 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[GenTraversable[String]](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " was not empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(decorateToStringValue(firstViolation) + " was not empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'traversable not be symbol' failed` {
      forAll(traversableExamples) { colFun => 
        val col = colFun(Set(Set(), Set("boom!"), Set()))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not be 'empty 
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[GenTraversable[String]](col, _.isEmpty)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was empty (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " was empty"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'seq have length' failed` {
      forAll(seqExamples) { colFun => 
        val col = colFun(Set(Seq(), Seq("boom!"), Seq()))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should have length 0
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[GenSeq[String]](col, _.length == 0)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + FailureMessages("hadLengthInsteadOfExpectedLength", firstViolation, 1, 0) + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(FailureMessages("hadLengthInsteadOfExpectedLength", firstViolation, 1, 0)))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'seq not have length' failed` {
      forAll(seqExamples) { colFun => 
        val col = colFun(Set(Seq(), Seq("boom!"), Seq()))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not have length (0)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[GenSeq[String]](col, _.length == 0)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " had length 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " had length 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'traversable have size' failed` {
      forAll(traversableExamples) { colFun => 
        val col = colFun(Set(Set(), Set("boom!"), Set()))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should have size 0
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[GenTraversable[String]](col, _.size == 0)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + FailureMessages("hadSizeInsteadOfExpectedSize", firstViolation, 1, 0) + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(FailureMessages("hadSizeInsteadOfExpectedSize", firstViolation, 1, 0)))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'traversable not have size' failed` {
      forAll(traversableExamples) { colFun => 
        val col = colFun(Set(Set(), Set("boom!"), Set()))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not have size (0)
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[GenTraversable[String]](col, _.size == 0)
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " had size 0 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(firstViolation + " had size 0"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'traversable contain' failed` {
      forAll(traversableExamples) { colFun => 
        val col = colFun(Set(Set("1", "2", "3"), Set("4", "5", "6"), Set("2", "6", "8")))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should contain ("2")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[GenTraversable[String]](col, _.exists(_ == "2"))
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain element \"2\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain element \"2\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'traversable not contain' failed` {
      forAll(traversableExamples) { colFun => 
        val col = colFun(Set(Set("1", "2", "3"), Set("4", "5", "6"), Set("2", "6", "8")))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not contain "2"
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[GenTraversable[String]](col, _.exists(_ == "2"))
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained element \"2\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained element \"2\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'map contain key' failed` {
      forAll(mapExamples) { colFun => 
        val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three"), Map("4" -> "four", "5" -> "five", "6" -> "six"), Map("2" -> "two", "6" -> "six", "8" -> "eight")))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should contain key "2"
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[GenMap[String, String]](col, _.exists(_._1 == "2"))
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain key \"2\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain key \"2\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'map not contain key' failed` {
      forAll(mapExamples) { colFun => 
        val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three"), Map("4" -> "four", "5" -> "five", "6" -> "six"), Map("2" -> "two", "6" -> "six", "8" -> "eight")))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not contain key ("2")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[GenMap[String, String]](col, _.exists(_._1 == "2"))
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained key \"2\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained key \"2\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'map contain value' failed` {
      forAll(mapExamples) { colFun => 
        val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three"), Map("4" -> "four", "5" -> "five", "6" -> "six"), Map("2" -> "two", "6" -> "six", "8" -> "eight")))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should contain value "two"
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirstNot[GenMap[String, String]](col, _.exists(_._2 == "two"))
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain value \"two\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain value \"two\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 'map not contain value' failed` {
      forAll(mapExamples) { colFun => 
        val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three"), Map("4" -> "four", "5" -> "five", "6" -> "six"), Map("2" -> "two", "6" -> "six", "8" -> "eight")))
        val e2 = intercept[exceptions.TestFailedException] {
          all(col) should not contain value ("two")
        }
        e2.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolation = getFirst[GenMap[String, String]](col, _.exists(_._2 == "two"))
        e2.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained value \"two\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained value \"two\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }

    private def javaCol(valueSet: GenTraversable[String]): java.util.Collection[String] = {
      val javaCol = new java.util.ArrayList[String]()
      for (value <- valueSet)
        javaCol.add(value)
      javaCol
    }
    
/*
    private def javaMap(valueMap: GenMap[String, String]): java.util.Map[String, String] = {
      val javaMap = new java.util.LinkedHashMap[String, String]()
      for ((key, value) <- valueMap)
        javaMap.put(key, value)
      javaMap
    }
*/
    
    object `when work with theSameElementsAs contain matcher` {
      
      def `should work correctly with all(traversable) should contain succeeded` {
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "3"), Set("2", "3", "1"), Set("3", "2", "1")))
          all(col) should contain theSameElementsAs Set("1", "2", "3")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(traversable) should contain failed` {
        val right = Set("1", "2", "8")
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "8"), Set("2", "3", "1"), Set("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain theSameElementsAs right
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenTraversable[String]](col, e => e.size == 3 && e.exists(_ == "1") && e.exists(_ == "2") && e.exists(_ == "8") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain the same elements as " + decorateToStringValue(right) + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain the same elements as " + decorateToStringValue(right)))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(array) should contain succeeded` {
        all(List(Array("1", "2", "3"), Array("2", "3", "1"), Array("3", "2", "1"))) should contain theSameElementsAs Set("1", "2", "3")
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(array) should contain failed` {
        val right = Set("1", "2", "8")
        val col = List(Array("1", "2", "8"), Array("2", "3", "1"), Array("3", "8", "1"))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should contain theSameElementsAs right
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolationArray = getFirstNot[Array[String]](col, e => e.size == 3 && e.contains("1") && e.contains("2") && e.contains("8") )
        val firstViolation = decorateToStringValue(firstViolationArray)
        e.message should be (Some("'all' inspection failed, because: \n" +
                                  "  at index " + getIndex(col, firstViolationArray) + ", " + firstViolation + " did not contain the same elements as " + decorateToStringValue(right) + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                  "in " + decorateToStringValue(col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 12))
            tfe.message should be (Some(firstViolation + " did not contain the same elements as " + decorateToStringValue(right)))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    
      def `should work correctly with all(seq) should contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "3"), Seq("2", "3", "1"), Seq("3", "2", "1")))
          all(col) should contain theSameElementsAs List("1", "2", "3")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(seq) should contain failed` {
        val right = Set("1", "2", "8")
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "8"), Seq("2", "3", "1"), Seq("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain theSameElementsAs right
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenTraversable[String]](col, e => e.size == 3 && e.exists(_ == "1") && e.exists(_ == "2") && e.exists(_ == "8") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain the same elements as " + decorateToStringValue(right) + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain the same elements as " + decorateToStringValue(right)))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(map) should contain succeeded` {
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three"), Map("2" -> "two", "3" -> "three", "1" -> "one"), Map("3" -> "three", "2" -> "two", "1" -> "one")))
          all(col) should contain theSameElementsAs List("1" -> "one", "2" -> "two", "3" -> "three")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(map) should contain failed` {
        val right = List("1" -> "one", "2" -> "two", "8" -> "eight")
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "8" -> "eight"), Map("2" -> "two", "3" -> "three", "1" -> "one"), Map("3" -> "three", "8" -> "eight", "1" -> "one")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain theSameElementsAs right
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenMap[String, String]](col, map => map.size == 3 && map.exists(e => e._1 == "1" && e._2 == "one") && map.exists(e => e._1 == "2" && e._2 == "two") && map.exists(e => e._1 == "8" && e._2 == "eight") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain the same elements as " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain the same elements as " + right.toString))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(traversable) should not contain succeeded` {
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "3"), Set("2", "3", "1"), Set("3", "2", "1")))
          all(col) should not contain theSameElementsAs (Set("1", "2", "8"))
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(traversable) should not contain failed` {
        val right = Set("1", "2", "8")
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "8"), Set("2", "3", "1"), Set("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain theSameElementsAs (right)
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenTraversable[String]](col, e => e.size == 3 && e.exists(_ == "1") && e.exists(_ == "2") && e.exists(_ == "8") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained the same elements as " + decorateToStringValue(right) + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained the same elements as " + decorateToStringValue(right)))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(array) should not contain succeeded` {
        all(List(Array("1", "2", "3"), Array("2", "3", "1"), Array("3", "2", "1"))) should not contain theSameElementsAs (Set("1", "2", "8"))
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(array) should not contain failed` {
        val right = Set("1", "2", "8")
        val col = List(Array("1", "2", "8"), Array("2", "3", "1"), Array("3", "8", "1"))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should not contain theSameElementsAs (right)
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolationArray = getFirst[Array[String]](col, e => e.size == 3 && e.contains("1") && e.contains("2") && e.contains("8") )
        val firstViolation = decorateToStringValue(firstViolationArray)
        e.message should be (Some("'all' inspection failed, because: \n" +
                                  "  at index " + getIndex(col, firstViolationArray) + ", " + firstViolation + " contained the same elements as " + decorateToStringValue(right) + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                  "in " + decorateToStringValue(col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 12))
            tfe.message should be (Some(firstViolation + " contained the same elements as " + decorateToStringValue(right)))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    
      def `should work correctly with all(seq) should not contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "3"), Seq("2", "3", "1"), Seq("3", "2", "1")))
          all(col) should not contain theSameElementsAs (List("1", "2", "8"))
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(seq) should not contain failed` {
        val right = Set("1", "2", "8")
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "8"), Seq("2", "3", "1"), Seq("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain theSameElementsAs (right)
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenSeq[String]](col, e => e.size == 3 && e.exists(_ == "1") && e.exists(_ == "2") && e.exists(_ == "8") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained the same elements as " + decorateToStringValue(right) + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained the same elements as " + decorateToStringValue(right)))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(map) should not contain succeeded` {
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three"), Map("2" -> "two", "3" -> "three", "1" -> "one"), Map("3" -> "three", "2" -> "two", "1" -> "one")))
          all(col) should not contain theSameElementsAs (Map("1" -> "one", "2" -> "two", "8" -> "eight"))
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(map) should not contain failed` {
        val right = Map("1" -> "one", "2" -> "two", "8" -> "eight")
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "8" -> "eight"), Map("2" -> "two", "3" -> "three", "1" -> "one"), Map("3" -> "three", "8" -> "eight", "1" -> "one")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain theSameElementsAs (right)
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenMap[String, String]](col, map => map.size == 3 && map.exists(e => e._1 == "1" && e._2 == "one") && map.exists(e => e._1 == "2" && e._2 == "two") && map.exists(e => e._1 == "8" && e._2 == "eight") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained the same elements as " + decorateToStringValue(right) + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained the same elements as " + decorateToStringValue(right)))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    }
    
    object `when work with theSameElementsInOrderAs contain matcher` {
      
      def `should work correctly with all(traversable) should contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(List("1", "2", "3")))
          all(col) should contain theSameElementsInOrderAs Set("1", "2", "3")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(traversable) should contain failed` {
        val right = Set("1", "2", "8")
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(List("1", "2", "8"), List("1", "6", "8")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain theSameElementsInOrderAs right
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenTraversable[String]](col, e => e.size == 3 && e.toList(0) == "1" && e.toList(1) == "2" && e.toList(2) == "8" )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right) + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right)))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(array) should contain succeeded` {
        all(List(Array("1", "2", "3"))) should contain theSameElementsInOrderAs Set("1", "2", "3")
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(array) should contain failed` {
        val right = Set("1", "2", "8")
        val col = List(Array("1", "2", "8"), Array("2", "8", "1"), Array("8", "2", "1"))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should contain theSameElementsInOrderAs right
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolationArray = getFirstNot[Array[String]](col, e => e.size == 3 && e(0) == "1" && e(1) == "2" && e(2) == "8" )
        val firstViolation = decorateToStringValue(firstViolationArray)
        e.message should be (Some("'all' inspection failed, because: \n" +
                                  "  at index " + getIndex(col, firstViolationArray) + ", " + firstViolation + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right) + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                  "in " + decorateToStringValue(col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 12))
            tfe.message should be (Some(firstViolation + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right)))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    
      def `should work correctly with all(seq) should contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "3")))
          all(col) should contain theSameElementsInOrderAs List("1", "2", "3")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(seq) should contain failed` {
        val right = Set("1", "2", "8")
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "8"), Seq("2", "3", "1"), Seq("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain theSameElementsInOrderAs right
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenSeq[String]](col, e => e.size == 3 && e(0) == "1" && e(1) == "2" && e(2) == "8" )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right) + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain the same elements in the same (iterated) order as " + decorateToStringValue(right)))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }

      def `should work correctly with all(see) should not contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(List("1", "2", "3")))
          all(col) should not contain theSameElementsInOrderAs (Set("1", "2", "8"))
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(traversable) should not contain failed` {
        val right = Set("1", "2", "8")
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(List("1", "2", "8"), List("2", "3", "1"), List("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain theSameElementsInOrderAs (right)
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenTraversable[String]](col, e => e.size == 3 && e.toList(0) == "1" && e.toList(1) == "2" && e.toList(2) == "8" )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right) + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right)))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(array) should not contain succeeded` {
        all(List(Array("1", "2", "3"), Array("2", "3", "1"), Array("3", "2", "1"))) should not contain theSameElementsInOrderAs (Set("1", "2", "8"))
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(array) should not contain failed` {
        val right = Set("1", "2", "8")
        val col = List(Array("1", "2", "8"), Array("2", "3", "1"), Array("3", "8", "1"))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should not contain theSameElementsInOrderAs (right)
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolationArray = getFirst[Array[String]](col, e => e.size == 3 && e(0) == "1" && e(1) == "2" && e(2) == "8" )
        val firstViolation = decorateToStringValue(firstViolationArray)
        e.message should be (Some("'all' inspection failed, because: \n" +
                                  "  at index " + getIndex(col, firstViolationArray) + ", " + firstViolation + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right) + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                  "in " + decorateToStringValue(col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 12))
            tfe.message should be (Some(firstViolation + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right)))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    
      def `should work correctly with all(seq) should not contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "3"), Seq("2", "3", "1"), Seq("3", "2", "1")))
          all(col) should not contain theSameElementsInOrderAs (List("1", "2", "8"))
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(seq) should not contain failed` {
        val right = Set("1", "2", "8")
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "8"), Seq("2", "3", "1"), Seq("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain theSameElementsInOrderAs (right)
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenSeq[String]](col, e => e.size == 3 && e(0) == "1" && e(1) == "2" && e(2) == "8" )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right) + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained the same elements in the same (iterated) order as " + decorateToStringValue(right)))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    }
    
    object `when work with allOf contain matcher` {
      
      def `should work correctly with all(traversable) should contain succeeded` {
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "3"), Set("3", "2", "1", "8")))
          all(col) should contain allOf ("1", "2", "3")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(traversable) should contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "8"), Set("1", "6", "8")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain allOf ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenTraversable[String]](col, e => e.size == 3 && e.exists(_ == "1") && e.exists(_ == "2") && e.exists(_ == "8") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain all of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain all of " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(array) should contain succeeded` {
        all(List(Array("1", "2", "3"), Array("3", "2", "1", "8"))) should contain allOf ("1", "2", "3")
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(array) should contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        val col = List(Array("1", "2", "8"), Array("2", "8", "1"), Array("8", "6", "1"))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should contain allOf ("1", "2", "8")
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolationArray = getFirstNot[Array[String]](col, e => e.size == 3 && e.exists(_ == "1") && e.exists(_ == "2") && e.exists(_ == "8") )
        val firstViolation = decorateToStringValue(firstViolationArray)
        e.message should be (Some("'all' inspection failed, because: \n" +
                                  "  at index " + getIndex(col, firstViolationArray) + ", " + firstViolation + " did not contain all of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                  "in " + decorateToStringValue(col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 12))
            tfe.message should be (Some(firstViolation + " did not contain all of " + right))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    
      def `should work correctly with all(seq) should contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "3"), Seq("3", "2", "1", "8")))
          all(col) should contain allOf ("1", "2", "3")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(seq) should contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "8"), Seq("2", "3", "1"), Seq("6", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain allOf ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenSeq[String]](col, e => e.size == 3 && e.exists(_ == "1") && e.exists(_ == "2") && e.exists(_ == "8") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain all of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain all of " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(map) should contain succeeded` {
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three")))
          all(col) should contain allOf ("1" -> "one", "2" -> "two", "3" -> "three")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(map) should contain failed` {
        val right = "(" + Array("1" -> "one", "2" -> "two", "8" -> "eight").mkString(", ") + ")"
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "8" -> "eight"), Map("2" -> "two", "3" -> "three", "1" -> "one"), Map("3" -> "three", "8" -> "eight", "1" -> "one")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain allOf ("1" -> "one", "2" -> "two", "8" -> "eight")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenMap[String, String]](col, map => map.size == 3 && map.exists(e => e._1 == "1" && e._2 == "one") && map.exists(e => e._1 == "2" && e._2 == "two") && map.exists(e => e._1 == "8" && e._2 == "eight") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain all of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain all of " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }

      def `should work correctly with all(traversable) should not contain succeeded` {
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "3")))
          all(col) should not contain allOf ("1", "2", "8")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(traversable) should not contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "8"), Set("2", "3", "1"), Set("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain allOf ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenTraversable[String]](col, e => e.size == 3 && e.exists(_ == "1") && e.exists(_ == "2") && e.exists(_ == "8") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained all of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained all of " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(array) should not contain succeeded` {
        all(List(Array("1", "2", "3"), Array("2", "3", "1"), Array("3", "2", "1"))) should not contain allOf ("1", "2", "8")
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(array) should not contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        val col = List(Array("1", "2", "8"), Array("2", "3", "1"), Array("3", "8", "1"))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should not contain allOf ("1", "2", "8")
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolationArray = getFirst[Array[String]](col, e => e.size == 3 && e.exists(_ == "1") && e.exists(_ == "2") && e.exists(_ == "8") )
        val firstViolation = decorateToStringValue(firstViolationArray)
        e.message should be (Some("'all' inspection failed, because: \n" +
                                  "  at index " + getIndex(col, firstViolationArray) + ", " + firstViolation + " contained all of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                  "in " + decorateToStringValue(col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 12))
            tfe.message should be (Some(firstViolation + " contained all of " + right))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    
      def `should work correctly with all(seq) should not contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "3"), Seq("2", "3", "1"), Seq("3", "2", "1")))
          all(col) should not contain allOf ("1", "2", "8")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(seq) should not contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "8"), Seq("2", "3", "1"), Seq("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain allOf ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenSeq[String]](col, e => e.size == 3 && e.exists(_ == "1") && e.exists(_ == "2") && e.exists(_ == "8") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained all of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained all of " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(map) should not contain succeeded` {
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three"), Map("2" -> "two", "3" -> "three", "1" -> "one"), Map("3" -> "three", "2" -> "two", "1" -> "one")))
          all(col) should not contain allOf ("1" -> "one", "2" -> "two", "8" -> "eight")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(map) should not contain failed` {
        val right = Array("1" -> "one", "2" -> "two", "8" -> "eight")
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "8" -> "eight"), Map("2" -> "two", "3" -> "three", "1" -> "one"), Map("3" -> "three", "8" -> "eight", "1" -> "one")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain allOf ("1" -> "one", "2" -> "two", "8" -> "eight")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenMap[String, String]](col, map => map.size == 3 && map.toList(0)._1 == "1" && map.toList(0)._2 == "one" && map.toList(1)._1 == "2" && map.toList(1)._2 == "two" && map.toList(2)._1 == "8" && map.toList(2)._2 == "eight" )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained all of (" + right.mkString(", ") + ") (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained all of (" + right.mkString(", ") + ")"))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    }
    
    object `when work with inOrder contain matcher` {
      
      def `should work correctly with all(traversable) should contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(List("1", "2", "3"), List("1", "2", "3", "8")))
          all(col) should contain inOrder ("1", "2", "3")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(traversable) should contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(List("1", "2", "8"), List("1", "6", "8")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain inOrder ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenTraversable[String]](col, e => e.size == 3 && e.toList(0) == "1" && e.toList(1) == "2" && e.toList(2) == "8" )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain all of " + right + " in order (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain all of " + right + " in order"))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(array) should contain succeeded` {
        all(List(Array("1", "2", "3"), Array("1", "2", "3", "8"))) should contain inOrder ("1", "2", "3")
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(array) should contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        val col = List(Array("1", "2", "8"), Array("2", "8", "1"), Array("8", "6", "1"))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should contain inOrder ("1", "2", "8")
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolationArray = getFirstNot[Array[String]](col, e => e.size == 3 && e(0) == "1" && e(1) == "2" && e(2) == "8" )
        val firstViolation = decorateToStringValue(firstViolationArray)
        e.message should be (Some("'all' inspection failed, because: \n" +
                                  "  at index " + getIndex(col, firstViolationArray) + ", " + firstViolation + " did not contain all of " + right + " in order (InspectorShorthandsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                  "in " + decorateToStringValue(col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 12))
            tfe.message should be (Some(firstViolation + " did not contain all of " + right + " in order"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    
      def `should work correctly with all(seq) should contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "3"), Seq("1", "2", "3", "8")))
          all(col) should contain inOrder ("1", "2", "3")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(seq) should contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "8"), Seq("2", "3", "1"), Seq("6", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain inOrder ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenSeq[String]](col, e => e.size == 3 && e(0) == "1" && e(1) == "2" && e(2) == "8" )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain all of " + right + " in order (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain all of " + right + " in order"))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
      
      def `should work correctly with all(traversable) should not contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(List("1", "2", "3")))
          all(col) should not contain inOrder ("1", "2", "8")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(traversable) should not contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(List("1", "2", "8"), List("2", "8", "1"), List("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain inOrder ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenTraversable[String]](col, e => e.size == 3 && e.toList(0) == "1" && e.toList(1) == "2" && e.toList(2) == "8" )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained all of " + right + " in order (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained all of " + right + " in order"))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(array) should not contain succeeded` {
        all(List(Array("1", "2", "3"), Array("2", "3", "1"), Array("3", "2", "1"))) should not contain inOrder ("1", "2", "8")
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(array) should not contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        val col = List(Array("1", "2", "8"), Array("2", "3", "1"), Array("3", "8", "1"))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should not contain inOrder ("1", "2", "8")
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolationArray = getFirst[Array[String]](col, e => e.size == 3 && e(0) == "1" && e(1) == "2" && e(2) == "8" )
        val firstViolation = decorateToStringValue(firstViolationArray)
        e.message should be (Some("'all' inspection failed, because: \n" +
                                  "  at index " + getIndex(col, firstViolationArray) + ", " + firstViolation + " contained all of " + right + " in order (InspectorShorthandsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                  "in " + decorateToStringValue(col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 12))
            tfe.message should be (Some(firstViolation + " contained all of " + right + " in order"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    
      def `should work correctly with all(seq) should not contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "3"), Seq("2", "3", "1"), Seq("3", "2", "1")))
          all(col) should not contain inOrder ("1", "2", "8")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(seq) should not contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "8"), Seq("2", "3", "1"), Seq("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain inOrder ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenSeq[String]](col, e => e.size == 3 && e(0) == "1" && e(1) == "2" && e(2) == "8" )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained all of " + right + " in order (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained all of " + right + " in order"))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    }
    
    object `when work with atLeastOneOf contain matcher` {
      
      def `should work correctly with all(traversable) should contain succeeded` {
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "3"), Set("1", "2", "3", "8")))
          all(col) should contain atLeastOneOf ("1", "2", "3")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(traversable) should contain failed` {
        val right = "(\"3\", \"5\", \"7\")"
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "8"), Set("1", "6", "8")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain atLeastOneOf ("3", "5", "7")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenTraversable[String]](col, e => e.exists(_ == "3") || e.exists(_ == "5") || e.exists(_ == "7") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain at least one of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain at least one of " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(array) should contain succeeded` {
        all(List(Array("1", "2", "3"), Array("1", "2", "3", "8"))) should contain atLeastOneOf ("1", "2", "3")
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(array) should contain failed` {
        val right = "(\"3\", \"5\", \"9\")"
        val col = List(Array("1", "2", "8"), Array("2", "8", "1"), Array("8", "6", "1"))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should contain atLeastOneOf ("3", "5", "9")
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolationArray = getFirstNot[Array[String]](col, e => e.exists(_ == "3") || e.exists(_ == "5") || e.exists(_ == "9") )
        val firstViolation = decorateToStringValue(firstViolationArray)
        e.message should be (Some("'all' inspection failed, because: \n" +
                                  "  at index " + getIndex(col, firstViolationArray) + ", " + firstViolation + " did not contain at least one of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                  "in " + decorateToStringValue(col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 12))
            tfe.message should be (Some(firstViolation + " did not contain at least one of " + right))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    
      def `should work correctly with all(seq) should contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "3"), Seq("1", "2", "3", "8")))
          all(col) should contain atLeastOneOf ("1", "2", "3")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(seq) should contain failed` {
        val right = "(\"3\", \"5\", \"9\")"
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "8"), Seq("2", "3", "1"), Seq("6", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain atLeastOneOf ("3", "5", "9")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenSeq[String]](col, e => e.exists(_ == "3") || e.exists(_ == "5") || e.exists(_ == "9") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain at least one of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain at least one of " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(map) should contain succeeded` {
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three")))
          all(col) should contain atLeastOneOf ("1" -> "one", "6" -> "six", "8" -> "eight")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(map) should contain failed` {
        val right = "(" + Array("3" -> "three", "5" -> "five", "7" -> "seven").mkString(", ") + ")"
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "8" -> "eight"), Map("2" -> "two", "3" -> "three", "1" -> "one"), Map("3" -> "three", "8" -> "eight", "1" -> "one")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain atLeastOneOf ("3" -> "three", "5" -> "five", "7" -> "seven")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenMap[String, String]](col, map => map.exists(e => e._1 == "3" && e._2 == "three") || map.exists(e => e._1 == "5" && e._2 == "five") || map.exists(e => e._1 == "7" && e._2 == "seven") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain at least one of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain at least one of " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }

      def `should work correctly with all(traversable) should not contain succeeded` {
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "3")))
          all(col) should not contain atLeastOneOf ("6", "7", "8")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(traversable) should not contain failed` {
        val right = "(\"6\", \"7\", \"8\")"
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "8"), Set("2", "8", "1"), Set("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain atLeastOneOf ("6", "7", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenTraversable[String]](col, e => e.exists(_ == "6") || e.exists(_ == "7") || e.exists(_ == "8") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained at least one of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained at least one of " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(array) should not contain succeeded` {
        all(List(Array("1", "2", "3"), Array("2", "3", "1"), Array("3", "2", "1"))) should not contain atLeastOneOf ("6", "7", "8")
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(array) should not contain failed` {
        val right = "(\"6\", \"7\", \"8\")"
        val col = List(Array("1", "2", "8"), Array("2", "3", "1"), Array("3", "8", "1"))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should not contain atLeastOneOf ("6", "7", "8")
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolationArray = getFirst[Array[String]](col, e => e.exists(_ == "6") || e.exists(_ == "7") || e.exists(_ == "8") )
        val firstViolation = decorateToStringValue(firstViolationArray)
        e.message should be (Some("'all' inspection failed, because: \n" +
                                  "  at index " + getIndex(col, firstViolationArray) + ", " + firstViolation + " contained at least one of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                  "in " + decorateToStringValue(col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 12))
            tfe.message should be (Some(firstViolation + " contained at least one of " + right))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    
      def `should work correctly with all(seq) should not contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "3"), Seq("2", "3", "1"), Seq("3", "2", "1")))
          all(col) should not contain atLeastOneOf ("6", "7", "8")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(seq) should not contain failed` {
        val right = "(\"6\", \"7\", \"8\")"
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "8"), Seq("2", "3", "1"), Seq("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain atLeastOneOf ("6", "7", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenSeq[String]](col, e => e.exists(_ == "6") || e.exists(_ == "7") || e.exists(_ == "8") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained at least one of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained at least one of " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(map) should not contain succeeded` {
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three"), Map("2" -> "two", "3" -> "three", "1" -> "one"), Map("3" -> "three", "2" -> "two", "1" -> "one")))
          all(col) should not contain atLeastOneOf ("6" -> "six", "7" -> "seven", "8" -> "eight")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(map) should not contain failed` {
        val right = Array("6" -> "six", "7" -> "seven", "8" -> "eight")
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "8" -> "eight"), Map("2" -> "two", "3" -> "three", "1" -> "one"), Map("3" -> "three", "8" -> "eight", "1" -> "one")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain atLeastOneOf ("6" -> "six", "7" -> "seven", "8" -> "eight")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenMap[String, String]](col, map => map.exists(t => t._1 == "6" && t._2 == "six") || map.exists(t => t._1 == "7" && t._2 == "seven") || map.exists(t => t._1 == "8" && t._2 == "eight") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained at least one of (" + right.mkString(", ") + ") (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained at least one of (" + right.mkString(", ") + ")"))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    }
    
    object `when work with only contain matcher` {
      
      def `should work correctly with all(traversable) should contain succeeded` {
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("2", "1"), Set("1", "2")))
          all(col) should contain only ("1", "2")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(traversable) should contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "8"), Set("1", "2", "3", "8")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain only ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenTraversable[String]](col, e => e.size == 3 && e.exists(_ == "1") && e.exists(_ == "2") && e.exists(_ == "8") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain only " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain only " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(array) should contain succeeded` {
        all(List(Array("1", "2", "3"), Array("1", "3", "2"))) should contain only ("1", "2", "3")
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(array) should contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        val col = List(Array("1", "2", "8"), Array("2", "8", "1"), Array("8", "6", "1"))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should contain only ("1", "2", "8")
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolationArray = getFirstNot[Array[String]](col, e => e.size == 3 && e.exists(_ == "1") && e.exists(_ == "2") && e.exists(_ == "8") )
        val firstViolation = decorateToStringValue(firstViolationArray)
        e.message should be (Some("'all' inspection failed, because: \n" +
                                  "  at index " + getIndex(col, firstViolationArray) + ", " + firstViolation + " did not contain only " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                  "in " + decorateToStringValue(col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 12))
            tfe.message should be (Some(firstViolation + " did not contain only " + right))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    
      def `should work correctly with all(seq) should contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "3"), Seq("1", "3", "2")))
          all(col) should contain only ("1", "2", "3")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(seq) should contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "8"), Seq("2", "3", "1"), Seq("6", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain only ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenSeq[String]](col, e => e.size == 3 && e.exists(_ == "1") && e.exists(_ == "2") && e.exists(_ == "8") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain only " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain only " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(map) should contain succeeded` {
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three")))
          all(col) should contain only ("1" -> "one", "2" -> "two", "3" -> "three")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(map) should contain failed` {
        val right = "(" + Array("1" -> "one", "2" -> "two", "8" -> "eight").mkString(", ") + ")"
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "8" -> "eight"), Map("2" -> "two", "3" -> "three", "1" -> "one"), Map("3" -> "three", "8" -> "eight", "1" -> "one")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain only ("1" -> "one", "2" -> "two", "8" -> "eight")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenMap[String, String]](col, map => map.size == 3 && map.exists(e => e._1 == "1" && e._2 == "one") && map.exists(e => e._1 == "2" && e._2 == "two") && map.exists(e => e._1 == "8" && e._2 == "eight") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain only " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain only " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }

      def `should work correctly with all(traversable) should not contain succeeded` {
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "3")))
          all(col) should not contain only ("1", "2", "8")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(traversable) should not contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "8"), Set("2", "8", "1"), Set("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain only ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenTraversable[String]](col, e => e.size == 3 && e.exists(_ == "1") && e.exists(_ == "2") && e.exists(_ == "8") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained only " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained only " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(array) should not contain succeeded` {
        all(List(Array("1", "2", "3"), Array("2", "3", "1"), Array("3", "2", "1"))) should not contain only ("1", "2", "8")
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(array) should not contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        val col = List(Array("1", "2", "8"), Array("2", "3", "1"), Array("3", "8", "1"))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should not contain only ("1", "2", "8")
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolationArray = getFirst[Array[String]](col, e => e.size == 3 && e.exists(_ == "1") && e.exists(_ == "2") && e.exists(_ == "8") )
        val firstViolation = decorateToStringValue(firstViolationArray)
        e.message should be (Some("'all' inspection failed, because: \n" +
                                  "  at index " + getIndex(col, firstViolationArray) + ", " + firstViolation + " contained only " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                  "in " + decorateToStringValue(col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 12))
            tfe.message should be (Some(firstViolation + " contained only " + right))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    
      def `should work correctly with all(seq) should not contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "3"), Seq("2", "3", "1"), Seq("3", "2", "1")))
          all(col) should not contain only ("6", "7", "8")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(seq) should not contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "8"), Seq("2", "3", "1"), Seq("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain only ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenSeq[String]](col, e => e.size == 3 && e.exists(_ == "1") && e.exists(_ == "2") && e.exists(_ == "8") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained only " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained only " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(map) should not contain succeeded` {
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three"), Map("2" -> "two", "3" -> "three", "1" -> "one"), Map("3" -> "three", "2" -> "two", "1" -> "one")))
          all(col) should not contain only ("1" -> "one", "2" -> "two", "8" -> "eight")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(map) should not contain failed` {
        val right = Array("1" -> "one", "2" -> "two", "8" -> "eight")
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "8" -> "eight"), Map("2" -> "two", "3" -> "three", "1" -> "one"), Map("3" -> "three", "8" -> "eight", "1" -> "one")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain only ("1" -> "one", "2" -> "two", "8" -> "eight")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenMap[String, String]](col, map => map.size == 3 && map.exists(t => t._1 == "1" && t._2 == "one") && map.exists(t => t._1 == "2" && t._2 == "two") && map.exists(t => t._1 == "8" && t._2 == "eight") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained only (" + right.mkString(", ") + ") (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained only (" + right.mkString(", ") + ")"))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    }
    
    object `when work with inOrderOnly contain matcher` {

      def `should work correctly with all(traversable) should contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(List("1", "2", "3")))
          all(col) should contain inOrderOnly ("1", "2", "3")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(traversable) should contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(List("1", "2", "8"), List("1", "6", "8")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain inOrderOnly ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenTraversable[String]](col, e => e.size == 3 && e.toList(0) == "1" && e.toList(1) == "2" && e.toList(2) == "8" )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain only " + right + " in order (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain only " + right + " in order"))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(array) should contain succeeded` {
        all(List(Array("1", "2", "3"), Array("1", "2", "3"))) should contain inOrderOnly ("1", "2", "3")
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(array) should contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        val col = List(Array("1", "2", "8"), Array("2", "8", "1"), Array("8", "2", "1"))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should contain inOrderOnly ("1", "2", "8")
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolationArray = getFirstNot[Array[String]](col, e => e.size == 3 && e(0) == "1" && e(1) == "2" && e(2) == "8" )
        val firstViolation = decorateToStringValue(firstViolationArray)
        e.message should be (Some("'all' inspection failed, because: \n" +
                                  "  at index " + getIndex(col, firstViolationArray) + ", " + firstViolation + " did not contain only " + right + " in order (InspectorShorthandsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                  "in " + decorateToStringValue(col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 12))
            tfe.message should be (Some(firstViolation + " did not contain only " + right + " in order"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    
      def `should work correctly with all(seq) should contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "3"), Seq("1", "2", "3")))
          all(col) should contain inOrderOnly ("1", "2", "3")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(seq) should contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "8"), Seq("2", "8", "1"), Seq("8", "2", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain inOrderOnly ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirstNot[GenSeq[String]](col, e => e.size == 3 && e(0) == "1" && e(1) == "2" && e(2) == "8" )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain only " + right + " in order (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain only " + right + " in order"))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
      
      def `should work correctly with all(traversable) should not contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(List("1", "2", "3")))
          all(col) should not contain inOrderOnly ("1", "2", "8")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(traversable) should not contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(List("1", "2", "8"), List("2", "8", "1"), List("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain inOrderOnly ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenTraversable[String]](col, e => e.size == 3 && e.toList(0) == "1" && e.toList(1) == "2" && e.toList(2) == "8" )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained only " + right + " in order (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained only " + right + " in order"))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(array) should not contain succeeded` {
        all(List(Array("1", "2", "3"), Array("2", "3", "1"), Array("3", "2", "1"))) should not contain inOrderOnly ("1", "2", "8")
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(array) should not contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        val col = List(Array("1", "2", "8"), Array("2", "3", "1"), Array("3", "8", "1"))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should not contain inOrderOnly ("1", "2", "8")
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolationArray = getFirst[Array[String]](col, e => e.size == 3 && e(0) == "1" && e(1) == "2" && e(2) == "8" )
        val firstViolation = decorateToStringValue(firstViolationArray)
        e.message should be (Some("'all' inspection failed, because: \n" +
                                  "  at index " + getIndex(col, firstViolationArray) + ", " + firstViolation + " contained only " + right + " in order (InspectorShorthandsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                  "in " + decorateToStringValue(col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 12))
            tfe.message should be (Some(firstViolation + " contained only " + right + " in order"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    
      def `should work correctly with all(seq) should not contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "3"), Seq("2", "3", "1"), Seq("3", "2", "1")))
          all(col) should not contain inOrderOnly ("1", "2", "8")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(seq) should not contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "8"), Seq("2", "3", "1"), Seq("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain inOrderOnly ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenSeq[String]](col, e => e.size == 3 && e(0) == "1" && e(1) == "2" && e(2) == "8" )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained only " + right + " in order (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained only " + right + " in order"))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    }
    
    object `when work with noneOf contain matcher` {
      
      def `should work correctly with all(traversable) should contain succeeded` {
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "3"), Set("3", "2", "1", "8")))
          all(col) should contain noneOf ("6", "7", "9")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(traversable) should contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "8"), Set("1", "6", "8")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain noneOf ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenTraversable[String]](col, e => e.exists(_ == "1") || e.exists(_ == "2") || e.exists(_ == "8") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained one of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained one of " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(array) should contain succeeded` {
        all(List(Array("1", "2", "3"), Array("3", "2", "1", "8"))) should contain noneOf ("6", "7", "9")
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(array) should contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        val col = List(Array("1", "2", "8"), Array("2", "8", "1"), Array("8", "6", "1"))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should contain noneOf ("1", "2", "8")
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolationArray = getFirst[Array[String]](col, e => e.exists(_ == "1") || e.exists(_ == "2") || e.exists(_ == "8") )
        val firstViolation = decorateToStringValue(firstViolationArray)
        e.message should be (Some("'all' inspection failed, because: \n" +
                                  "  at index " + getIndex(col, firstViolationArray) + ", " + firstViolation + " contained one of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                  "in " + decorateToStringValue(col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 12))
            tfe.message should be (Some(firstViolation + " contained one of " + right))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    
      def `should work correctly with all(seq) should contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "3"), Seq("3", "2", "1", "8")))
          all(col) should contain noneOf ("6", "7", "9")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(seq) should contain failed` {
        val right = "(\"1\", \"2\", \"8\")"
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "8"), Seq("2", "3", "1"), Seq("6", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain noneOf ("1", "2", "8")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenSeq[String]](col, e => e.exists(_ == "1") || e.exists(_ == "2") || e.exists(_ == "8") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained one of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained one of " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(map) should contain succeeded` {
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three")))
          all(col) should contain noneOf ("6" -> "six", "7" -> "seven", "8" -> "eight")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(map) should contain failed` {
        val right = "(" + Array("1" -> "one", "2" -> "two", "8" -> "eight").mkString(", ") + ")"
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "8" -> "eight"), Map("2" -> "two", "3" -> "three", "1" -> "one"), Map("3" -> "three", "8" -> "eight", "1" -> "one")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should contain noneOf ("1" -> "one", "2" -> "two", "8" -> "eight")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenMap[String, String]](col, map => map.exists(e => e._1 == "1" && e._2 == "one") || map.exists(e => e._1 == "2" && e._2 == "two") || map.exists(e => e._1 == "8" && e._2 == "eight") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " contained one of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " contained one of " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(traversable) should not contain succeeded` {
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "3")))
          all(col) should not contain noneOf ("1", "2", "8")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(traversable) should not contain failed` {
        val right = "(\"6\", \"7\", \"9\")"
        forAll(traversableExamples) { colFun => 
          val col = colFun(Set(Set("1", "2", "8"), Set("2", "3", "1"), Set("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain noneOf ("6", "7", "9")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenTraversable[String]](col, e => e.exists(_ != "6") && e.exists(_ != "7") && e.exists(_ != "9") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain one of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain one of " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(array) should not contain succeeded` {
        all(List(Array("1", "2", "3"), Array("2", "3", "1"), Array("3", "2", "1"))) should not contain noneOf ("1", "2", "8")
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(array) should not contain failed` {
        val right = "(\"6\", \"7\", \"9\")"
        val col = List(Array("1", "2", "8"), Array("2", "3", "1"), Array("3", "8", "1"))
        val e = intercept[exceptions.TestFailedException] {
          all(col) should not contain noneOf ("6", "7", "9")
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val firstViolationArray = getFirst[Array[String]](col, e => e.exists(_ != "6") && e.exists(_ != "7") && e.exists(_ != "9") )
        val firstViolation = decorateToStringValue(firstViolationArray)
        e.message should be (Some("'all' inspection failed, because: \n" +
                                  "  at index " + getIndex(col, firstViolationArray) + ", " + firstViolation + " did not contain one of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                  "in " + decorateToStringValue(col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 12))
            tfe.message should be (Some(firstViolation + " did not contain one of " + right))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    
      def `should work correctly with all(seq) should not contain succeeded` {
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "3"), Seq("2", "3", "1"), Seq("3", "2", "1")))
          all(col) should not contain noneOf ("1", "2", "8")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(seq) should not contain failed` {
        val right = "(\"6\", \"7\", \"9\")"
        forAll(seqExamples) { colFun => 
          val col = colFun(Set(Seq("1", "2", "8"), Seq("2", "3", "1"), Seq("3", "8", "1")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain noneOf ("6", "7", "9")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenSeq[String]](col, e => e.exists(_ != "6") && e.exists(_ != "7") && e.exists(_ != "9") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain one of " + right + " (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain one of " + right))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    
      def `should work correctly with all(map) should not contain succeeded` {
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "3" -> "three"), Map("2" -> "two", "3" -> "three", "1" -> "one"), Map("3" -> "three", "2" -> "two", "1" -> "one")))
          all(col) should not contain noneOf ("1" -> "one", "2" -> "two", "8" -> "eight")
        }
      }
    
      def `should throw TestFailedException with correct message and stack depth when all(map) should not contain failed` {
        val right = Array("6" -> "six", "7" -> "seven", "9" -> "nine")
        forAll(mapExamples) { colFun => 
          val col = colFun(Set(Map("1" -> "one", "2" -> "two", "8" -> "eight"), Map("2" -> "two", "3" -> "three", "1" -> "one"), Map("3" -> "three", "8" -> "eight", "1" -> "one")))
          val e = intercept[exceptions.TestFailedException] {
            all(col) should not contain noneOf ("6" -> "six", "7" -> "seven", "9" -> "nine")
          }
          e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
          e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
          val firstViolation = getFirst[GenMap[String, String]](col, map => map.exists(t => t._1 != "6" && t._2 != "six") && map.exists(t => t._1 != "7" && t._2 != "seven") && map.exists(t => t._1 != "9" && t._2 != "nine") )
          e.message should be (Some("'all' inspection failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + decorateToStringValue(firstViolation) + " did not contain one of (" + right.mkString(", ") + ") (InspectorShorthandsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + decorateToStringValue(col)))
          e.getCause match {
            case tfe: exceptions.TestFailedException =>
              tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
              tfe.message should be (Some(decorateToStringValue(firstViolation) + " did not contain one of (" + right.mkString(", ") + ")"))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        }
      }
    }
    object `when used with Arrays` {
      def `should do nothing if succeeds` {
        all(Array(1, 2, 3)) should be < 4
      }
      def `should throw a TFE with a good error message if fails` {
        val e = intercept[exceptions.TestFailedException] {
          all(Array(1, 2, 3, 4, 5)) should be < 4
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 3, 4 was not less than 4 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in Array(1, 2, 3, 4, 5)"))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 10))
            tfe.message should be (Some("4 was not less than 4"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    object `when used with java.util.Collection` {
      import collection.JavaConverters._
      def `should do nothing if succeeds` {
        val jList123: java.util.List[Int] = List(1, 2, 3).asJava
        all(jList123) should be < 4
      }
      def `should throw a TFE with a good error message if fails` {
        val jList12345: java.util.List[Int] = List(1, 2, 3, 4, 5).asJava
        val e = intercept[exceptions.TestFailedException] {
          all(jList12345) should be < 4
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 3, 4 was not less than 4 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in [1, 2, 3, 4, 5]"))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 10))
            tfe.message should be (Some("4 was not less than 4"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    object `when used with java.util.Collection[String]` {
      import collection.JavaConverters._
      def `should do nothing if succeeds` {
        val jList123: java.util.List[String] = List("1", "2", "3").asJava
        all(jList123) should be < "4"
      }
      def `should throw a TFE with a good error message if fails` {
        val jList12345: java.util.List[String] = List("1", "2", "3", "4", "5").asJava
        val e = intercept[exceptions.TestFailedException] {
          all(jList12345) should be < "4"
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 3, \"4\" was not less than \"4\" (InspectorShorthandsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   """in ["1", "2", "3", "4", "5"]"""))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 10))
            tfe.message should be (Some("\"4\" was not less than \"4\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    object `when used with Strings` {
      def `should do nothing if succeeds` {
        all ("123") should be < '4'
      }
      def `should throw a TFE with a good error message if fails` {
        val e = intercept[exceptions.TestFailedException] {
          all ("12345") should be < '4'
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at index 3, '4' was not less than '4' (InspectorShorthandsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in \"12345\""))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 10))
            tfe.message should be (Some("'4' was not less than '4'"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    object `when used with java.util.Map` {
      import collection.JavaConverters._
      def `should do nothing if succeeds` {
        val jMap123: java.util.Map[Int, Int] = Map(1 -> 5, 2 -> 5, 3 -> 5).asJava
        all (jMap123) should have ('value(5))
      }
      def `should throw a TFE with a good error message if fails` {
        val jMap12345: java.util.Map[Int, Int] = javaMap(Entry(1, 5), Entry(2, 5), Entry(3, 5), Entry(4, 6), Entry(5, 5))
        val e = intercept[exceptions.TestFailedException] {
          all(jMap12345) should have ('value(5))
        }
        e.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some("'all' inspection failed, because: \n" +
                                   "  at key 4, The value property had value 6, instead of its expected value 5, on object 4=6 (InspectorShorthandsSpec.scala:" + (thisLineNumber - 5) + ") \n" +
                                   "in {1=5, 2=5, 3=5, 4=6, 5=5}"))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorShorthandsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 10))
            tfe.message should be (Some("The value property had value 6, instead of its expected value 5, on object 4=6"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
  }
}
