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

import Matchers._
import SharedHelpers._
import collection._
import java.lang.annotation.AnnotationFormatError
import java.nio.charset.CoderMalfunctionError
import javax.xml.parsers.FactoryConfigurationError
import javax.xml.transform.TransformerFactoryConfigurationError
import org.scalactic.Prettifier
import org.scalatest.prop.TableDrivenPropertyChecks
import scala.annotation.tailrec
import scala.collection.GenTraversable
import FailureMessages.decorateToStringValue
import org.scalatest.CompatParColls.Converters._

class InspectorsSpec extends FunSpec with Inspectors with TableDrivenPropertyChecks {

  private val prettifier = Prettifier.default
  
  def examples =
    Table[Set[Int] => GenTraversable[Int]](
      ("Fun"), 
      ((set: Set[Int]) => set), 
      ((set: Set[Int]) => set.toList), 
      ((set: Set[Int]) => set.toSeq), 
      ((set: Set[Int]) => set.toArray), 
      ((set: Set[Int]) => set.toIndexedSeq), 
      ((set: Set[Int]) => Vector.empty ++ set),
      // SKIP-SCALATESTJS,NATIVE-START
      ((set: Set[Int]) => set.par), 
      ((set: Set[Int]) => set.toList.par), 
      ((set: Set[Int]) => set.toSeq.par), 
      ((set: Set[Int]) => set.toIndexedSeq.par),
      ((set: Set[Int]) => (mutable.Set.empty ++ set).par),
      ((set: Set[Int]) => (new mutable.ListBuffer() ++ set).par),
      ((set: Set[Int]) => (mutable.Seq.empty ++ set).par),
      ((set: Set[Int]) => (mutable.IndexedSeq.empty ++ set).par),
      // SKIP-SCALATESTJS,NATIVE-END
      ((set: Set[Int]) => mutable.Set.empty ++ set), 
      ((set: Set[Int]) => new mutable.ListBuffer() ++ set), 
      ((set: Set[Int]) => mutable.Seq.empty ++ set), 
      ((set: Set[Int]) => mutable.IndexedSeq.empty ++ set)
    )  
  
  describe("forAll") {
    
    it("should pass when all elements passed") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forAll(col) { e => e should be < 4 }
      }
    }
    
    ignore("should, when passed a Fact, convert that Fact to an Assertion") { // Unignore after we uncomment the expectation implicits in RegistrationPolicy
      import expectations.Expectations._

      forAll(List(1, 2, 3)) { x => expect(x > 0) } shouldBe Succeeded

      val tfe = intercept[exceptions.TestFailedException] {
        forAll(List(1, 2, 3)) { x => expect(x < 0) }
      }
      tfe.failedCodeFileName should be (Some("InspectorsSpec.scala"))
      tfe.failedCodeLineNumber should be (Some( thisLineNumber - 3))
    }
    
    it("should throw TestFailedException with correct stack depth and message when at least one element failed") {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forAll(col) { e => 
            e should not equal 2 
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        e.message should be (Some("forAll failed, because: \n" +
                                   "  at index " + getIndex(col, 2) + ", 2 equaled 2 (InspectorsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, col)))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("2 equaled 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    it("should throw TestFailedException with correct stack depth and message when more than one element failed") {
      forAll(examples) { colFun => 
        val col = colFun(Set(1, 2, 3))
        val e2 = intercept[exceptions.TestFailedException] {
          forAll(col) { e => 
            e should be < 2 
          }
        }
        e2.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val firstViolation = getFirst[Int](col, _ >= 2)
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at index " + getIndex(col, firstViolation) + ", " + firstViolation + " was not less than 2 (InspectorsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                    "in " + decorateToStringValue(prettifier, col)))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 12))
            tfe.message should be (Some(firstViolation + " was not less than 2"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    it("should propagate TestPendingException thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[exceptions.TestPendingException] {
          forAll(col) { e => pending }
        }
      }
    }
    
    it("should propagate TestCanceledException thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[exceptions.TestCanceledException] {
          forAll(col) { e => cancel }
        }  
      }
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[AnnotationFormatError] {
          forAll(col) { e => throw new AnnotationFormatError("test") }
        }
      }
    }
    
    it("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[CoderMalfunctionError] {
          forAll(col) { e => throw new CoderMalfunctionError(new RuntimeException("test")) }
        }
      }
    }
    
    it("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[FactoryConfigurationError] {
          forAll(col) { e => throw new FactoryConfigurationError() }
        }
      }
    }
    
    it("should propagate java.lang.LinkageError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[LinkageError] {
          forAll(col) { e => throw new LinkageError() }
        }
      }
    }
    
    it("should propagate java.lang.ThreadDeath thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[ThreadDeath] {
          forAll(col) { e => throw new ThreadDeath() }
        }
      }
    }
    
    it("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[TransformerFactoryConfigurationError] {
          forAll(col) { e => throw new TransformerFactoryConfigurationError() }
        }
      }
    }
    
    it("should propagate java.lang.VirtualMachineError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[VirtualMachineError] {
          forAll(col) { e => throw new VirtualMachineError() {} }
        }
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END

    describe("when used with Arrays") {
      it("should do nothing if succeeds") {
        forAll(Array(1, 2, 3)) { e => e should be < 4 }
      }
      it("should throw a TFE with a good error message if fails") {
        val e = intercept[exceptions.TestFailedException] {
          forAll(Array(1, 2, 3, 4, 5)) { e =>
            e should be < 4
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        e.message should be (Some("forAll failed, because: \n" +
                                   "  at index 3, 4 was not less than 4 (InspectorsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                   "in Array(1, 2, 3, 4, 5)"))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("4 was not less than 4"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }

    describe("when used with Strings") {
      it("should do nothing if succeeds") {
        forAll("123") { e => e should be < '4' }
      }
      it("should throw a TFE with a good error message if fails") {
        val e = intercept[exceptions.TestFailedException] {
          forAll("12345") { e =>
            e should be < '4'
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        e.message should be (Some("forAll failed, because: \n" +
                                   "  at index 3, '4' was not less than '4' (InspectorsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                   "in \"12345\""))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("'4' was not less than '4'"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }

    // SKIP-SCALATESTJS,NATIVE-START
    describe("when used with java.util.Collection") {
      import collection.JavaConverters._
      it("should do nothing if succeeds") {
        val jList123: java.util.List[Int] = List(1, 2, 3).asJava
        forAll(jList123) { e => e should be < 4 }
      }
      it("should throw a TFE with a good error message if fails") {
        val jList12345: java.util.List[Int] = List(1, 2, 3, 4, 5).asJava
        val e = intercept[exceptions.TestFailedException] {
          forAll(jList12345) { e =>
            e should be < 4
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        e.message should be (Some("forAll failed, because: \n" +
                                   "  at index 3, 4 was not less than 4 (InspectorsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                   "in [1, 2, 3, 4, 5]"))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("4 was not less than 4"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }

    describe("when used with java.util.Map") {
      import collection.JavaConverters._
      it("should do nothing if succeeds") {
        val jMap123: java.util.Map[Int, Int] = Map(1 -> 2, 2 -> 3, 3 -> 4).asJava
        forAll(jMap123) { e => e.key should be < 4 }
      }
      it("should throw a TFE with a good error message if fails") {
        val jMap12345: java.util.Map[Int, Int] = javaMap(Entry(1, 2), Entry(2, 3), Entry(3, 4), Entry(4, 5), Entry(5, 6))
        val e = intercept[exceptions.TestFailedException] {
          forAll(jMap12345) { e =>
            e.key should be < 4
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        e.message should be (Some("forAll failed, because: \n" +
                                   "  at key 4, 4 was not less than 4 (InspectorsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                   "in {1=2, 2=3, 3=4, 4=5, 5=6}"))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorsSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("4 was not less than 4"))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
  }
  
  describe("forAtLeast ") {
    
    it("should throw IllegalArgumentException when 0 is passed in as min") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forAtLeast(0, col) { e => e should be (2) }
        }
        e.getMessage should be ("'min' argument must be more than 0")
      }
    }
    
    it("should throw IllegalArgumentException when -1 is passed in as min") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forAtLeast(-1, col) { e => e should be (2) }
        }
        e.getMessage should be ("'min' argument must be more than 0")
      }
    }
    
    it("should pass when minimum count of elements passed") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forAtLeast(1, col) { e => e should be (2) }
      }
    }
    
    it("should throw TestFailedException with correct stack depth and message when less than minimum count of elements passed") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forAtLeast(2, col) { e => 
            e should be (2) 
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val first = getNext[Int](itr, _ != 2)
        val firstIndex = getIndex(col, first)
        val second = getNext[Int](itr, _ != 2)
        val secondIndex = getIndex(col, second)
        e.message should be (Some("forAtLeast(2) failed, because only 1 element satisfied the assertion block: \n" +
                                   "  at index " + firstIndex + ", " + first + " was not equal to 2 (InspectorsSpec.scala:" + (thisLineNumber - 11) + "), \n" +
                                   "  at index " + secondIndex + ", " + second + " was not equal to 2 (InspectorsSpec.scala:" + (thisLineNumber - 12) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, col)))
        e.getCause should be (null)
      }
    }
    
    it("should use 'no element' in error message when no element satisfied the assertion block") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forAtLeast(2, col) { e => 
            e should be (5) 
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val first = itr.next
        val second = itr.next
        val third = itr.next
        e.message should be (Some("forAtLeast(2) failed, because no element satisfied the assertion block: \n" +
                                   "  at index 0, " + first + " was not equal to 5 (InspectorsSpec.scala:" + (thisLineNumber - 10) + "), \n" +
                                   "  at index 1, " + second + " was not equal to 5 (InspectorsSpec.scala:" + (thisLineNumber - 11) + "), \n" +
                                   "  at index 2, " + third + " was not equal to 5 (InspectorsSpec.scala:" + (thisLineNumber - 12) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, col)))
        e.getCause should be (null)
      }
    }
    
    it("should use 'element' in error message when exactly 1 element satisfied the assertion block") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forAtLeast(2, col) { e => 
            e should be (2)
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val first = getNextNot[Int](itr, _ == 2)
        val firstIndex = getIndex(col, first)
        val second = getNextNot[Int](itr, _ == 2)
        val secondIndex = getIndex(col, second)
        e.message should be (Some("forAtLeast(2) failed, because only 1 element satisfied the assertion block: \n" +
                                   "  at index " + firstIndex + ", " + first + " was not equal to 2 (InspectorsSpec.scala:" + (thisLineNumber - 11) + "), \n" +
                                   "  at index " + secondIndex + ", " + second + " was not equal to 2 (InspectorsSpec.scala:" + (thisLineNumber - 12) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, col)))
        e.getCause should be (null)
      }
    }
    
    it("should use 'elements' in error message when > 1 element satisfied the assertion block") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forAtLeast(3, col) { e => 
            e should be < 3 
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val index = getIndex(col, 3)
        e.message should be (Some("forAtLeast(3) failed, because only 2 elements satisfied the assertion block: \n" +
                                   "  at index " + index + ", 3 was not less than 3 (InspectorsSpec.scala:" + (thisLineNumber - 7) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, col)))
        e.getCause should be (null)
      }
    }
    
    it("should pass when more than minimum count of elements passed") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forAtLeast(1, col) { e => e should be < 3 }
      }
    }
    
    it("should throw TestFailedException with correct stack depth and message when none of the elements passed") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forAtLeast(1, col) { e => 
            e should be > 5 
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val first = itr.next
        val second = itr.next
        val third = itr.next
        e.message should be (Some("forAtLeast(1) failed, because no element satisfied the assertion block: \n" +
                                   "  at index 0, " + first + " was not greater than 5 (InspectorsSpec.scala:" + (thisLineNumber - 10) + "), \n" +
                                   "  at index 1, " + second + " was not greater than 5 (InspectorsSpec.scala:" + (thisLineNumber - 11) + "), \n" +
                                   "  at index 2, " + third + " was not greater than 5 (InspectorsSpec.scala:" + (thisLineNumber - 12) + ") \n" +
                                   "in " + decorateToStringValue(prettifier, col)))
        e.getCause should be (null)
      }
    }
    
    it("should pass when all of the elements passed") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forAtLeast(1, col) { e => e should be < 5 }
      }
    }
    
    it("should propagate TestPendingException thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[exceptions.TestPendingException] {
          forAtLeast(1, col) { e => pending }
        }
      }
    }
    
    it("should propagate TestCanceledException thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[exceptions.TestCanceledException] {
          forAtLeast(1, col) { e => cancel }
        }
      }
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[AnnotationFormatError] {
          forAtLeast(1, col) { e => throw new AnnotationFormatError("test") }
        }
      }
    }
    
    it("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[CoderMalfunctionError] {
          forAtLeast(1, col) { e => throw new CoderMalfunctionError(new RuntimeException("test")) }
        }
      }
    }
    
    it("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[FactoryConfigurationError] {
          forAtLeast(1, col) { e => throw new FactoryConfigurationError() }
        }
      }
    }
    
    it("should propagate java.lang.LinkageError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[LinkageError] {
          forAtLeast(1, col) { e => throw new LinkageError() }
        }
      }
    }
    
    it("should propagate java.lang.ThreadDeath thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[ThreadDeath] {
          forAtLeast(1, col) { e => throw new ThreadDeath() }
        }
      }
    }
    
    it("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[TransformerFactoryConfigurationError] {
          forAtLeast(1, col) { e => throw new TransformerFactoryConfigurationError() }
        }
      }
    }
    
    it("should propagate java.lang.VirtualMachineError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[VirtualMachineError] {
          forAtLeast(1, col) { e => throw new VirtualMachineError() {} }
        }
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
  }

  describe("forAtMost ") {
    
    it("should throw IllegalArgumentException when 0 is passed in as max") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forAtMost(0, col) { e => e should be (2) }
        }
        e.getMessage should be ("'max' argument must be more than 0")
      }
    }
    
    it("should throw IllegalArgumentException when -1 is passed in as max") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forAtMost(-1, col) { e => e should be (2) }
        }
        e.getMessage should be ("'max' argument must be more than 0")
      }
    }
    
    it("should pass when number of elements passed is less than maximum allowed") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forAtMost(2, col) { e => e should be (2) }
      }
    }
    
    it("should pass when number of elements passed equal to maximum allowed") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forAtMost(2, col) { e => e should be < 3 }
      }
    }
    
    it("should throw TestFailedException with correct stack depth and message when number of element passed is more than maximum allowed") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        val e = intercept[exceptions.TestFailedException] {
          forAtMost(2, col) { e => 
            e should be < 4 
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val first = getNext[Int](itr, _ < 4)
        val firstIndex = getIndex(col, first)
        val second = getNext[Int](itr, _ < 4)
        val secondIndex = getIndex(col, second)
        val third = getNext[Int](itr, _ < 4)
        val thirdIndex = getIndex(col, third)
        e.message should be (Some("forAtMost(2) failed, because 3 elements satisfied the assertion block at index " + firstIndex + ", " + secondIndex + " and " + thirdIndex + " in " + decorateToStringValue(prettifier, col)))
      }
    }
    
    it("should pass when none of the elements passed") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forAtMost(2, col) { e => e should be > 5 }
      }
    }
    
    it("should propagate TestPendingException thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[exceptions.TestPendingException] {
          forAtMost(1, col) { e => pending }
        }
      }
    }
    
    it("should propagate TestCanceledException thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[exceptions.TestCanceledException] {
          forAtMost(1, col) { e => cancel }
        }
      }
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[AnnotationFormatError] {
          forAtMost(1, col) { e => throw new AnnotationFormatError("test") }
        }
      }
    }
    
    it("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[CoderMalfunctionError] {
          forAtMost(1, col) { e => throw new CoderMalfunctionError(new RuntimeException("test")) }
        }
      }
    }
    
    it("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[FactoryConfigurationError] {
          forAtMost(1, col) { e => throw new FactoryConfigurationError() }
        }
      }
    }
    
    it("should propagate java.lang.LinkageError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[LinkageError] {
          forAtMost(1, col) { e => throw new LinkageError() }
        }
      }
    }
    
    it("should propagate java.lang.ThreadDeath thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[ThreadDeath] {
          forAtMost(1, col) { e => throw new ThreadDeath() }
        }
      }
    }
    
    it("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[TransformerFactoryConfigurationError] {
          forAtMost(1, col) { e => throw new TransformerFactoryConfigurationError() }
        }
      }
    }
    
    it("should propagate java.lang.VirtualMachineError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[VirtualMachineError] {
          forAtMost(1, col) { e => throw new VirtualMachineError() {} }
        }
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
  }
  
  describe("forExactly ") {
    
    it("should throw IllegalArgumentException when 0 is passed in as max") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forExactly(0, col) { e => e should be (2) }
        }
        e.getMessage should be ("'succeededCount' argument must be more than 0")
      }
    }
    
    it("should throw IllegalArgumentException when -1 is passed in as max") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forExactly(-1, col) { e => e should be (2) }
        }
        e.getMessage should be ("'succeededCount' argument must be more than 0")
      }
    }
    
    it("should pass when number of element passes is equal to specified succeeded count") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forExactly(2, col) { e => e should be < 3 }
      }
    }
    
    it("should use 'no element' in error message when no element satisfied the assertion block") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forExactly(2, col) { e => 
            e should be (5) 
          }
        }
        val itr = col.toIterator
        val first = itr.next
        val second = itr.next
        val third = itr.next
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 9))
        e.message should be (Some("forExactly(2) failed, because no element satisfied the assertion block: \n" +
                                  "  at index 0, " + first + " was not equal to 5 (InspectorsSpec.scala:" + (thisLineNumber - 10) + "), \n" +
                                  "  at index 1, " + second + " was not equal to 5 (InspectorsSpec.scala:" + (thisLineNumber - 11) + "), \n" +
                                  "  at index 2, " + third + " was not equal to 5 (InspectorsSpec.scala:" + (thisLineNumber - 12) + ") \n" +
                                  "in " + decorateToStringValue(prettifier, col)))
        e.getCause should be (null)
      }
    }
    
    it("should use 'element' in error message when exactly 1 element satisfied the assertion block, when passed count is less than the expected count") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forExactly(2, col) { e => 
            e should be (2)
          }
        }
        val itr = col.toIterator
        val first = getNextNot[Int](itr, _ == 2)
        val firstIndex = getIndex(col, first)
        val second = getNextNot[Int](itr, _ == 2)
        val secondIndex = getIndex(col, second)
        val succeededIndex = getIndex(col, 2)
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 11))
        e.message should be (Some("forExactly(2) failed, because only 1 element satisfied the assertion block at index " + succeededIndex + ": \n" +
                                  "  at index " + firstIndex + ", " + first + " was not equal to 2 (InspectorsSpec.scala:" + (thisLineNumber - 12) + "), \n" +
                                  "  at index " + secondIndex + ", " + second + " was not equal to 2 (InspectorsSpec.scala:" + (thisLineNumber - 13) + ") \n" +
                                  "in " + decorateToStringValue(prettifier, col)))
        e.getCause should be (null)
      }
    }
    
    it("should use 'element' in error message when exactly 1 element satisfied the assertion block, when passed count is more than the expected count") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forExactly(2, col) { e => 
            e should be < 5
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        e.message should be (Some("forExactly(2) failed, because 3 elements satisfied the assertion block at index 0, 1 and 2 in " + decorateToStringValue(prettifier, col)))
        e.getCause should be (null)
      }
    }
    
    it("should use 'elements' in error message when > 1 element satisfied the assertion block, when passed count is less than the expected count") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forExactly(3, col) { e => 
            e should be < 3 
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val passedItr = col.toIterator
        val passedFirst = getNext[Int](passedItr, _ < 3)
        val passedFirstIndex = getIndex(col, passedFirst)
        val passedSecond = getNext[Int](passedItr, _ < 3)
        val passedSecondIndex = getIndex(col, passedSecond)
        val failedIndex = getIndex(col, 3)
        e.message should be (Some("forExactly(3) failed, because only 2 elements satisfied the assertion block at index " + passedFirstIndex + " and " + passedSecondIndex + ": \n" +
                                  "  at index " + failedIndex + ", 3 was not less than 3 (InspectorsSpec.scala:" + (thisLineNumber - 12) + ") \n" +
                                  "in " + decorateToStringValue(prettifier, col)))
        e.getCause should be (null)
      }
    }
    
    it("should use 'elements' in error message when > 1 element satisfied the assertion block, when passed count is more than the expected count") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forExactly(1, col) { e => 
            e should be < 3 
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val first = getNext[Int](itr, _ < 3)
        val firstIndex = getIndex(col, first)
        val second = getNext[Int](itr, _ < 3)
        val secondIndex = getIndex(col, second)
        e.message should be (Some("forExactly(1) failed, because 2 elements satisfied the assertion block at index " + firstIndex + " and " + secondIndex + " in " + decorateToStringValue(prettifier, col)))
        e.getCause should be (null)
      }
    }
    
    it("should throw TestFailedException with correct stack depth and message when number of element passed is less than specified succeeded count") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forExactly(2, col) { e => 
            e should equal (2) 
          }
        }
        val index = getIndex(col, 2)
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 6))
        val itr = col.toIterator
        val first = getNext[Int](itr, _ != 2)
        val firstIndex = getIndex(col, first)
        val second = getNext[Int](itr, _ != 2)
        val secondIndex = getIndex(col, second)
        e.message should be (Some("forExactly(2) failed, because only 1 element satisfied the assertion block at index " + index + ": \n" +
                                  "  at index " + firstIndex + ", " + first + " did not equal 2 (InspectorsSpec.scala:" + (thisLineNumber - 12) + "), \n" +
                                  "  at index " + secondIndex + ", " + second + " did not equal 2 (InspectorsSpec.scala:" + (thisLineNumber - 13) + ") \n" +
                                  "in " + decorateToStringValue(prettifier, col)))
      }
    }
    
    it("should throw TestFailedException with correct stack depth and messsage when number of element passed is more than specified succeeded count") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forExactly(2, col) { e => e should be < 5 }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some("forExactly(2) failed, because 3 elements satisfied the assertion block at index 0, 1 and 2 in " + decorateToStringValue(prettifier, col)))
      }
    }
    
    it("should propagate TestPendingException thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[exceptions.TestPendingException] {
          forExactly(1, col) { e => pending }
        }
      }
    }
    
    it("should propagate TestCanceledException thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[exceptions.TestCanceledException] {
          forExactly(1, col) { e => cancel }
        }
      }
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[AnnotationFormatError] {
          forExactly(1, col) { e => throw new AnnotationFormatError("test") }
        }
      }
    }
    
    it("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[CoderMalfunctionError] {
          forExactly(1, col) { e => throw new CoderMalfunctionError(new RuntimeException("test")) }
        }
      }
    }
    
    it("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[FactoryConfigurationError] {
          forExactly(1, col) { e => throw new FactoryConfigurationError() }
        }
      }
    }
    
    it("should propagate java.lang.LinkageError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[LinkageError] {
          forExactly(1, col) { e => throw new LinkageError() }
        }
      }
    }
    
    it("should propagate java.lang.ThreadDeath thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[ThreadDeath] {
          forExactly(1, col) { e => throw new ThreadDeath() }
        }
      }
    }
    
    it("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[TransformerFactoryConfigurationError] {
          forExactly(1, col) { e => throw new TransformerFactoryConfigurationError() }
        }
      }
    }
    
    it("should propagate java.lang.VirtualMachineError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[VirtualMachineError] {
          forExactly(1, col) { e => throw new VirtualMachineError() {} }
        }
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
  }
  
  describe("forNo ") {
    
    it("should pass when none of the element pass") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forNo(col) { e => e should be > 5 }
      }
    }
    
    it("should throw TestFailedException with correct stack depth and message when 1 element passed") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forNo(col) { e => e should equal (2) }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val index = getIndex(col, 2)
        e.message should be (Some("forNo failed, because 1 element satisfied the assertion block at index " + index + " in " + decorateToStringValue(prettifier, col)))
      }
    }
    
    it("should throw TestFailedException with correct stack depth and message when 2 element passed") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forNo(col) { e => e should be < 3 }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val first = col.toIterator.next
        e.message should be (Some("forNo failed, because 1 element satisfied the assertion block at index 0 in " + decorateToStringValue(prettifier, col)))
      }
    }
    
    it("should throw TestFailedException with correct stack depth and message when all elements passed") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forNo(col) { e => e should be < 5 }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some("forNo failed, because 1 element satisfied the assertion block at index 0 in " + decorateToStringValue(prettifier, col)))
      }
    }
    
    it("should pass when empty list of element is passed in") {
      forAll(examples) { colFun =>
        val col = colFun(Set.empty[Int])
        forNo(col) { e => e should be < 5 }
      }
    }
    
    it("should propagate TestPendingException thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[exceptions.TestPendingException] {
          forNo(col) { e => pending }
        }
      }
    }
    
    it("should propagate TestCanceledException thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[exceptions.TestCanceledException] {
          forNo(col) { e => cancel }
        }
      }
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[AnnotationFormatError] {
          forNo(col) { e => throw new AnnotationFormatError("test") }
        }
      }
    }
    
    it("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[CoderMalfunctionError] {
          forNo(col) { e => throw new CoderMalfunctionError(new RuntimeException("test")) }
        }
      }
    }
    
    it("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[FactoryConfigurationError] {
          forNo(col) { e => throw new FactoryConfigurationError() }
        }
      }
    }
    
    it("should propagate java.lang.LinkageError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[LinkageError] {
          forNo(col) { e => throw new LinkageError() }
        }
      }
    }
    
    it("should propagate java.lang.ThreadDeath thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[ThreadDeath] {
          forNo(col) { e => throw new ThreadDeath() }
        }
      }
    }
    
    it("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[TransformerFactoryConfigurationError] {
          forNo(col) { e => throw new TransformerFactoryConfigurationError() }
        }
      }
    }
    
    it("should propagate java.lang.VirtualMachineError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[VirtualMachineError] {
          forNo(col) { e => throw new VirtualMachineError() {} }
        }
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
  }
  
  describe("forBetween ") {
    
    it("should throw IllegalArgumentException when -1 is passed in as from") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forBetween(-1, 2, col) { e => e should be (2) }
        }
        e.getMessage should be ("'from' argument must be more than or equal 0")
      }
    }
    
    it("should throw IllegalArgumentException when 0 is passed in as upTo") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forBetween(0, 0, col) { e => e should be (2) }
        }
        e.getMessage should be ("'upTo' argument must be more than 0")
      }
    }
    
    it("should throw IllegalArgumentException when -1 is passed in as upTo") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forBetween(0, -1, col) { e => e should be (2) }
        }
        e.getMessage should be ("'upTo' argument must be more than 0")
      }
    }
    
    it("should throw IllegalArgumentException when from and upTo is the same") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forBetween(1, 1, col) { e => e should be (2) }
        }
        e.getMessage should be ("'upTo' argument must be more than 'from' argument")
      }
    }
    
    it("should throw IllegalArgumentException when from is greater than upTo") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forBetween(3, 2, col) { e => e should be (2) }
        }
        e.getMessage should be ("'upTo' argument must be more than 'from' argument")
      }
    }
    
    it("should pass when number of element passed is within the specified range") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        forBetween(2, 4, col) { e => e should be > 2 }
      }
    }
    
    it("should pass when number of element passed is same as lower bound of the specified range") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        forBetween(2, 4, col) { e => e should be > 3 }
      }
    }
    
    it("should pass when number of element passed is same as upper bound of the specified range") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        forBetween(2, 4, col) { e => e should be > 1 }
      }
    }
    
    it("should use 'no element' in error message when no element satisfied the assertion block and 'from' is > 0 ") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forBetween(1, 2, col) { e => 
            e should be (5) 
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val first = itr.next
        val second = itr.next
        val third = itr.next
        e.message should be (Some("forBetween(1, 2) failed, because no element satisfied the assertion block: \n" +
                                  "  at index 0, " + first + " was not equal to 5 (InspectorsSpec.scala:" + (thisLineNumber - 10) + "), \n" +
                                  "  at index 1, " + second + " was not equal to 5 (InspectorsSpec.scala:" + (thisLineNumber - 11) + "), \n" +
                                  "  at index 2, " + third + " was not equal to 5 (InspectorsSpec.scala:" + (thisLineNumber - 12) + ") \n" +
                                  "in " + decorateToStringValue(prettifier, col)))
        e.getCause should be (null)
      }
    }
    
    it("should use 'element' in error message when exactly 1 element satisfied the assertion block, when total passed is less than 'from'") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forBetween(2, 3, col) { e => 
            e should be (2)
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val first = getNextNot[Int](itr, _ == 2)
        val firstIndex = getIndex(col, first)
        val second = getNextNot[Int](itr, _ == 2)
        val secondIndex = getIndex(col, second)
        val passedIndex = getIndex(col, 2)
        e.message should be (Some("forBetween(2, 3) failed, because only 1 element satisfied the assertion block at index " + passedIndex + ": \n" +
                                  "  at index " + firstIndex + ", " + first + " was not equal to 2 (InspectorsSpec.scala:" + (thisLineNumber - 12) + "), \n" +
                                  "  at index " + secondIndex + ", " + second + " was not equal to 2 (InspectorsSpec.scala:" + (thisLineNumber - 13) + ") \n" +
                                  "in " + decorateToStringValue(prettifier, col)))
        e.getCause should be (null)
      }
    }
    
    it("should use 'elements' in error message when > 1 element satisfied the assertion block, when total passed is less than 'from'") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forBetween(3, 4, col) { e => 
            e should be < 3 
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val firstIndex = getIndex(col, getNext[Int](itr, _ < 3))
        val secondIndex = getIndex(col, getNext[Int](itr, _ < 3))
        val failedIndex = getIndex(col, 3)
        e.message should be (Some("forBetween(3, 4) failed, because only 2 elements satisfied the assertion block at index " + firstIndex + " and " + secondIndex + ": \n" +
                                  "  at index " + failedIndex + ", 3 was not less than 3 (InspectorsSpec.scala:" + (thisLineNumber - 10) + ") \n" +
                                  "in " + decorateToStringValue(prettifier, col)))
        e.getCause should be (null)
      }
    }
    
    it("should use 'elements' in error message when > 1 element satisfied the assertion block, when total passed is more than 'upTo'") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        val e = intercept[exceptions.TestFailedException] {
          forBetween(2, 3, col) { e => 
            e should be > 1 
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val firstIndex = getIndex(col, getNext[Int](itr, _ > 1))
        val secondIndex = getIndex(col, getNext[Int](itr, _ > 1))
        val thirdIndex = getIndex(col, getNext[Int](itr, _ > 1))
        val forthIndex = getIndex(col, getNext[Int](itr, _ > 1))
        e.message should be (Some("forBetween(2, 3) failed, because 4 elements satisfied the assertion block at index " + firstIndex + ", " + secondIndex + ", " + thirdIndex + " and " + forthIndex + " in " + decorateToStringValue(prettifier, col)))
        e.getCause should be (null)
      }
    }
    
    it("should throw TestFailedException with correct stack depth and message when number of element passed is less than lower bound of the specified range") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        val e = intercept[exceptions.TestFailedException] {
          forBetween(2, 4, col) { e => 
            e should be > 4 
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val index = getIndex(col, 5)
        val itr = col.toIterator
        val first = getNextNot[Int](itr, _ > 4)
        val second = getNextNot[Int](itr, _ > 4)
        val third = getNextNot[Int](itr, _ > 4)
        val forth = getNextNot[Int](itr, _ > 4)
        val firstIndex = getIndex(col, first)
        val secondIndex = getIndex(col, second)
        val thirdIndex = getIndex(col, third)
        val forthIndex = getIndex(col, forth)
        e.message should be (Some("forBetween(2, 4) failed, because only 1 element satisfied the assertion block at index " + index + ": \n" + 
                                  "  at index " + firstIndex + ", " + first  + " was not greater than 4 (InspectorsSpec.scala:" + (thisLineNumber - 16) + "), \n" +
                                  "  at index " + secondIndex + ", " + second  + " was not greater than 4 (InspectorsSpec.scala:" + (thisLineNumber - 17) + "), \n" +
                                  "  at index " + thirdIndex + ", " + third  + " was not greater than 4 (InspectorsSpec.scala:" + (thisLineNumber - 18) + "), \n" +
                                  "  at index " + forthIndex + ", " + forth  + " was not greater than 4 (InspectorsSpec.scala:" + (thisLineNumber - 19) + ") \n" +
                                  "in " + decorateToStringValue(prettifier, col)))
      }
    }
    
    it("should throw TestFailedException with correct stack depth and message when number of element passed is more than upper bound of the specified range") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        val e = intercept[exceptions.TestFailedException] {
          forBetween(2, 4, col) { e => e should be > 0 }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some("forBetween(2, 4) failed, because 5 elements satisfied the assertion block at index 0, 1, 2, 3 and 4 in " + decorateToStringValue(prettifier, col)))
      }
    }
    
    it("should propagate TestPendingException thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        assertThrows[exceptions.TestPendingException] {
          forBetween(2, 4, col) { e => pending }
        }
      }
    }
    
    it("should propagate TestCanceledException thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        assertThrows[exceptions.TestCanceledException] {
          forBetween(2, 4, col) { e => cancel }
        }
      }
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        assertThrows[AnnotationFormatError] {
          forBetween(2, 4, col) { e => throw new AnnotationFormatError("test") }
        }
      }
    }
    
    it("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        assertThrows[CoderMalfunctionError] {
          forBetween(2, 4, col) { e => throw new CoderMalfunctionError(new RuntimeException("test")) }
        }
      }
    }
    
    it("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        assertThrows[FactoryConfigurationError] {
          forBetween(2, 4, col) { e => throw new FactoryConfigurationError() }
        }
      }
    }
    
    it("should propagate java.lang.LinkageError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        assertThrows[LinkageError] {
          forBetween(2, 4, col) { e => throw new LinkageError() }
        }
      }
    }
    
    it("should propagate java.lang.ThreadDeath thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        assertThrows[ThreadDeath] {
          forBetween(2, 4, col) { e => throw new ThreadDeath() }
        }
      }
    }
    
    it("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        assertThrows[TransformerFactoryConfigurationError] {
          forBetween(2, 4, col) { e => throw new TransformerFactoryConfigurationError() }
        }
      }
    }
    
    it("should propagate java.lang.VirtualMachineError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        assertThrows[VirtualMachineError] {
          forBetween(2, 4, col) { e => throw new VirtualMachineError() {} }
        }
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
  }
  
  describe("forEvery ") {
    
    it("should pass when all elements passed") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forEvery(col) { e => e should be < 4 }
      }
    }
    
    it("should throw TestFailedException with correct stack depth and message when at least one element failed") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forEvery(col) { e => e should not equal 2 }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val index = getIndex(col, 2)
        e.message should be (Some("forEvery failed, because: \n" +
                                  "  at index " + index + ", 2 equaled 2 (InspectorsSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                  "in " + decorateToStringValue(prettifier, col)))
      }
    }
    
    it("should throw TestFailedException with correct stack depth and message when more than one element failed") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forEvery(col) { e => e should be < 2 }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val itr = col.toIterator
        val first = getNextNot[Int](itr, _ < 2)
        val firstIndex = getIndex(col, first)
        val second = getNextNot[Int](itr, _ < 2)
        val secondIndex = getIndex(col, second)
        e.message should be (Some("forEvery failed, because: \n" +
                                  "  at index " + firstIndex + ", " + first + " was not less than 2 (InspectorsSpec.scala:" + (thisLineNumber - 10) + "), \n" +
                                  "  at index " + secondIndex + ", " + second + " was not less than 2 (InspectorsSpec.scala:" + (thisLineNumber - 11) + ") \n" +
                                  "in " + decorateToStringValue(prettifier, col)))
      }
    }
    
    it("should propagate TestPendingException thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[exceptions.TestPendingException] {
          forEvery(col) { e => pending }
        }
      }
    }
    
    it("should propagate TestCanceledException thrown from assertion") {
      forAll(examples) { colFun =>
        val colFun = Set(1, 2, 3)
        assertThrows[exceptions.TestCanceledException] {
          forEvery(colFun) { e => cancel }
        }
      }
    }

    // SKIP-SCALATESTJS,NATIVE-START
    it("should propagate java.lang.annotation.AnnotationFormatError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[AnnotationFormatError] {
          forEvery(col) { e => throw new AnnotationFormatError("test") }
        }
      }
    }
    
    it("should propagate java.nio.charset.CoderMalfunctionError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[CoderMalfunctionError] {
          forEvery(col) { e => throw new CoderMalfunctionError(new RuntimeException("test")) }
        }
      }
    }
    
    it("should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[FactoryConfigurationError] {
          forEvery(col) { e => throw new FactoryConfigurationError() }
        }
      }
    }
    
    it("should propagate java.lang.LinkageError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[LinkageError] {
          forEvery(col) { e => throw new LinkageError() }
        }
      }
    }
    
    it("should propagate java.lang.ThreadDeath thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[ThreadDeath] {
          forEvery(col) { e => throw new ThreadDeath() }
        }
      }
    }
    
    it("should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[TransformerFactoryConfigurationError] {
          forEvery(col) { e => throw new TransformerFactoryConfigurationError() }
        }
      }
    }
    
    it("should propagate java.lang.VirtualMachineError thrown from assertion") {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        assertThrows[VirtualMachineError] {
          forEvery(col) { e => throw new VirtualMachineError() {} }
        }
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
  }
  
  describe("forAll nested") {
    
    it("should have no problem nesting themselves") {
      val listOfList = 
        List(
          List(2, 4, 6), 
          List(8, 10, 12)
        )
      forAll(listOfList) { l => 
        forAll(l) { _ % 2 should be (0) }
      }
    }
    
    it("should throw TestFailedException with correct stack depth and error message when at least one element fails in nested forAll ") {
      val listOfList = 
        List(
          List(2, 4, 6), 
          List(8, 9, 12)
        )
      val e = intercept[exceptions.TestFailedException] {
        forAll(listOfList) { l => 
          forAll(l) { 
            _ % 2 should be (0) 
          }
        }
      }
      e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 7))
      e.message should be (Some("forAll failed, because: \n" +
                                "  at index 1, forAll failed, because: \n" +
                                "    at index 1, 1 was not equal to 0 (InspectorsSpec.scala:" + (thisLineNumber - 8) + ") \n" +
                                "  in List(8, 9, 12) (InspectorsSpec.scala:" + (thisLineNumber - 10) + ") \n" +
                                "in List(List(2, 4, 6), List(8, 9, 12))"))
      e.getCause match {
        case tfe: exceptions.TestFailedException => 
          tfe.failedCodeFileName should be (Some("InspectorsSpec.scala"))
          tfe.failedCodeLineNumber should be (Some(thisLineNumber - 15))
          tfe.message should be (Some("forAll failed, because: \n" +
                                      "  at index 1, 1 was not equal to 0 (InspectorsSpec.scala:" + (thisLineNumber - 16) + ") \n" +
                                      "in List(8, 9, 12)"))
          tfe.getCause match {
            case tfe: exceptions.TestFailedException => 
              tfe.failedCodeFileName should be (Some("InspectorsSpec.scala"))
              tfe.failedCodeLineNumber should be (Some(thisLineNumber - 21))
              tfe.message should be (Some("1 was not equal to 0"))
              tfe.getCause should be (null)
            case other => fail("Expected cause to be TestFailedException, but got: " + other)
          }
        case other => fail("Expected cause to be TestFailedException, but got: " + other)
      }
    }
  }
  
  describe("forAtLeast nested") {
    
    it("should have no problem nesting themselves") {
      val listOfList = 
        List(
          List(2, 3, 6), 
          List(8, 11, 12)
        )
      forAtLeast(2, listOfList) { l => 
        forAtLeast(2, l) { _ % 2 should be (0) }
      }
    }
    
    it("should throw TestFailedException with correct stack depth and error message when nested satisfied assertion does not satisfied the outer") {
      val listOfList = 
        List(
          List(2, 3, 6), 
          List(8, 11, 13)
        )
      val e = intercept[exceptions.TestFailedException] {
        forAtLeast(2, listOfList) { l =>
          forAtLeast(2, l) { 
            _ % 2 should be (0) 
          }
        }
      }
      e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 7))
      e.message should be (Some("forAtLeast(2) failed, because only 1 element satisfied the assertion block: \n" +
                                 "  at index 1, forAtLeast(2) failed, because only 1 element satisfied the assertion block: \n" +
                                 "    at index 1, 1 was not equal to 0 (InspectorsSpec.scala:" + (thisLineNumber - 8) + "), \n" +
                                 "    at index 2, 1 was not equal to 0 (InspectorsSpec.scala:" + (thisLineNumber - 9) + ") \n" + 
                                 "  in List(8, 11, 13) (InspectorsSpec.scala:" + (thisLineNumber - 11) + ") \n" +
                                 "in List(List(2, 3, 6), List(8, 11, 13))"))
      e.getCause should be (null)
    }
  }
  
  describe("forAtMost nested") {
    
    it("should have no problem nesting themselves") {
      val listOfList = 
        List(
          List(2, 3, 6), 
          List(8, 10, 12)
        )
      forAtMost(2, listOfList) { l => 
        forAtMost(2, l) { _ % 2 should be (0) }
      }
    }
    
    it("should throw TestFailedException with correct stack depth and error message when nested satisfied assertion does not satisfied the outer") {
      val listOfList = 
        List(
          List(2, 3, 6), 
          List(8, 11, 13)
        )
      val e = intercept[exceptions.TestFailedException] {
        forAtMost(1, listOfList) { l =>
          forAtMost(2, l) { 
            _ % 2 should be (0) 
          }
        }
      }
      e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 7))
      e.message should be (Some("forAtMost(1) failed, because 2 elements satisfied the assertion block at index 0 and 1 in List(List(2, 3, 6), List(8, 11, 13))"))
      e.getCause should be (null)
    }
    
  }
  
  describe("forExactly nested ") {
    
    it("should have no problem nesting themselves") {
      val listOfList = 
        List(
          List(2, 3, 6), 
          List(8, 10, 12)
        )
      forExactly(1, listOfList) { l => 
        forExactly(3, l) { _ % 2 should be (0) }
      }
    }
    
    it("should throw TestFailedException with correct stack depth and error message when nested assertion does not satisfied the outer") {
      val listOfList = 
        List(
          List(2, 4, 6), 
          List(8, 10, 12)
        )
      val e = intercept[exceptions.TestFailedException] {
        forExactly(1, listOfList) { l => 
          forExactly(3, l) { _ % 2 should be (0) }
        }
      }
      e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
      e.message should be (Some("forExactly(1) failed, because 2 elements satisfied the assertion block at index 0 and 1 in List(List(2, 4, 6), List(8, 10, 12))"))
      e.getCause should be (null)
    }
    
  }
  
  describe("forNo nested") {
    
    it("should have no problem nesting themselves") {
      val listOfList = 
        List(
          List(2, 3, 6), 
          List(8, 9, 12)
        )
      forNo(listOfList) { l => 
        forNo(l) { _ % 2 should be (0) }
      }
    }
    
    it("should throw TestFailedException with correct stack depth and error message when nested assertion does not satisfied the outer") {
      val listOfList = 
        List(
          List(1, 8, 10), 
          List(7, 9, 11)
        )
      val e = intercept[exceptions.TestFailedException] {
        forNo(listOfList) { l => 
          forNo(l) { _ % 2 should be (0) }
        }
      }
      e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
      e.message should be (Some("forNo failed, because 1 element satisfied the assertion block at index 1 in List(List(1, 8, 10), List(7, 9, 11))"))
      e.getCause should be (null)
    }
  }
  
  describe("forBetween nested") {
    
    it("should have no problem nesting themselves") {
      val listOfList = 
        List(
          List(2, 4, 6, 8, 12), 
          List(11, 14, 16, 18, 21), 
          List(22, 24, 26, 28, 30), 
          List(31, 34, 36, 39, 40), 
          List(41, 45, 46, 48, 50)
        )
      forBetween(2, 4, listOfList) { l => 
        forBetween(2, 4, l) { 
          _ % 2 should be (0) 
        }
      }
    }
    
    it("should throw TestFailedException with correct stack depth and error message when nested assertion does not satisfied the outer") {
      val listOfList = 
        List(
          List(3, 5, 6, 8, 12), 
          List(11, 14, 16, 18, 21), 
          List(22, 25, 26, 29, 30), 
          List(31, 34, 36, 39, 40), 
          List(41, 45, 46, 48, 50)
        )
      val e = intercept[exceptions.TestFailedException] {
        forBetween(2, 4, listOfList) { l => 
          forBetween(2, 4, l) { 
            _ % 2 should be (0) 
          }
        }
      }
      e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 7))
      e.message should be (Some("forBetween(2, 4) failed, because 5 elements satisfied the assertion block at index 0, 1, 2, 3 and 4 in " +
                                "List(List(3, 5, 6, 8, 12), List(11, 14, 16, 18, 21), List(22, 25, 26, 29, 30), List(31, 34, 36, 39, 40), " +
                                "List(41, 45, 46, 48, 50))"))
      e.getCause should be (null)
    }
  }
  
  describe("forEvery nested") {
    
    it("should have no problem nesting themselves") {
      val listOfList = 
        List(
          List(2, 4, 6), 
          List(8, 10, 12)
        )
      forEvery(listOfList) { l => 
        forEvery(l) { _ % 2 should be (0) }
      }
    }
    
    it("should throw TestFailedException with correct stack depth and error message when at least one element fails in nested forAll ") {
      val listOfList = 
        List(
          List(2, 4, 6), 
          List(8, 9, 12), 
          List(11, 16, 19)
        )
      val e = intercept[exceptions.TestFailedException] {
        forEvery(listOfList) { l => 
          forEvery(l) { 
            _ % 2 should be (0) 
          }
        }
      }
      e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
      e.failedCodeLineNumber should be (Some(thisLineNumber - 7))
      e.message should be (Some("forEvery failed, because: \n" +
                                "  at index 1, forEvery failed, because: \n" +
                                "    at index 1, 1 was not equal to 0 (InspectorsSpec.scala:" + (thisLineNumber - 8) + ") \n" +
                                "  in List(8, 9, 12) (InspectorsSpec.scala:" + (thisLineNumber - 10) + "), \n" +
                                "  at index 2, forEvery failed, because: \n" + 
                                "    at index 0, 1 was not equal to 0 (InspectorsSpec.scala:" + (thisLineNumber - 11) + "), \n" +
                                "    at index 2, 1 was not equal to 0 (InspectorsSpec.scala:" + (thisLineNumber - 12) + ") \n" + 
                                "  in List(11, 16, 19) (InspectorsSpec.scala:" + (thisLineNumber - 14) + ") \n" +
                                "in List(List(2, 4, 6), List(8, 9, 12), List(11, 16, 19))"))
      e.getCause should be (null)
    }
    
  }

}
