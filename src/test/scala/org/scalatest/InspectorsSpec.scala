package org.scalatest

import org.scalatest.prop.TableDrivenPropertyChecks
import java.lang.annotation.AnnotationFormatError
import java.nio.charset.CoderMalfunctionError
import javax.xml.parsers.FactoryConfigurationError
import javax.xml.transform.TransformerFactoryConfigurationError
import scala.collection.GenTraversable
import scala.annotation.tailrec
import collection._

class InspectorsSpec extends Spec with Matchers with Inspectors with TableDrivenPropertyChecks with SharedHelpers {
  
  def examples =
    Table[Set[Int] => GenTraversable[Int]](
      ("Fun"), 
      ((set: Set[Int]) => set), 
      ((set: Set[Int]) => set.toList), 
      ((set: Set[Int]) => set.toSeq), 
      ((set: Set[Int]) => set.toArray), 
      ((set: Set[Int]) => set.toIndexedSeq), 
      ((set: Set[Int]) => Vector.empty ++ set),
      ((set: Set[Int]) => set.par), 
      ((set: Set[Int]) => set.toList.par), 
      ((set: Set[Int]) => set.toSeq.par), 
      ((set: Set[Int]) => set.toIndexedSeq.par), 
      ((set: Set[Int]) => mutable.Set.empty ++ set), 
      ((set: Set[Int]) => new mutable.ListBuffer() ++ set), 
      ((set: Set[Int]) => mutable.Seq.empty ++ set), 
      ((set: Set[Int]) => mutable.IndexedSeq.empty ++ set), 
      ((set: Set[Int]) => (mutable.Set.empty ++ set).par), 
      ((set: Set[Int]) => (new mutable.ListBuffer() ++ set).par), 
      ((set: Set[Int]) => (mutable.Seq.empty ++ set).par), 
      ((set: Set[Int]) => (mutable.IndexedSeq.empty ++ set).par) 
    )  
  
  object `forAll ` {
    
    def `should pass when all elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forAll(col) { e => e should be < 4 }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when at least one element failed` {
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
                                   "in " + col))
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
    
    def `should throw TestFailedException with correct stack depth and message when more than one element failed` {
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
                                    "in " + col))
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
    
    def `should propagate TestPendingException thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[exceptions.TestPendingException] {
          forAll(col) { e => pending }
        }
      }
    }
    
    def `should propagate TestCanceledException thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[exceptions.TestCanceledException] {
          forAll(col) { e => cancel }
        }  
      }
    }
    
    def `should propagate java.lang.annotation.AnnotationFormatError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[AnnotationFormatError] {
          forAll(col) { e => throw new AnnotationFormatError("test") }
        }
      }
    }
    
    def `should propagate java.nio.charset.CoderMalfunctionError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[CoderMalfunctionError] {
          forAll(col) { e => throw new CoderMalfunctionError(new RuntimeException("test")) }
        }
      }
    }
    
    def `should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[FactoryConfigurationError] {
          forAll(col) { e => throw new FactoryConfigurationError() }
        }
      }
    }
    
    def `should propagate java.lang.LinkageError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[LinkageError] {
          forAll(col) { e => throw new LinkageError() }
        }
      }
    }
    
    def `should propagate java.lang.ThreadDeath thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[ThreadDeath] {
          forAll(col) { e => throw new ThreadDeath() }
        }
      }
    }
    
    def `should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[TransformerFactoryConfigurationError] {
          forAll(col) { e => throw new TransformerFactoryConfigurationError() }
        }
      }
    }
    
    def `should propagate java.lang.VirtualMachineError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[VirtualMachineError] {
          forAll(col) { e => throw new VirtualMachineError() {} }
        }
      }
    }
  }
  
  object `forAtLeast ` {
    
    def `should throw IllegalArgumentException when 0 is passed in as min` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forAtLeast(0, col) { e => e should be (2) }
        }
        e.getMessage should be ("'min' argument must be more than 0")
      }
    }
    
    def `should throw IllegalArgumentException when -1 is passed in as min` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forAtLeast(-1, col) { e => e should be (2) }
        }
        e.getMessage should be ("'min' argument must be more than 0")
      }
    }
    
    def `should pass when minimum count of elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forAtLeast(1, col) { e => e should be (2) }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when less than minimum count of elements passed` {
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
                                   "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'no element' in error message when no element satisfied the assertion block` {
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
                                   "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'element' in error message when exactly 1 element satisfied the assertion block` {
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
                                   "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'elements' in error message when > 1 element satisfied the assertion block` {
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
                                   "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should pass when more than minimum count of elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forAtLeast(1, col) { e => e should be < 3 }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when none of the elements passed` {
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
                                   "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should pass when all of the elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forAtLeast(1, col) { e => e should be < 5 }
      }
    }
    
    def `should propagate TestPendingException thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[exceptions.TestPendingException] {
          forAtLeast(1, col) { e => pending }
        }
      }
    }
    
    def `should propagate TestCanceledException thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[exceptions.TestCanceledException] {
          forAtLeast(1, col) { e => cancel }
        }
      }
    }
    
    def `should propagate java.lang.annotation.AnnotationFormatError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[AnnotationFormatError] {
          forAtLeast(1, col) { e => throw new AnnotationFormatError("test") }
        }
      }
    }
    
    def `should propagate java.nio.charset.CoderMalfunctionError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[CoderMalfunctionError] {
          forAtLeast(1, col) { e => throw new CoderMalfunctionError(new RuntimeException("test")) }
        }
      }
    }
    
    def `should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[FactoryConfigurationError] {
          forAtLeast(1, col) { e => throw new FactoryConfigurationError() }
        }
      }
    }
    
    def `should propagate java.lang.LinkageError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[LinkageError] {
          forAtLeast(1, col) { e => throw new LinkageError() }
        }
      }
    }
    
    def `should propagate java.lang.ThreadDeath thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[ThreadDeath] {
          forAtLeast(1, col) { e => throw new ThreadDeath() }
        }
      }
    }
    
    def `should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[TransformerFactoryConfigurationError] {
          forAtLeast(1, col) { e => throw new TransformerFactoryConfigurationError() }
        }
      }
    }
    
    def `should propagate java.lang.VirtualMachineError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[VirtualMachineError] {
          forAtLeast(1, col) { e => throw new VirtualMachineError() {} }
        }
      }
    }
  }

  object `forAtMost ` {
    
    def `should throw IllegalArgumentException when 0 is passed in as max` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forAtMost(0, col) { e => e should be (2) }
        }
        e.getMessage should be ("'max' argument must be more than 0")
      }
    }
    
    def `should throw IllegalArgumentException when -1 is passed in as max` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forAtMost(-1, col) { e => e should be (2) }
        }
        e.getMessage should be ("'max' argument must be more than 0")
      }
    }
    
    def `should pass when number of elements passed is less than maximum allowed` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forAtMost(2, col) { e => e should be (2) }
      }
    }
    
    def `should pass when number of elements passed equal to maximum allowed` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forAtMost(2, col) { e => e should be < 3 }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when number of element passed is more than maximum allowed` {
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
        e.message should be (Some("forAtMost(2) failed, because 3 elements satisfied the assertion block at index " + firstIndex + ", " + secondIndex + " and " + thirdIndex + " in " + col))
      }
    }
    
    def `should pass when none of the elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forAtMost(2, col) { e => e should be > 5 }
      }
    }
    
    def `should propagate TestPendingException thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[exceptions.TestPendingException] {
          forAtMost(1, col) { e => pending }
        }
      }
    }
    
    def `should propagate TestCanceledException thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[exceptions.TestCanceledException] {
          forAtMost(1, col) { e => cancel }
        }
      }
    }
    
    def `should propagate java.lang.annotation.AnnotationFormatError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[AnnotationFormatError] {
          forAtMost(1, col) { e => throw new AnnotationFormatError("test") }
        }
      }
    }
    
    def `should propagate java.nio.charset.CoderMalfunctionError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[CoderMalfunctionError] {
          forAtMost(1, col) { e => throw new CoderMalfunctionError(new RuntimeException("test")) }
        }
      }
    }
    
    def `should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[FactoryConfigurationError] {
          forAtMost(1, col) { e => throw new FactoryConfigurationError() }
        }
      }
    }
    
    def `should propagate java.lang.LinkageError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[LinkageError] {
          forAtMost(1, col) { e => throw new LinkageError() }
        }
      }
    }
    
    def `should propagate java.lang.ThreadDeath thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[ThreadDeath] {
          forAtMost(1, col) { e => throw new ThreadDeath() }
        }
      }
    }
    
    def `should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[TransformerFactoryConfigurationError] {
          forAtMost(1, col) { e => throw new TransformerFactoryConfigurationError() }
        }
      }
    }
    
    def `should propagate java.lang.VirtualMachineError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[VirtualMachineError] {
          forAtMost(1, col) { e => throw new VirtualMachineError() {} }
        }
      }
    }
  }
  
  object `forExactly ` {
    
    def `should throw IllegalArgumentException when 0 is passed in as max` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forExactly(0, col) { e => e should be (2) }
        }
        e.getMessage should be ("'succeededCount' argument must be more than 0")
      }
    }
    
    def `should throw IllegalArgumentException when -1 is passed in as max` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forExactly(-1, col) { e => e should be (2) }
        }
        e.getMessage should be ("'succeededCount' argument must be more than 0")
      }
    }
    
    def `should pass when number of element passes is equal to specified succeeded count` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forExactly(2, col) { e => e should be < 3 }
      }
    }
    
    def `should use 'no element' in error message when no element satisfied the assertion block` {
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
                                  "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'element' in error message when exactly 1 element satisfied the assertion block, when passed count is less than the expected count` {
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
                                  "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'element' in error message when exactly 1 element satisfied the assertion block, when passed count is more than the expected count` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forExactly(2, col) { e => 
            e should be < 5
          }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        e.message should be (Some("forExactly(2) failed, because 3 elements satisfied the assertion block at index 0, 1 and 2 in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'elements' in error message when > 1 element satisfied the assertion block, when passed count is less than the expected count` {
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
                                  "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'elements' in error message when > 1 element satisfied the assertion block, when passed count is more than the expected count` {
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
        e.message should be (Some("forExactly(1) failed, because 2 elements satisfied the assertion block at index " + firstIndex + " and " + secondIndex + " in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when number of element passed is less than specified succeeded count` {
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
                                  "in " + col))
      }
    }
    
    def `should throw TestFailedException with correct stack depth and messsage when number of element passed is more than specified succeeded count` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forExactly(2, col) { e => e should be < 5 }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some("forExactly(2) failed, because 3 elements satisfied the assertion block at index 0, 1 and 2 in " + col))
      }
    }
    
    def `should propagate TestPendingException thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[exceptions.TestPendingException] {
          forExactly(1, col) { e => pending }
        }
      }
    }
    
    def `should propagate TestCanceledException thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[exceptions.TestCanceledException] {
          forExactly(1, col) { e => cancel }
        }
      }
    }
    
    def `should propagate java.lang.annotation.AnnotationFormatError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[AnnotationFormatError] {
          forExactly(1, col) { e => throw new AnnotationFormatError("test") }
        }
      }
    }
    
    def `should propagate java.nio.charset.CoderMalfunctionError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[CoderMalfunctionError] {
          forExactly(1, col) { e => throw new CoderMalfunctionError(new RuntimeException("test")) }
        }
      }
    }
    
    def `should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[FactoryConfigurationError] {
          forExactly(1, col) { e => throw new FactoryConfigurationError() }
        }
      }
    }
    
    def `should propagate java.lang.LinkageError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[LinkageError] {
          forExactly(1, col) { e => throw new LinkageError() }
        }
      }
    }
    
    def `should propagate java.lang.ThreadDeath thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[ThreadDeath] {
          forExactly(1, col) { e => throw new ThreadDeath() }
        }
      }
    }
    
    def `should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[TransformerFactoryConfigurationError] {
          forExactly(1, col) { e => throw new TransformerFactoryConfigurationError() }
        }
      }
    }
    
    def `should propagate java.lang.VirtualMachineError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[VirtualMachineError] {
          forExactly(1, col) { e => throw new VirtualMachineError() {} }
        }
      }
    }
  }
  
  object `forNo ` {
    
    def `should pass when none of the element pass` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forNo(col) { e => e should be > 5 }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 1 element passed` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forNo(col) { e => e should equal (2) }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val index = getIndex(col, 2)
        e.message should be (Some("forNo failed, because 1 element satisfied the assertion block at index " + index + " in " + col))
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 2 element passed` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forNo(col) { e => e should be < 3 }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val first = col.toIterator.next
        e.message should be (Some("forNo failed, because 1 element satisfied the assertion block at index 0 in " + col))
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when all elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[exceptions.TestFailedException] {
          forNo(col) { e => e should be < 5 }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some("forNo failed, because 1 element satisfied the assertion block at index 0 in " + col))
      }
    }
    
    def `should pass when empty list of element is passed in` {
      forAll(examples) { colFun =>
        val col = colFun(Set.empty[Int])
        forNo(col) { e => e should be < 5 }
      }
    }
    
    def `should propagate TestPendingException thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[exceptions.TestPendingException] {
          forNo(col) { e => pending }
        }
      }
    }
    
    def `should propagate TestCanceledException thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[exceptions.TestCanceledException] {
          forNo(col) { e => cancel }
        }
      }
    }
    
    def `should propagate java.lang.annotation.AnnotationFormatError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[AnnotationFormatError] {
          forNo(col) { e => throw new AnnotationFormatError("test") }
        }
      }
    }
    
    def `should propagate java.nio.charset.CoderMalfunctionError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[CoderMalfunctionError] {
          forNo(col) { e => throw new CoderMalfunctionError(new RuntimeException("test")) }
        }
      }
    }
    
    def `should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[FactoryConfigurationError] {
          forNo(col) { e => throw new FactoryConfigurationError() }
        }
      }
    }
    
    def `should propagate java.lang.LinkageError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[LinkageError] {
          forNo(col) { e => throw new LinkageError() }
        }
      }
    }
    
    def `should propagate java.lang.ThreadDeath thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[ThreadDeath] {
          forNo(col) { e => throw new ThreadDeath() }
        }
      }
    }
    
    def `should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[TransformerFactoryConfigurationError] {
          forNo(col) { e => throw new TransformerFactoryConfigurationError() }
        }
      }
    }
    
    def `should propagate java.lang.VirtualMachineError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[VirtualMachineError] {
          forNo(col) { e => throw new VirtualMachineError() {} }
        }
      }
    }
  }
  
  object `forBetween ` {
    
    def `should throw IllegalArgumentException when -1 is passed in as from` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forBetween(-1, 2, col) { e => e should be (2) }
        }
        e.getMessage should be ("'from' argument must be more than or equal 0")
      }
    }
    
    def `should throw IllegalArgumentException when 0 is passed in as upTo` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forBetween(0, 0, col) { e => e should be (2) }
        }
        e.getMessage should be ("'upTo' argument must be more than 0")
      }
    }
    
    def `should throw IllegalArgumentException when -1 is passed in as upTo` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forBetween(0, -1, col) { e => e should be (2) }
        }
        e.getMessage should be ("'upTo' argument must be more than 0")
      }
    }
    
    def `should throw IllegalArgumentException when from and upTo is the same` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forBetween(1, 1, col) { e => e should be (2) }
        }
        e.getMessage should be ("'upTo' argument must be more than 'from' argument")
      }
    }
    
    def `should throw IllegalArgumentException when from is greater than upTo` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        val e = intercept[IllegalArgumentException] {
          forBetween(3, 2, col) { e => e should be (2) }
        }
        e.getMessage should be ("'upTo' argument must be more than 'from' argument")
      }
    }
    
    def `should pass when number of element passed is within the specified range` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        forBetween(2, 4, col) { e => e should be > 2 }
      }
    }
    
    def `should pass when number of element passed is same as lower bound of the specified range` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        forBetween(2, 4, col) { e => e should be > 3 }
      }
    }
    
    def `should pass when number of element passed is same as upper bound of the specified range` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        forBetween(2, 4, col) { e => e should be > 1 }
      }
    }
    
    def `should use 'no element' in error message when no element satisfied the assertion block and 'from' is > 0 ` {
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
                                  "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'element' in error message when exactly 1 element satisfied the assertion block, when total passed is less than 'from'` {
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
                                  "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'elements' in error message when > 1 element satisfied the assertion block, when total passed is less than 'from'` {
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
                                  "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'elements' in error message when > 1 element satisfied the assertion block, when total passed is more than 'upTo'` {
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
        e.message should be (Some("forBetween(2, 3) failed, because 4 elements satisfied the assertion block at index " + firstIndex + ", " + secondIndex + ", " + thirdIndex + " and " + forthIndex + " in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when number of element passed is less than lower bound of the specified range` {
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
                                  "in " + col))
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when number of element passed is more than upper bound of the specified range` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        val e = intercept[exceptions.TestFailedException] {
          forBetween(2, 4, col) { e => e should be > 0 }
        }
        e.failedCodeFileName should be (Some("InspectorsSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some("forBetween(2, 4) failed, because 5 elements satisfied the assertion block at index 0, 1, 2, 3 and 4 in " + col))
      }
    }
    
    def `should propagate TestPendingException thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        intercept[exceptions.TestPendingException] {
          forBetween(2, 4, col) { e => pending }
        }
      }
    }
    
    def `should propagate TestCanceledException thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        intercept[exceptions.TestCanceledException] {
          forBetween(2, 4, col) { e => cancel }
        }
      }
    }
    
    def `should propagate java.lang.annotation.AnnotationFormatError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        intercept[AnnotationFormatError] {
          forBetween(2, 4, col) { e => throw new AnnotationFormatError("test") }
        }
      }
    }
    
    def `should propagate java.nio.charset.CoderMalfunctionError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        intercept[CoderMalfunctionError] {
          forBetween(2, 4, col) { e => throw new CoderMalfunctionError(new RuntimeException("test")) }
        }
      }
    }
    
    def `should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        intercept[FactoryConfigurationError] {
          forBetween(2, 4, col) { e => throw new FactoryConfigurationError() }
        }
      }
    }
    
    def `should propagate java.lang.LinkageError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        intercept[LinkageError] {
          forBetween(2, 4, col) { e => throw new LinkageError() }
        }
      }
    }
    
    def `should propagate java.lang.ThreadDeath thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        intercept[ThreadDeath] {
          forBetween(2, 4, col) { e => throw new ThreadDeath() }
        }
      }
    }
    
    def `should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        intercept[TransformerFactoryConfigurationError] {
          forBetween(2, 4, col) { e => throw new TransformerFactoryConfigurationError() }
        }
      }
    }
    
    def `should propagate java.lang.VirtualMachineError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3, 4, 5))
        intercept[VirtualMachineError] {
          forBetween(2, 4, col) { e => throw new VirtualMachineError() {} }
        }
      }
    }
  }
  
  object `forEvery ` {
    
    def `should pass when all elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        forEvery(col) { e => e should be < 4 }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when at least one element failed` {
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
                                  "in " + col))
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when more than one element failed` {
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
                                  "in " + col))
      }
    }
    
    def `should propagate TestPendingException thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[exceptions.TestPendingException] {
          forEvery(col) { e => pending }
        }
      }
    }
    
    def `should propagate TestCanceledException thrown from assertion` {
      forAll(examples) { colFun =>
        val colFun = Set(1, 2, 3)
        intercept[exceptions.TestCanceledException] {
          forEvery(colFun) { e => cancel }
        }
      }
    }
    
    def `should propagate java.lang.annotation.AnnotationFormatError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[AnnotationFormatError] {
          forEvery(col) { e => throw new AnnotationFormatError("test") }
        }
      }
    }
    
    def `should propagate java.nio.charset.CoderMalfunctionError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[CoderMalfunctionError] {
          forEvery(col) { e => throw new CoderMalfunctionError(new RuntimeException("test")) }
        }
      }
    }
    
    def `should propagate javax.xml.parsers.FactoryConfigurationError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[FactoryConfigurationError] {
          forEvery(col) { e => throw new FactoryConfigurationError() }
        }
      }
    }
    
    def `should propagate java.lang.LinkageError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[LinkageError] {
          forEvery(col) { e => throw new LinkageError() }
        }
      }
    }
    
    def `should propagate java.lang.ThreadDeath thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[ThreadDeath] {
          forEvery(col) { e => throw new ThreadDeath() }
        }
      }
    }
    
    def `should propagate javax.xml.transform.TransformerFactoryConfigurationError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[TransformerFactoryConfigurationError] {
          forEvery(col) { e => throw new TransformerFactoryConfigurationError() }
        }
      }
    }
    
    def `should propagate java.lang.VirtualMachineError thrown from assertion` {
      forAll(examples) { colFun =>
        val col = colFun(Set(1, 2, 3))
        intercept[VirtualMachineError] {
          forEvery(col) { e => throw new VirtualMachineError() {} }
        }
      }
    }
  }
  
  object `forAll nested` {
    
    def `should have no problem nesting themselves` {
      val listOfList = 
        List(
          List(2, 4, 6), 
          List(8, 10, 12)
        )
      forAll(listOfList) { l => 
        forAll(l) { _ % 2 should be (0) }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and error message when at least one element fails in nested forAll ` {
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
  
  object `forAtLeast nested` {
    
    def `should have no problem nesting themselves` {
      val listOfList = 
        List(
          List(2, 3, 6), 
          List(8, 11, 12)
        )
      forAtLeast(2, listOfList) { l => 
        forAtLeast(2, l) { _ % 2 should be (0) }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and error message when nested satisfied assertion does not satisfied the outer` {
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
  
  object `forAtMost nested` {
    
    def `should have no problem nesting themselves` {
      val listOfList = 
        List(
          List(2, 3, 6), 
          List(8, 10, 12)
        )
      forAtMost(2, listOfList) { l => 
        forAtMost(2, l) { _ % 2 should be (0) }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and error message when nested satisfied assertion does not satisfied the outer` {
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
  
  object `forExactly nested ` {
    
    def `should have no problem nesting themselves` {
      val listOfList = 
        List(
          List(2, 3, 6), 
          List(8, 10, 12)
        )
      forExactly(1, listOfList) { l => 
        forExactly(3, l) { _ % 2 should be (0) }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and error message when nested assertion does not satisfied the outer` {
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
  
  object `forNo nested` {
    
    def `should have no problem nesting themselves` {
      val listOfList = 
        List(
          List(2, 3, 6), 
          List(8, 9, 12)
        )
      forNo(listOfList) { l => 
        forNo(l) { _ % 2 should be (0) }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and error message when nested assertion does not satisfied the outer` {
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
  
  object `forBetween nested` {
    
    def `should have no problem nesting themselves` {
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
    
    def `should throw TestFailedException with correct stack depth and error message when nested assertion does not satisfied the outer` {
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
  
  object `forEvery nested` {
    
    def `should have no problem nesting themselves` {
      val listOfList = 
        List(
          List(2, 4, 6), 
          List(8, 10, 12)
        )
      forEvery(listOfList) { l => 
        forEvery(l) { _ % 2 should be (0) }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and error message when at least one element fails in nested forAll ` {
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
