package org.scalatest

import org.scalatest.prop.TableDrivenPropertyChecks
import java.lang.annotation.AnnotationFormatError
import java.nio.charset.CoderMalfunctionError
import javax.xml.parsers.FactoryConfigurationError
import javax.xml.transform.TransformerFactoryConfigurationError
import scala.collection.GenTraversable
import scala.annotation.tailrec
import collection._

class InspectorsForMapSpec extends Spec with Matchers with Inspectors with TableDrivenPropertyChecks with SharedHelpers {

  def examples =
    Table[Map[Int, String] => collection.GenMap[Int, String]](
      ("Fun"), 
      ((map: Map[Int, String]) => map), 
      ((map: Map[Int, String]) => mutable.Map.empty ++ map), 
      ((map: Map[Int, String]) => map.par), 
      ((map: Map[Int, String]) => (mutable.Map.empty ++ map).par)
    )
  
  val standardMap = 
    Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five")
  
  object `forAll ` {
    
    def `should pass when all elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        forAll(col) { e => e._2 should be (standardMap(e._1)) }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when at least one element failed` {
      forAll(examples) { colFun => 
        val col = colFun(Map(1 -> "one", 2 -> "dua", 3 -> "three"))
        val e = intercept[exceptions.TestFailedException] {
          forAll(col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        e.message should be (Some("forAll failed, because: \n" +
                                   "  at key 2, \"[dua]\" did not equal \"[two]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                   "in " + col))
        e.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"[dua]\" did not equal \"[two]\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when more than one element failed` {
      forAll(examples) { colFun => 
        val col = colFun(Map(1 -> "one", 2 -> "dua", 3 -> "three"))
        val e2 = intercept[exceptions.TestFailedException] {
          forAll(col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        e2.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e2.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        e2.message should be (Some("forAll failed, because: \n" +
                                    "  at key 2, \"[dua]\" did not equal \"[two]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                    "in " + col))
        e2.getCause match {
          case tfe: exceptions.TestFailedException =>
            tfe.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
            tfe.failedCodeLineNumber should be (Some(thisLineNumber - 11))
            tfe.message should be (Some("\"[dua]\" did not equal \"[two]\""))
            tfe.getCause should be (null)
          case other => fail("Expected cause to be TestFailedException, but got: " + other)
        }
      }
    }
  }
  
  object `forAtLeast ` {
    
    def `should throw IllegalArgumentException when 0 is passed in as min` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        val e = intercept[IllegalArgumentException] {
          forAtLeast(0, col) { e => e._2 should equal (standardMap(e._1)) }
        }
        e.getMessage should be ("'min' argument must be more than 0")
      }
    }
    
    def `should throw IllegalArgumentException when -1 is passed in as min` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        val e = intercept[IllegalArgumentException] {
          forAtLeast(-1, col) { e => e._2 should equal (standardMap(e._1)) }
        }
        e.getMessage should be ("'min' argument must be more than 0")
      }
    }
    
    def `should pass when minimum count of elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "two", 3 -> "tiga"))
        forAtLeast(1, col) { e => e._2 should equal (standardMap(e._1)) }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when less than minimum count of elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "dua", 3 -> "three"))
        val e = intercept[exceptions.TestFailedException] {
          forAtLeast(2, col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val predicate = (e: Tuple2[Int, String]) => e._2 != standardMap(e._1)
        val first = getNext(itr, predicate)._1
        val firstValue = col(first)
        val firstExpectedValue = standardMap(first)
        val second = getNext(itr, predicate)._1
        val secondValue = col(second)
        val secondExpectedValue = standardMap(second)
        e.message should be (Some("forAtLeast(2) failed, because only 1 element satisfied the assertion block: \n" +
                                   "  at key " + first + ", \"[" + firstValue + "]\" did not equal \"[" + firstExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 14) + "), \n" +
                                   "  at key " + second + ", \"[" + secondValue + "]\" did not equal \"[" + secondExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 15) + ") \n" +
                                   "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'no element' in error message when no element satisfied the assertion block` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "dua", 3 -> "Tiga"))
        val e = intercept[exceptions.TestFailedException] {
          forAtLeast(2, col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val first = itr.next._1
        val firstValue = col(first)
        val firstExpectedValue = standardMap(first)
        val second = itr.next._1
        val secondValue = col(second)
        val secondExpectedValue = standardMap(second)
        val third = itr.next._1
        val thirdValue = col(third)
        val thirdExpectedValue = standardMap(third)
        e.message should be (Some("forAtLeast(2) failed, because no element satisfied the assertion block: \n" +
                                   "  at key " + first + ", \"[" + firstValue + "]\" did not equal \"[" + firstExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 16) + "), \n" +
                                   "  at key " + second + ", \"[" + secondValue + "]\" did not equal \"[" + secondExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 17) + "), \n" +
                                   "  at key " + third + ", \"[" + thirdValue + "]\" did not equal \"[" + thirdExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 18) + ") \n" +
                                   "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'element' in error message when exactly 1 element satisfied the assertion block` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "dua", 3 -> "three"))
        val e = intercept[exceptions.TestFailedException] {
          forAtLeast(2, col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val predicate = (e: Tuple2[Int, String]) => e._2 != standardMap(e._1)
        val first = getNext(itr, predicate)._1
        val firstValue = col(first)
        val firstExpectedValue = standardMap(first)
        val second = getNext(itr, predicate)._1
        val secondValue = col(second)
        val secondExpectedValue = standardMap(second)
        e.message should be (Some("forAtLeast(2) failed, because only 1 element satisfied the assertion block: \n" +
                                   "  at key " + first + ", \"[" + firstValue + "]\" did not equal \"[" + firstExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 14) + "), \n" +
                                   "  at key " + second + ", \"[" + secondValue + "]\" did not equal \"[" + secondExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 15) + ") \n" +
                                   "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'elements' in error message when > 1 element satisfied the assertion block` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "dua", 3 -> "three"))
        val e = intercept[exceptions.TestFailedException] {
          forAtLeast(3, col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        e.message should be (Some("forAtLeast(3) failed, because only 2 elements satisfied the assertion block: \n" +
                                   "  at key 2" + ", \"[dua]\" did not equal \"[two]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                   "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should pass when more than minimum count of elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        forAtLeast(1, col) { e => e._2 should equal (standardMap(e._1)) }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when none of the elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "dua", 3 -> "Tiga"))
        val e = intercept[exceptions.TestFailedException] {
          forAtLeast(1, col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val first = itr.next._1
        val firstValue = col(first)
        val firstExpectedValue = standardMap(first)
        val second = itr.next._1
        val secondValue = col(second)
        val secondExpectedValue = standardMap(second)
        val third = itr.next._1
        val thirdValue = col(third)
        val thirdExpectedValue = standardMap(third)
        e.message should be (Some("forAtLeast(1) failed, because no element satisfied the assertion block: \n" +
                                   "  at key " + first + ", \"[" + firstValue + "]\" did not equal \"[" + firstExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 16) + "), \n" +
                                   "  at key " + second + ", \"[" + secondValue + "]\" did not equal \"[" + secondExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 17) + "), \n" +
                                   "  at key " + third + ", \"[" + thirdValue + "]\" did not equal \"[" + thirdExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 18) + ") \n" +
                                   "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should pass when all of the elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        forAtLeast(1, col) { e => e._2 should equal (standardMap(e._1)) }
      }
    }
    
  }
  
  object `forAtMost ` {
    
    def `should throw IllegalArgumentException when 0 is passed in as max` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        val e = intercept[IllegalArgumentException] {
          forAtMost(0, col) { e => e._2 should equal (standardMap(e._1)) }
        }
        e.getMessage should be ("'max' argument must be more than 0")
      }
    }
    
    def `should throw IllegalArgumentException when -1 is passed in as max` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        val e = intercept[IllegalArgumentException] {
          forAtMost(-1, col) { e => e._2 should equal (standardMap(e._1)) }
        }
        e.getMessage should be ("'max' argument must be more than 0")
      }
    }
    
    def `should pass when number of elements passed is less than maximum allowed` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "two", 3 -> "tiga"))
        forAtMost(2, col) { e => e._2 should equal (standardMap(e._1)) }
      }
    }
    
    def `should pass when number of elements passed equal to maximum allowed` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "tiga"))
        forAtMost(2, col) { e => e._2 should equal (standardMap(e._1)) }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when number of element passed is more than maximum allowed` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "tiga", 4 -> "four", 5 -> "five"))
        val e = intercept[exceptions.TestFailedException] {
          forAtMost(2, col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val predicate = (e: Tuple2[Int, String]) => e._2 == standardMap(e._1)
        val first = getNext(itr, predicate)._1
        val second = getNext(itr, predicate)._1
        val third = getNext(itr, predicate)._1
        e.message should be (Some("forAtMost(2) failed, because 3 elements satisfied the assertion block at key " + first + ", " + second + " and " + third + " in " + col))
      }
    }
    
    def `should pass when none of the elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "dua", 3 -> "tiga"))
        forAtMost(2, col) { e => e._2 should equal (standardMap(e._1)) }
      }
    }
    
  }
  
  object `forExactly ` {
    
    def `should throw IllegalArgumentException when 0 is passed in as max` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        val e = intercept[IllegalArgumentException] {
          forExactly(0, col) { e => e._2 should equal (standardMap(e._1)) }
        }
        e.getMessage should be ("'succeededCount' argument must be more than 0")
      }
    }
    
    def `should throw IllegalArgumentException when -1 is passed in as max` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        val e = intercept[IllegalArgumentException] {
          forExactly(-1, col) { e => e._2 should equal (standardMap(e._1)) }
        }
        e.getMessage should be ("'succeededCount' argument must be more than 0")
      }
    }
    
    def `should pass when number of element passes is equal to specified succeeded count` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "tiga"))
        forExactly(2, col) { e => e._2 should equal (standardMap(e._1)) }
      }
    }
    
    def `should use 'no element' in error message when no element satisfied the assertion block` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "dua", 3 -> "Tiga"))
        val e = intercept[exceptions.TestFailedException] {
          forExactly(2, col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        val itr = col.toIterator
        val first = itr.next._1
        val firstValue = col(first)
        val firstExpectedValue = standardMap(first)
        val second = itr.next._1
        val secondValue = col(second)
        val secondExpectedValue = standardMap(second)
        val third = itr.next._1
        val thirdValue = col(third)
        val thirdExpectedValue = standardMap(third)
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 15))
        e.message should be (Some("forExactly(2) failed, because no element satisfied the assertion block: \n" +
                                  "  at key " + first + ", \"[" + firstValue + "]\" did not equal \"[" + firstExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 16) + "), \n" +
                                  "  at key " + second + ", \"[" + secondValue + "]\" did not equal \"[" + secondExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 17) + "), \n" +
                                  "  at key " + third + ", \"[" + thirdValue + "]\" did not equal \"[" + thirdExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 18) + ") \n" +
                                  "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'element' in error message when exactly 1 element satisfied the assertion block, when passed count is less than the expected count` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "dua", 3 -> "three"))
        val e = intercept[exceptions.TestFailedException] {
          forExactly(2, col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        val itr = col.toIterator
        val predicate = (e: Tuple2[Int, String]) => e._2 != standardMap(e._1)
        val first = getNext(itr, predicate)._1
        val firstValue = col(first)
        val firstExpectedValue = standardMap(first)
        val second = getNext(itr, predicate)._1
        val secondValue = col(second)
        val secondExpectedValue = standardMap(second)
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 13))
        e.message should be (Some("forExactly(2) failed, because only 1 element satisfied the assertion block at key 3: \n" +
                                  "  at key " + first + ", \"[" + firstValue + "]\" did not equal \"[" + firstExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 14) + "), \n" +
                                  "  at key " + second + ", \"[" + secondValue + "]\" did not equal \"[" + secondExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 15) + ") \n" +
                                  "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'element' in error message when exactly 1 element satisfied the assertion block, when passed count is more than the expected count` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        val e = intercept[exceptions.TestFailedException] {
          forExactly(2, col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.iterator
        val first = itr.next._1
        val second = itr.next._1
        val third = itr.next._1
        e.message should be (Some("forExactly(2) failed, because 3 elements satisfied the assertion block at key " + first + ", " + second + " and " + third + " in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'elements' in error message when > 1 element satisfied the assertion block, when passed count is less than the expected count` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "dua", 3 -> "three"))
        val e = intercept[exceptions.TestFailedException] {
          forExactly(3, col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val predicate = (e: Tuple2[Int, String]) => e._2 == standardMap(e._1)
        val first = getNext(itr, predicate)._1
        val second = getNext(itr, predicate)._1
        e.message should be (Some("forExactly(3) failed, because only 2 elements satisfied the assertion block at key " + first + " and " + second + ": \n" +
                                  "  at key " + 2 + ", \"[dua]\" did not equal \"[two]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 10) + ") \n" +
                                  "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'elements' in error message when > 1 element satisfied the assertion block, when passed count is more than the expected count` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "dua", 3 -> "three"))
        val e = intercept[exceptions.TestFailedException] {
          forExactly(1, col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val predicate = (e: Tuple2[Int, String]) => e._2 == standardMap(e._1)
        val first = getNext(itr, predicate)._1
        val second = getNext(itr, predicate)._1
        e.message should be (Some("forExactly(1) failed, because 2 elements satisfied the assertion block at key " + first + " and " + second + " in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when number of element passed is less than specified succeeded count` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "dua", 3 -> "three"))
        val e = intercept[exceptions.TestFailedException] {
          forExactly(2, col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val predicate = (e: Tuple2[Int, String]) => e._2 != standardMap(e._1)
        val first = getNext(itr, predicate)._1
        val firstValue = col(first)
        val firstExpectedValue = standardMap(first)
        val second = getNext(itr, predicate)._1
        val secondValue = col(second)
        val secondExpectedValue = standardMap(second)
        e.message should be (Some("forExactly(2) failed, because only 1 element satisfied the assertion block at key 3: \n" +
                                  "  at key " + first + ", \"[" + firstValue + "]\" did not equal \"[" + firstExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 14) + "), \n" +
                                  "  at key " + second + ", \"[" + secondValue + "]\" did not equal \"[" + secondExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 15) + ") \n" +
                                  "in " + col))
      }
    }
    
    def `should throw TestFailedException with correct stack depth and messsage when number of element passed is more than specified succeeded count` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        val e = intercept[exceptions.TestFailedException] {
          forExactly(2, col) { e => e._2 should equal (standardMap(e._1)) }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        val itr = col.toIterator
        val predicate = (e: Tuple2[Int, String]) => e._2 == standardMap(e._1)
        val first = getNext(itr, predicate)._1
        val second = getNext(itr, predicate)._1
        val third = getNext(itr, predicate)._1
        e.message should be (Some("forExactly(2) failed, because 3 elements satisfied the assertion block at key " + first + ", " + second + " and " + third + " in " + col))
      }
    }
    
  }
  
  object `forNo ` {
    
    def `should pass when none of the element pass` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "dua", 3 -> "tiga"))
        forNo(col) { e => e._2 should equal (standardMap(e._1)) }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 1 element passed` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "two", 3 -> "tiga"))
        val e = intercept[exceptions.TestFailedException] {
          forNo(col) { e => 
            e._2 should equal (standardMap(e._1)) 
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        e.message should be (Some("forNo failed, because 1 element satisfied the assertion block at key 2 in " + col))
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when 2 element passed` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "two", 3 -> "three"))
        val e = intercept[exceptions.TestFailedException] {
          forNo(col) { e => 
            e._2 should equal (standardMap(e._1)) 
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val predicate = (e: Tuple2[Int, String]) => e._2 == standardMap(e._1)
        val first = getNext(itr, predicate)._1
        e.message should be (Some("forNo failed, because 1 element satisfied the assertion block at key " + first + " in " + col))
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when all elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        val e = intercept[exceptions.TestFailedException] {
          forNo(col) { e => 
            e._2 should equal (standardMap(e._1)) 
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val predicate = (e: Tuple2[Int, String]) => e._2 == standardMap(e._1)
        val first = getNext(itr, predicate)._1
        e.message should be (Some("forNo failed, because 1 element satisfied the assertion block at key " + first + " in " + col))
      }
    }
    
    def `should pass when empty list of element is passed in` {
      forAll(examples) { colFun =>
        val col = colFun(Map.empty[Int, String])
        forNo(col) { e => e._2 should equal (standardMap(e._1)) }
      }
    }
    
  }
  
  object `forBetween ` {
    
    def `should throw IllegalArgumentException when -1 is passed in as from` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        val e = intercept[IllegalArgumentException] {
          forBetween(-1, 2, col) { e => e._2 should equal (standardMap(e._1)) }
        }
        e.getMessage should be ("'from' argument must be more than or equal 0")
      }
    }
    
    def `should throw IllegalArgumentException when 0 is passed in as upTo` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        val e = intercept[IllegalArgumentException] {
          forBetween(0, 0, col) { e => e._2 should equal (standardMap(e._1)) }
        }
        e.getMessage should be ("'upTo' argument must be more than 0")
      }
    }
    
    def `should throw IllegalArgumentException when -1 is passed in as upTo` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        val e = intercept[IllegalArgumentException] {
          forBetween(0, -1, col) { e => e._2 should equal (standardMap(e._1)) }
        }
        e.getMessage should be ("'upTo' argument must be more than 0")
      }
    }
    
    def `should throw IllegalArgumentException when from and upTo is the same` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        val e = intercept[IllegalArgumentException] {
          forBetween(1, 1, col) { e => e._2 should equal (standardMap(e._1)) }
        }
        e.getMessage should be ("'upTo' argument must be more than 'from' argument")
      }
    }
    
    def `should throw IllegalArgumentException when from is greater than upTo` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        val e = intercept[IllegalArgumentException] {
          forBetween(3, 2, col) { e => e._2 should equal (standardMap(e._1)) }
        }
        e.getMessage should be ("'upTo' argument must be more than 'from' argument")
      }
    }
    
    def `should pass when number of element passed is within the specified range` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "two", 3 -> "tiga", 4 -> "four", 5 -> "five"))
        forBetween(2, 4, col) { e => e._2 should equal (standardMap(e._1)) }
      }
    }
    
    def `should pass when number of element passed is same as lower bound of the specified range` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "two", 3 -> "tiga", 4 -> "four", 5 -> "lima"))
        forBetween(2, 4, col) { e => e._2 should equal (standardMap(e._1)) }
      }
    }
    
    def `should pass when number of element passed is same as upper bound of the specified range` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "tiga", 4 -> "four", 5 -> "five"))
        forBetween(2, 4, col) { e => e._2 should equal (standardMap(e._1)) }
      }
    }
    
    def `should use 'no element' in error message when no element satisfied the assertion block and 'from' is > 0 ` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "dua", 3 -> "Tiga"))
        val e = intercept[exceptions.TestFailedException] {
          forBetween(1, 2, col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val first = itr.next._1
        val firstValue = col(first)
        val firstExpectedValue = standardMap(first)
        val second = itr.next._1
        val secondValue = col(second)
        val secondExpectedValue = standardMap(second)
        val third = itr.next._1
        val thirdValue = col(third)
        val thirdExpectedValue = standardMap(third)
        e.message should be (Some("forBetween(1, 2) failed, because no element satisfied the assertion block: \n" +
                                  "  at key " + first + ", \"[" + firstValue + "]\" did not equal \"[" + firstExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 16) + "), \n" +
                                  "  at key " + second + ", \"[" + secondValue + "]\" did not equal \"[" + secondExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 17) + "), \n" +
                                  "  at key " + third + ", \"[" + thirdValue + "]\" did not equal \"[" + thirdExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 18) + ") \n" +
                                  "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'element' in error message when exactly 1 element satisfied the assertion block, when total passed is less than 'from'` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "two", 3 -> "Tiga"))
        val e = intercept[exceptions.TestFailedException] {
          forBetween(2, 3, col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val predicate = (e: Tuple2[Int, String]) => e._2 != standardMap(e._1)
        val first = getNext(itr, predicate)._1
        val firstValue = col(first)
        val firstExpectedValue = standardMap(first)
        val second = getNext(itr, predicate)._1
        val secondValue = col(second)
        val secondExpectedValue = standardMap(second)
        e.message should be (Some("forBetween(2, 3) failed, because only 1 element satisfied the assertion block at key 2: \n" +
                                  "  at key " + first + ", \"[" + firstValue + "]\" did not equal \"[" + firstExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 14) + "), \n" +
                                  "  at key " + second + ", \"[" + secondValue + "]\" did not equal \"[" + secondExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 15) + ") \n" +
                                  "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'elements' in error message when > 1 element satisfied the assertion block, when total passed is less than 'from'` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "two", 3 -> "three"))
        val e = intercept[exceptions.TestFailedException] {
          forBetween(3, 4, col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val predicate = (e: Tuple2[Int, String]) => e._2 == standardMap(e._1)
        val first = getNext(itr, predicate)._1
        val second = getNext(itr, predicate)._1
        e.message should be (Some("forBetween(3, 4) failed, because only 2 elements satisfied the assertion block at key " + first + " and " + second + ": \n" +
                                  "  at key 1, \"[satu]\" did not equal \"[one]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 10) + ") \n" +
                                  "in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should use 'elements' in error message when > 1 element satisfied the assertion block, when total passed is more than 'upTo'` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "tiga", 4 -> "four", 5 -> "five"))
        val e = intercept[exceptions.TestFailedException] {
          forBetween(2, 3, col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val predicate = (e: Tuple2[Int, String]) => e._2 == standardMap(e._1)
        val first = getNext(itr, predicate)._1
        val second = getNext(itr, predicate)._1
        val third = getNext(itr, predicate)._1
        val forth = getNext(itr, predicate)._1
        e.message should be (Some("forBetween(2, 3) failed, because 4 elements satisfied the assertion block at key " + first + ", " + second + ", " + third + " and " + forth + " in " + col))
        e.getCause should be (null)
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when number of element passed is less than lower bound of the specified range` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "dua", 3 -> "three", 4 -> "empat", 5 -> "lima"))
        val e = intercept[exceptions.TestFailedException] {
          forBetween(2, 4, col) { e => 
            e._2 should equal (standardMap(e._1)) 
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val predicate = (e: Tuple2[Int, String]) => e._2 != standardMap(e._1)
        val first = getNext(itr, predicate)._1
        val firstValue = col(first)
        val firstExpectedValue = standardMap(first)
        val second = getNext(itr, predicate)._1
        val secondValue = col(second)
        val secondExpectedValue = standardMap(second)
        val third = getNext(itr, predicate)._1
        val thirdValue = col(third)
        val thirdExpectedValue = standardMap(third)
        val forth = getNext(itr, predicate)._1
        val forthValue = col(forth)
        val forthExpectedValue = standardMap(forth)
        e.message should be (Some("forBetween(2, 4) failed, because only 1 element satisfied the assertion block at key 3: \n" + 
                                  "  at key " + first + ", \"[" + firstValue  + "]\" did not equal \"[" + firstExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 20) + "), \n" +
                                  "  at key " + second + ", \"[" + secondValue  + "]\" did not equal \"[" + secondExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 21) + "), \n" +
                                  "  at key " + third + ", \"[" + thirdValue  + "]\" did not equal \"[" + thirdExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 22) + "), \n" +
                                  "  at key " + forth + ", \"[" + forthValue  + "]\" did not equal \"[" + forthExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 23) + ") \n" +
                                  "in " + col))
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when number of element passed is more than upper bound of the specified range` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five"))
        val e = intercept[exceptions.TestFailedException] {
          forBetween(2, 4, col) { e => 
            e._2 should equal (standardMap(e._1))  
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val predicate = (e: Tuple2[Int, String]) => e._2 == standardMap(e._1)
        val first = getNext(itr, predicate)._1
        val second = getNext(itr, predicate)._1
        val third = getNext(itr, predicate)._1
        val forth = getNext(itr, predicate)._1
        val fifth = getNext(itr, predicate)._1
        e.message should be (Some("forBetween(2, 4) failed, because 5 elements satisfied the assertion block at key " + first + ", " + second + ", " + third + ", " + forth + " and " + fifth + " in " + col))
      }
    }
    
  }
  
  object `forEvery ` {
    
    def `should pass when all elements passed` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "two", 3 -> "three"))
        forEvery(col) { e => e._2 should equal (standardMap(e._1)) }
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when at least one element failed` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "one", 2 -> "dua", 3 -> "three"))
        val e = intercept[exceptions.TestFailedException] {
          forEvery(col) { e => 
            e._2 should equal (standardMap(e._1)) 
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        e.message should be (Some("forEvery failed, because: \n" +
                                  "  at key 2, \"[dua]\" did not equal \"[two]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 6) + ") \n" +
                                  "in " + col))
      }
    }
    
    def `should throw TestFailedException with correct stack depth and message when more than one element failed` {
      forAll(examples) { colFun =>
        val col = colFun(Map(1 -> "satu", 2 -> "dua", 3 -> "three"))
        val e = intercept[exceptions.TestFailedException] {
          forEvery(col) { e => 
            e._2 should equal (standardMap(e._1))
          }
        }
        e.failedCodeFileName should be (Some("InspectorsForMapSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 5))
        val itr = col.toIterator
        val predicate = (e: Tuple2[Int, String]) => e._2 != standardMap(e._1)
        val first = getNext(itr, predicate)._1
        val firstValue = col(first)
        val firstExpectedValue = standardMap(first)
        val second = getNext(itr, predicate)._1
        val secondValue = col(second)
        val secondExpectedValue = standardMap(second)
        e.message should be (Some("forEvery failed, because: \n" +
                                  "  at key " + first + ", \"[" + firstValue + "]\" did not equal \"[" + firstExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 14) + "), \n" +
                                  "  at key " + second + ", \"[" + secondValue + "]\" did not equal \"[" + secondExpectedValue + "]\" (InspectorsForMapSpec.scala:" + (thisLineNumber - 15) + ") \n" +
                                  "in " + col))
      }
    }
    
  }
}
