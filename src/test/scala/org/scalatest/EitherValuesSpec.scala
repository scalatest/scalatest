package org.scalatest

import org.scalatest.EitherValues._
import org.scalatest.OptionValues._
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.SharedHelpers.thisLineNumber
/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.TestFailedException
*/

class EitherValuesSpec extends FunSpec with ShouldMatchers {
  describe("values on Either") {

    it("should return the left value inside an either if left.value is defined") {
      val e: Either[String, String] = Left("hi there")
      e.left.value should be === ("hi there")
      e.left.value should startWith ("hi")
    }

    it("should throw TestFailedException if left.value is empty") {
      val e: Either[String, String] = Right("hi there")
      val caught = 
        evaluating {
          e.left.value should startWith ("hi")
        } should produce [TestFailedException]
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
      caught.failedCodeFileName.value should be ("EitherValuesSpec.scala")
      caught.message.value should be (Resources("eitherLeftValueNotDefined"))
    }
    
    it("should return the right value inside an either if right.value is defined") {
      val e: Either[String, String] = Right("hi there")
      e.right.value should be === ("hi there")
      e.right.value should startWith ("hi")
    }
    
    it("should throw TestFailedException if right.value is empty") {
      val e: Either[String, String] = Left("hi there")
      val caught = 
        evaluating {
          e.right.value should startWith ("hi")
        } should produce [TestFailedException]
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
      caught.failedCodeFileName.value should be ("EitherValuesSpec.scala")
      caught.message.value should be (Resources("eitherRightValueNotDefined"))
    }
  } 
}
