package org.scalatest

import org.scalatest.TryValues._
import org.scalatest.OptionValues._
import org.scalatest.Matchers._
import org.scalatest.SharedHelpers.thisLineNumber
import scala.util.Try
import scala.util.Failure
import scala.util.Success
/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.TestFailedException
*/

class TryValuesSpec extends FunSpec {

  describe("invoking failure on Try") {

    it("should return the Try as a Failure if it is one") {
      val ex = new Exception
      val t: Try[String] = new Failure(ex)
      t.failure.exception should be theSameInstanceAs ex
    }

    it("should throw TestFailedException if the Try is a Success") {
      val t: Try[String] = Success("hi there")
      val caught = 
        evaluating {
          t.failure.exception should equal (new Exception)
        } should produce [TestFailedException]
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
      caught.failedCodeFileName.value should be ("TryValuesSpec.scala")
      caught.message.value should be (Resources("tryNotAFailure"))
    }

    it("should return the value inside a Try if it is a Success") {
      val t: Try[String] = Success("hi there")
      t.success.value should equal ("hi there")
    }

    it("should throw TestFailedException if right.value is empty") {
      val ex = new Exception
      val t: Try[String] = Failure(ex)
      val caught = 
        evaluating {
          t.success.value should startWith ("hi")
        } should produce [TestFailedException]
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 2)
      caught.failedCodeFileName.value should be ("TryValuesSpec.scala")
      caught.message.value should be (Resources("tryNotASuccess"))
    }
  } 
}

