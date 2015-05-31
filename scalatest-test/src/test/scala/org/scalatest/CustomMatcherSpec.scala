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

import scala.collection.mutable.ListBuffer
import org.scalatest.exceptions.TestFailedException

import matchers.Matcher
import matchers.MatchResult

trait CustomMatchers {

  trait File {
    val getName: String
    val isFile: Boolean
    val exists: Boolean
  }

  class FileExistsMatcher extends Matcher[File] {

    def apply(left: File) = {

      val fileOrDir = if (left.isFile) "file" else "directory"

      val failureMessageSuffix = 
        fileOrDir + " named " + left.getName + " did not exist"

      val negatedFailureMessageSuffix = 
        fileOrDir + " named " + left.getName + " existed"

      MatchResult(
        left.exists,
        "The " + failureMessageSuffix,
        "The " + negatedFailureMessageSuffix,
        "the " + failureMessageSuffix,
        "the " + negatedFailureMessageSuffix
      )
    }
  }

  val beFound = new FileExistsMatcher
}

import Matchers._

class CustomMatcherSpec extends FunSpec with CustomMatchers {

  describe("A customer matcher") {

    val imaginaryFile = new File {
      val getName: String = "imaginary.txt"
      val isFile: Boolean = false
      val exists: Boolean = false
      override def toString: String = getName
    }

    val tempFile = new File {
      val getName: String = "delete"
      val isFile: Boolean = true
      val exists: Boolean = true
      override def toString: String = getName
    }

    it("should work when used in various combinations of and, or, and not, when the file does not exist") {

      imaginaryFile should not (beFound)

      val caught1 = intercept[TestFailedException] {
        imaginaryFile should beFound
      }
      assert(caught1.getMessage === "The directory named imaginary.txt did not exist")

      imaginaryFile should (not be tempFile and not (beFound))

      val caught2 = intercept[TestFailedException] {
        imaginaryFile should (not be tempFile and beFound)
      }
      assert(caught2.getMessage === "imaginary.txt was not equal to delete, but the directory named imaginary.txt did not exist")
    }

    it("should work when used in various combinations of and, or, and not, when the file does exist") {

      tempFile should beFound

      val caught1 = intercept[TestFailedException] {
        tempFile should not (beFound)
      }
      assert(caught1.getMessage === "The file named " + tempFile.getName + " existed")
      caught1.getMessage should be ("The file named delete existed")

      tempFile should (be (tempFile) and beFound)

      val caught2 = intercept[TestFailedException] {
        tempFile should (be (tempFile) and not (beFound))
      }
      caught2.getMessage should be ("delete was equal to delete, but the file named delete existed")
    }
  }

  describe("the compose method") {
    describe("on functions that return a matcher") {
      it("should return another function that returns a usable matcher") {
        val beAsIntEqual =  (equal (_: Int)) compose ((_: String).toInt)
        3 should beAsIntEqual ("3")
        3 should not (beAsIntEqual ("4"))
      }
    }

    describe("on matchers themselves") {
      it("should return a usable matcher") {
        val beOdd =
          new Matcher[Int] {
            def apply(left: Int) =
              MatchResult(
                left % 2 == 1,
                left + " was not odd",
                left + " was odd"
              )
          }

        3 should beOdd
        4 should not (beOdd)

        // val beOddAsInt = beOdd compose ((_: String).toInt)
        val beOddAsInt = beOdd compose { (s: String) => s.toInt }

        "3" should beOddAsInt
        "4" should not (beOddAsInt)

        case class Product(name: String)
        case class LineItem(product: Product)
        def haveProduct(p: Product) = equal(p).matcher[Product] compose { (lineItem: LineItem) => lineItem.product }

        LineItem(Product("widgets")) should (haveProduct(Product("widgets")))
      }
    }
  }

  describe("A factory method on Matcher's companion object") {
    it("should produce a matcher that executes the passed function when its apply is called") {
      val f = { (s: String) => MatchResult(s.length < 3, "s was not less than 3", "s was less than 3") }
      val haveLengthLessThanThree = Matcher(f)
      "" should haveLengthLessThanThree
      "x" should haveLengthLessThanThree
      "xx" should haveLengthLessThanThree
      "xxx" should not (haveLengthLessThanThree)
      "xxxx" should not (haveLengthLessThanThree)
    }
  }
}
 
