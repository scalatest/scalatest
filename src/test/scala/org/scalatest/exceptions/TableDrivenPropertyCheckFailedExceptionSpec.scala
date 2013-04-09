/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest.exceptions

import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.SharedHelpers.thisLineNumber
import org.scalatest.prop._
/* Uncomment this after removing the deprecated type aliases in the org.scalatest.prop package object
import org.scalatest.exceptions.TableDrivenPropertyCheckFailedException
*/

class TableDrivenPropertyCheckFailedExceptionSpec extends FunSpec with ShouldMatchers with TableDrivenPropertyChecks {

  describe("The TableDrivenPropertyCheckFailedException") {

    it("should give the proper line on a table-driven property check") {
      val examples =
        Table(
          ("a", "b"),
          (1, 2),
          (3, 4),
          (6, 5),
          (7, 8)
        )
      try {
        forAll (examples) { (a, b) => a should be < b }
      }
      catch {
        case e: TableDrivenPropertyCheckFailedException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => s should equal ("TableDrivenPropertyCheckFailedExceptionSpec.scala:" + (thisLineNumber - 5))
            case None => fail("A table-driven property check didn't produce a file name and line number string", e)
          }
        case e: Throwable =>
          fail("forAll (examples) { (a, b) => a should be < b } didn't produce a TableDrivenPropertyCheckFailedException", e)
      }
    }

    describe("even when it is nested in another describe") {
      it("should give the proper line on a table-driven property check") {
        val examples =
          Table(
            ("a", "b"),
            (1, 2),
            (3, 4),
            (6, 5),
            (7, 8)
          )
        try {
          forAll (examples) { (a, b) => a should be < b }
        }
        catch {
          case e: TableDrivenPropertyCheckFailedException =>
            e.failedCodeFileNameAndLineNumberString match {
              case Some(s) => s should equal ("TableDrivenPropertyCheckFailedExceptionSpec.scala:" + (thisLineNumber - 5))
              case None => fail("A table-driven property check didn't produce a file name and line number string", e)
            }
          case e: Throwable =>
            fail("forAll (examples) { (a, b) => a should be < b } didn't produce a TableDrivenPropertyCheckFailedException", e)
        }
      }
    }

    it("should return the cause in both cause and getCause") {
      val theCause = new IllegalArgumentException("howdy")
      val tfe = new TableDrivenPropertyCheckFailedException(sde => "doody", Some(theCause), sde => 3, None, "howdy", List(1, 2, 3), List("a", "b", "c"), 7)
      assert(tfe.cause.isDefined)
      assert(tfe.cause.get === theCause)
      assert(tfe.getCause == theCause)
    }

    it("should return None in cause and null in getCause if no cause") {
      val tfe = new TableDrivenPropertyCheckFailedException(sde => "doody", None, sde => 3, None, "howdy", List(1, 2, 3), List("a", "b", "c"), 7)
      assert(tfe.cause.isEmpty)
      assert(tfe.getCause == null)
    }

    it("should be equal to itself") {
      val tfe = new TableDrivenPropertyCheckFailedException(sde => "doody", None, sde => 3, None, "howdy", List(1, 2, 3), List("a", "b", "c"), 7)
      assert(tfe equals tfe)
    }
  }
}
 
