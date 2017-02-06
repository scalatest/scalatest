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
import scala.concurrent.Future
import org.scalatest.exceptions.TestFailedException

class AsyncInspectorsSpec extends AsyncFunSpec with Inspectors with TableDrivenPropertyChecks {

  describe("forAll") {

    it("should pass when all elements passed") {
      forAll(List(1, 2, 3)) { e =>
        Future {
          e should be < 4
        }
      }
    }

    it("should failed when at least one of the elements passed") {
      recoverToSucceededIf[TestFailedException] {
        forAll(List(1, 2, 3)) { e =>
          Future {
            e should be < 3
          }
        }
      }
    }
  }

}
