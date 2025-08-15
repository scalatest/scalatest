/*
 * Copyright 2001-2025 Artima, Inc.
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
import collection._
import java.lang.annotation.AnnotationFormatError
import java.nio.charset.CoderMalfunctionError
import javax.xml.parsers.FactoryConfigurationError
import javax.xml.transform.TransformerFactoryConfigurationError
import org.scalactic.Prettifier
import org.scalatest.prop.TableDrivenPropertyChecks
import scala.annotation.tailrec
import FailureMessages.decorateToStringValue
import scala.concurrent.Future
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AsyncFunSpec
import org.scalatest.matchers.should.Matchers._

class AsyncInspectorsSpec extends AsyncFunSpec with Inspectors with TableDrivenPropertyChecks {

  describe("Inspectors") {

    it("forAll with future block should pass when all elements passed") {
      forAll(List(1, 2, 3)) { e =>
        Future {
          e should be < 4
        }
      }
    }

    it("forAll with future block should fail when at least one of the elements failed") {
      recoverToSucceededIf[TestFailedException] {
        forAll(List(1, 2, 3)) { e =>
          Future {
            e should not be 2
          }
        }
      }
    }

    it("forAtLeast with future block should pass when at least x number of elements passed") {
      forAtLeast(2, List(1, 2, 3)) { e =>
        Future {
          e should be < 3
        }
      }
    }

    it("forAtLeast with future block should fail when the number of the elements passed is lesser than the min required") {
      recoverToSucceededIf[TestFailedException] {
        forAtLeast(2, List(1, 2, 3)) { e =>
          Future {
            e should be > 2
          }
        }
      }
    }

    it("forAtMost with future block should pass when at most x number of elements passed") {
      forAtMost(2, List(1, 2, 3)) { e =>
        Future {
          e should be < 3
        }
      }
    }

    it("forAtMost with future block should fail when the number of the elements passed is lesser than the max required") {
      recoverToSucceededIf[TestFailedException] {
        forAtMost(2, List(1, 2, 3)) { e =>
          Future {
            e should be > 0
          }
        }
      }
    }

    it("forExactly with future block should pass when exactly x number of elements passed") {
      forExactly(2, List(1, 2, 3)) { e =>
        Future {
          e should be < 3
        }
      }
    }

    it("forExactly with future block should fail when the number of the elements passed is not exactly the number required") {
      recoverToSucceededIf[TestFailedException] {
        forExactly(2, List(1, 2, 3)) { e =>
          Future {
            e should be > 0
          }
        }
      }
    }

    it("forNo with future block should pass when none of elements passed") {
      forNo(List(1, 2, 3)) { e =>
        Future {
          e should be > 3
        }
      }
    }

    it("forNo with future block should fail when any one of the elements passed") {
      recoverToSucceededIf[TestFailedException] {
        forNo(List(1, 2, 3)) { e =>
          Future {
            e should be < 3
          }
        }
      }
    }

    it("forBetween with future block should pass when number of elements passed is within the between range") {
      forBetween(1, 2, List(1, 2, 3)) { e =>
        Future {
          e should be < 3
        }
      }
    }

    it("forBetween with future block should fail when number of elements passed is not within the between range") {
      recoverToSucceededIf[TestFailedException] {
        forBetween(1, 2, List(1, 2, 3)) { e =>
          Future {
            e should be > 0
          }
        }
      }
    }

    it("forEvery with future block should pass when all elements passed") {
      forEvery(List(1, 2, 3)) { e =>
        Future {
          e should be < 4
        }
      }
    }

    it("forEvery with future block should fail when at least one of the elements failed") {
      recoverToSucceededIf[TestFailedException] {
        forEvery(List(1, 2, 3)) { e =>
          Future {
            e should not be 2
          }
        }
      }
    }
  }

}
