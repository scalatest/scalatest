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
package org.scalatest.matchers

import org.scalatest._
import org.scalatest.exceptions.TestFailedException

class ShouldBeASymbolSpec extends Spec with ShouldMatchers with FileMocks {

  object `The be a ('symbol) syntax` {

    def `should do nothing if the object has an appropriately named method, which returns true` {
      fileMock should be a ('file)
      isFileMock should be a ('file)
    }

    def `should throw TestFailedException if no <symbol> or is<Symbol> method exists` {
      val ex1 = intercept[TestFailedException] {
        noPredicateMock should be a ('apple)
      }
      ex1.getMessage should equal ("NoPredicateMock has neither an apple nor an isApple method")
      // Check message for name that starts with a consonant (should use a instead of an)
      val ex2 = intercept[TestFailedException] {
        noPredicateMock should be a ('file)
      }
      ex2.getMessage should equal ("NoPredicateMock has neither a file nor an isFile method")
    }

    def `should do nothing if the object has an appropriately named method, which returns false when used with not` {
      notFileMock should not { be a ('file) }
      notFileMock should not be a ('file)
      isNotFileMock should not { be a ('file) }
      isNotFileMock should not be a ('file)
    }

    def `should throw TestFailedException if no <symbol> or is<Symbol> method exists, when used with not` {
      val ex1 = intercept[TestFailedException] {
        noPredicateMock should not { be a ('apple) }
      }
      ex1.getMessage should equal ("NoPredicateMock has neither an apple nor an isApple method")
      val ex2 = intercept[TestFailedException] {
        noPredicateMock should not (be a ('directory))
      }
      ex2.getMessage should equal ("NoPredicateMock has neither a directory nor an isDirectory method")
      val ex3 = intercept[TestFailedException] {
        noPredicateMock should not be a ('apple)
      }
      ex3.getMessage should equal ("NoPredicateMock has neither an apple nor an isApple method")
      val ex4 = intercept[TestFailedException] {
        noPredicateMock should not be a ('directory)
      }
      ex4.getMessage should equal ("NoPredicateMock has neither a directory nor an isDirectory method")
    }

    def `should do nothing if the object has an appropriately named method, which returns true, when used in a logical-and expression` {
      fileMock should ((be a ('file)) and (be a ('file)))
      fileMock should (be a ('file) and (be a ('file)))
      fileMock should (be a ('file) and be a ('file))
      isFileMock should ((be a ('file)) and (be a ('file)))
      isFileMock should (be a ('file) and (be a ('file)))
      isFileMock should (be a ('file) and be a ('file))
    }

    def `should do nothing if the object has an appropriately named method, which returns true, when used in a logical-or expression` {

      fileMock should ((be a ('directory)) or (be a ('file)))
      fileMock should (be a ('directory) or (be a ('file)))
      fileMock should (be a ('directory) or be a ('file))
      isFileMock should ((be a ('directory)) or (be a ('file)))
      isFileMock should (be a ('directory) or (be a ('file)))
      isFileMock should (be a ('directory) or be a ('file))

      fileMock should ((be a ('file)) or (be a ('directory)))
      fileMock should (be a ('file) or (be a ('directory)))
      fileMock should (be a ('file) or be a ('directory))
      isFileMock should ((be a ('file)) or (be a ('directory)))
      isFileMock should (be a ('file) or (be a ('directory)))
      isFileMock should (be a ('file) or be a ('directory))
    }

    def `should do nothing if the object has an appropriately named method, which returns false, when used in a logical-and expression with not` {

      notFileMock should (not (be a ('file)) and not (be a ('file)))
      notFileMock should ((not be a ('file)) and (not be a ('file)))
      notFileMock should (not be a ('file) and not be a ('file))

      isNotFileMock should (not (be a ('file)) and not (be a ('file)))
      isNotFileMock should ((not be a ('file)) and (not be a ('file)))
      isNotFileMock should (not be a ('file) and not be a ('file))
    }

    def `should do nothing if the object has an appropriately named method, which returns false, when used in a logical-or expression with not` {

      notFileMock should (not (be a ('file)) or not (be a ('file)))
      notFileMock should ((not be a ('file)) or (not be a ('file)))
      notFileMock should (not be a ('file) or not be a ('file))

      isNotFileMock should (not (be a ('file)) or not (be a ('file)))
      isNotFileMock should ((not be a ('file)) or (not be a ('file)))
      isNotFileMock should (not be a ('file) or not be a ('file))

      notFileMock should (not (be a ('directory)) or not (be a ('file)))
      notFileMock should ((not be a ('directory)) or (not be a ('file)))
      notFileMock should (not be a ('directory) or not be a ('file))

      isNotFileMock should (not (be a ('directory)) or not (be a ('file)))
      isNotFileMock should ((not be a ('directory)) or (not be a ('file)))
      isNotFileMock should (not be a ('directory) or not be a ('file))
    }

    def `should throw TestFailedException if the object has an appropriately named method, which returns false` {
      val caught1 = intercept[TestFailedException] {
        notFileMock should be a ('file)
      }
      assert(caught1.getMessage === "NotFileMock was not a file")
      val caught2 = intercept[TestFailedException] {
        isNotFileMock should be a ('file)
      }
      assert(caught2.getMessage === "IsNotFileMock was not a file")
    }

    def `should throw TestFailedException if the object has an appropriately named method, which returns true when used with not` {
      val caught1 = intercept[TestFailedException] {
        fileMock should not { be a ('file) }
      }
      assert(caught1.getMessage === "FileMock was a file")
      val caught2 = intercept[TestFailedException] {
        fileMock should not be a ('file)
      }
      assert(caught2.getMessage === "FileMock was a file")
      val caught3 = intercept[TestFailedException] {
        isFileMock should not { be a ('file) }
      }
      assert(caught3.getMessage === "IsFileMock was a file")
      val caught4 = intercept[TestFailedException] {
        isFileMock should not be a ('file)
      }
      assert(caught4.getMessage === "IsFileMock was a file")
    }

    def `should throw TestFailedException if the object has an appropriately named method, which returns false, when used in a logical-and expression` {
      val caught1 = intercept[TestFailedException] {
        fileMock should ((be a ('file)) and (be a ('directory)))
      }
      assert(caught1.getMessage === "FileMock was a file, but FileMock was not a directory")
      val caught2 = intercept[TestFailedException] {
        fileMock should (be a ('file) and (be a ('directory)))
      }
      assert(caught2.getMessage === "FileMock was a file, but FileMock was not a directory")
      val caught3 = intercept[TestFailedException] {
        fileMock should (be a ('file) and be a ('directory))
      }
      assert(caught3.getMessage === "FileMock was a file, but FileMock was not a directory")
      val caught4 = intercept[TestFailedException] {
        isFileMock should ((be a ('file)) and (be a ('directory)))
      }
      assert(caught4.getMessage === "IsFileMock was a file, but IsFileMock was not a directory")
      val caught5 = intercept[TestFailedException] {
        isFileMock should (be a ('file) and (be a ('directory)))
      }
      assert(caught5.getMessage === "IsFileMock was a file, but IsFileMock was not a directory")
      val caught6 = intercept[TestFailedException] {
        isFileMock should (be a ('file) and be a ('directory))
      }
      assert(caught6.getMessage === "IsFileMock was a file, but IsFileMock was not a directory")
    }

    def `should throw TestFailedException if the object has an appropriately named method, which returns false, when used in a logical-or expression` {

      val caught1 = intercept[TestFailedException] {
        notFileMock should ((be a ('file)) or (be a ('file)))
      }
      assert(caught1.getMessage === "NotFileMock was not a file, and NotFileMock was not a file")
      val caught2 = intercept[TestFailedException] {
        notFileMock should (be a ('file) or (be a ('file)))
      }
      assert(caught2.getMessage === "NotFileMock was not a file, and NotFileMock was not a file")
      val caught3 = intercept[TestFailedException] {
        notFileMock should (be a ('file) or be a ('file))
      }
      assert(caught3.getMessage === "NotFileMock was not a file, and NotFileMock was not a file")
      val caught4 = intercept[TestFailedException] {
        isNotFileMock should ((be a ('file)) or (be a ('file)))
      }
      assert(caught4.getMessage === "IsNotFileMock was not a file, and IsNotFileMock was not a file")
      val caught5 = intercept[TestFailedException] {
        isNotFileMock should (be a ('file) or (be a ('file)))
      }
      assert(caught5.getMessage === "IsNotFileMock was not a file, and IsNotFileMock was not a file")
      val caught6 = intercept[TestFailedException] {
        isNotFileMock should (be a ('file) or be a ('file))
      }
      assert(caught6.getMessage === "IsNotFileMock was not a file, and IsNotFileMock was not a file")
    }

    def `should throw TestFailedException if the object has an appropriately named method, which returns true, when used in a logical-and expression with not` {

      val caught1 = intercept[TestFailedException] {
        fileMock should (not (be a ('directory)) and not (be a ('file)))
      }
      assert(caught1.getMessage === "FileMock was not a directory, but FileMock was a file")
      val caught2 = intercept[TestFailedException] {
        fileMock should ((not be a ('directory)) and (not be a ('file)))
      }
      assert(caught2.getMessage === "FileMock was not a directory, but FileMock was a file")
      val caught3 = intercept[TestFailedException] {
        fileMock should (not be a ('directory) and not be a ('file))
      }
      assert(caught3.getMessage === "FileMock was not a directory, but FileMock was a file")
      val caught4 = intercept[TestFailedException] {
        isFileMock should (not (be a ('directory)) and not (be a ('file)))
      }
      assert(caught4.getMessage === "IsFileMock was not a directory, but IsFileMock was a file")
      val caught5 = intercept[TestFailedException] {
        isFileMock should ((not be a ('directory)) and (not be a ('file)))
      }
      assert(caught5.getMessage === "IsFileMock was not a directory, but IsFileMock was a file")
      val caught6 = intercept[TestFailedException] {
        isFileMock should (not be a ('directory) and not be a ('file))
      }
      assert(caught6.getMessage === "IsFileMock was not a directory, but IsFileMock was a file")
      // Check that the error message "short circuits"
      val caught7 = intercept[TestFailedException] {
        fileMock should (not (be a ('file)) and not (be a ('directory)))
      }
      assert(caught7.getMessage === "FileMock was a file")
    }

    def `should throw TestFailedException if the object has an appropriately named method, which returns true, when used in a logical-or expression with not` {

      val caught1 = intercept[TestFailedException] {
        fileMock should (not (be a ('file)) or not (be a ('file)))
      }
      assert(caught1.getMessage === "FileMock was a file, and FileMock was a file")
      val caught2 = intercept[TestFailedException] {
        fileMock should ((not be a ('file)) or (not be a ('file)))
      }
      assert(caught2.getMessage === "FileMock was a file, and FileMock was a file")
      val caught3 = intercept[TestFailedException] {
        fileMock should (not be a ('file) or not be a ('file))
      }
      assert(caught3.getMessage === "FileMock was a file, and FileMock was a file")
      val caught4 = intercept[TestFailedException] {
        isFileMock should (not (be a ('file)) or not (be a ('file)))
      }
      assert(caught4.getMessage === "IsFileMock was a file, and IsFileMock was a file")
      val caught5 = intercept[TestFailedException] {
        isFileMock should ((not be a ('file)) or (not be a ('file)))
      }
      assert(caught5.getMessage === "IsFileMock was a file, and IsFileMock was a file")
      val caught6 = intercept[TestFailedException] {
        isFileMock should (not be a ('file) or not be a ('file))
      }
      assert(caught6.getMessage === "IsFileMock was a file, and IsFileMock was a file")
    }
  }
}
