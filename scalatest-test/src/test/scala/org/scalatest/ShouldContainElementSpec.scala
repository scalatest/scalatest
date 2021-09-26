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

import FailureMessages._
import Matchers._
import org.scalactic.Prettifier
import org.scalatest.prop.PropertyChecks
import org.scalatest.exceptions.TestFailedException
import org.scalatest.CompatParColls.Converters._

class ShouldContainElementSpec extends FunSpec with PropertyChecks with ReturnsNormallyThrowsAssertion {

  private val prettifier = Prettifier.default

  // Checking for a specific size
  describe("The 'contain (Int)' syntax") {

    describe("on Array") {

      it("should do nothing if array contains the specified element") {
        Array(1, 2) should contain (2)
        Array(1, 2) should (contain (2))
        // check((arr: Array[Int]) => arr.size != 0 ==> returnsNormally(arr should contain (arr(arr.length - 1))))
      }

      it("should do nothing if array does not contain the element and used with should not") {
        Array(1, 2) should not { contain (3) }
        Array(1, 2) should not contain (3)
        // check((arr: Array[Int], i: Int) => !arr.exists(_ == i) ==> returnsNormally(arr should not { contain (i) }))
        // check((arr: Array[Int], i: Int) => !arr.exists(_ == i) ==> returnsNormally(arr should not contain (i)))
      }

      it("should do nothing when array contains the specified element and used in a logical-and expression") {
        Array(1, 2) should { contain (2) and (contain (1)) }
        Array(1, 2) should ((contain (2)) and (contain (1)))
        Array(1, 2) should (contain (2) and contain (1))
       }

      it("should do nothing when array contains the specified element and used in a logical-or expression") {
        Array(1, 2) should { contain (77) or (contain (2)) }
        Array(1, 2) should ((contain (77)) or (contain (2)))
        Array(1, 2) should (contain (77) or contain (2))
      }

      it("should do nothing when array doesn't contain the specified element and used in a logical-and expression with not") {
        Array(1, 2) should { not { contain (5) } and not { contain (3) }}
        Array(1, 2) should ((not contain (5)) and (not contain (3)))
        Array(1, 2) should (not contain (5) and not contain (3))
      }

      it("should do nothing when array doesn't contain the specified element and used in a logical-or expression with not") {
        Array(1, 2) should { not { contain (1) } or not { contain (3) }}
        Array(1, 2) should ((not contain (1)) or (not contain (3)))
        Array(1, 2) should (not contain (3) or not contain (2))
      }

      it("should throw TestFailedException if array does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          Array(1, 2) should contain (3)
        }
        assert(caught.getMessage === "Array(1, 2) did not contain element 3")
        // check((arr: Array[String], s: String) => !arr.exists(_ == s) ==> throwsTestFailedException(arr should contain (s)))
      }

      it("should throw TestFailedException if array contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          Array(1, 2) should not contain (2)
        }
        assert(caught1.getMessage === "Array(1, 2) contained element 2")
        // check((arr: Array[String]) => arr.length > 0 ==> throwsTestFailedException(arr should not contain (arr(0))))

        val caught2 = intercept[TestFailedException] {
          Array(1, 2) should not (contain (2))
        }
        assert(caught2.getMessage === "Array(1, 2) contained element 2")
        // check((arr: Array[String]) => arr.length > 0 ==> throwsTestFailedException(arr should not (contain (arr(0)))))

        val caught3 = intercept[TestFailedException] {
          Array(1, 2) should (not contain (2))
        }
        assert(caught3.getMessage === "Array(1, 2) contained element 2")
        // check((arr: Array[String]) => arr.length > 0 ==> throwsTestFailedException(arr should not (contain (arr(0)))))
      }

      it("should throw a TestFailedException when array doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          Array(1, 2) should { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "Array(1, 2) did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          Array(1, 2) should (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "Array(1, 2) did not contain element 5")
      }

      it("should throw a TestFailedException when array doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          Array(1, 2) should { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "Array(1, 2) did not contain element 55, and Array(1, 2) did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          Array(1, 2) should (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "Array(1, 2) did not contain element 55, and Array(1, 2) did not contain element 22")
      }

      it("should throw a TestFailedException when array contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          Array(1, 2) should { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "Array(1, 2) did not contain element 3, but Array(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          Array(1, 2) should ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "Array(1, 2) did not contain element 3, but Array(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          Array(1, 2) should (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "Array(1, 2) did not contain element 3, but Array(1, 2) contained element 2")
      }

      it("should throw a TestFailedException when array contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          Array(1, 2) should { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "Array(1, 2) contained element 2, and Array(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          Array(1, 2) should ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "Array(1, 2) contained element 2, and Array(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          Array(1, 2) should (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "Array(1, 2) contained element 2, and Array(1, 2) contained element 2")
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should work on parallel form") {
        Array(1, 2).par should contain (2)
      }
      // SKIP-SCALATESTJS,NATIVE-END
    }

    describe("on scala.collection.immutable.Set ") {

      it("should do nothing if set contains the specified element") {
        Set(1, 2) should contain (2)
        Set(1, 2) should (contain (2))
      }

      it("should do nothing if set does not contain the element and used with should not") {
        Set(1, 2) should not { contain (3) }
        Set(1, 2) should not contain (3)
      }

      it("should do nothing when set contains the specified element and used in a logical-and expression") {
        Set(1, 2) should { contain (2) and (contain (1)) }
        Set(1, 2) should ((contain (2)) and (contain (1)))
        Set(1, 2) should (contain (2) and contain (1))
       }

      it("should do nothing when set contains the specified element and used in a logical-or expression") {
        Set(1, 2) should { contain (77) or (contain (2)) }
        Set(1, 2) should ((contain (77)) or (contain (2)))
        Set(1, 2) should (contain (77) or contain (2))
      }

      it("should do nothing when set doesn't contain the specified element and used in a logical-and expression with not") {
        Set(1, 2) should { not { contain (5) } and not { contain (3) }}
        Set(1, 2) should ((not contain (5)) and (not contain (3)))
        Set(1, 2) should (not contain (5) and not contain (3))
      }

      it("should do nothing when set doesn't contain the specified element and used in a logical-or expression with not") {
        Set(1, 2) should { not { contain (1) } or not { contain (3) }}
        Set(1, 2) should ((not contain (1)) or (not contain (3)))
        Set(1, 2) should (not contain (3) or not contain (2))
      }

      it("should throw TestFailedException if set does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          Set(1, 2) should contain (3)
        }
        assert(caught.getMessage === "Set(1, 2) did not contain element 3")
      }

      it("should throw TestFailedException if set contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          Set(1, 2) should not contain (2)
        }
        assert(caught1.getMessage === "Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          Set(1, 2) should not (contain (2))
        }
        assert(caught2.getMessage === "Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          Set(1, 2) should (not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) contained element 2")
      }

      it("should throw a TestFailedException when set doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          Set(1, 2) should { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          Set(1, 2) should (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 5")
      }

      it("should throw a TestFailedException when set doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          Set(1, 2) should { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 55, and Set(1, 2) did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          Set(1, 2) should (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 55, and Set(1, 2) did not contain element 22")
      }

      it("should throw a TestFailedException when set contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          Set(1, 2) should { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          Set(1, 2) should ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          Set(1, 2) should (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")
      }

      it("should throw a TestFailedException when set contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          Set(1, 2) should { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          Set(1, 2) should ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          Set(1, 2) should (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should work on parallel form") {
        Set(1, 2).par should contain (2)
      }
      // SKIP-SCALATESTJS,NATIVE-END
    }

    describe("on scala.collection.mutable.Set ") {

      import scala.collection.mutable

      it("should do nothing if set contains the specified element") {
        mutable.Set(1, 2) should contain (2)
        mutable.Set(1, 2) should (contain (2))
      }

      it("should do nothing if set does not contain the element and used with should not") {
        mutable.Set(1, 2) should not { contain (3) }
        mutable.Set(1, 2) should not contain (3)
      }

      it("should do nothing when set contains the specified element and used in a logical-and expression") {
        mutable.Set(1, 2) should { contain (2) and (contain (1)) }
        mutable.Set(1, 2) should ((contain (2)) and (contain (1)))
        mutable.Set(1, 2) should (contain (2) and contain (1))
       }

      it("should do nothing when set contains the specified element and used in a logical-or expression") {
        mutable.Set(1, 2) should { contain (77) or (contain (2)) }
        mutable.Set(1, 2) should ((contain (77)) or (contain (2)))
        mutable.Set(1, 2) should (contain (77) or contain (2))
      }

      it("should do nothing when set doesn't contain the specified element and used in a logical-and expression with not") {
        mutable.Set(1, 2) should { not { contain (5) } and not { contain (3) }}
        mutable.Set(1, 2) should ((not contain (5)) and (not contain (3)))
        mutable.Set(1, 2) should (not contain (5) and not contain (3))
      }

      it("should do nothing when set doesn't contain the specified element and used in a logical-or expression with not") {
        mutable.Set(1, 2) should { not { contain (1) } or not { contain (3) }}
        mutable.Set(1, 2) should ((not contain (1)) or (not contain (3)))
        mutable.Set(1, 2) should (not contain (3) or not contain (2))
      }

      it("should throw TestFailedException if set does not contain the specified element") {
        val set = mutable.Set(1, 2)
        val caught = intercept[TestFailedException] {
           set should contain (3)
        }
        assert(caught.getMessage === set + " did not contain element 3")
      }

      it("should throw TestFailedException if set contains the specified element, when used with not") {
        val set1 = mutable.Set(1, 2)
        val caught1 = intercept[TestFailedException] {
          set1 should not contain (2)
        }
        assert(caught1.getMessage === set1 + " contained element 2")

        val set2 = mutable.Set(1, 2)
        val caught2 = intercept[TestFailedException] {
          set2 should not (contain (2))
        }
        assert(caught2.getMessage === set2 + " contained element 2")

        val set3 = mutable.Set(1, 2)
        val caught3 = intercept[TestFailedException] {
          set3 should (not contain (2))
        }
        assert(caught3.getMessage === set3 + " contained element 2")
      }

      it("should throw a TestFailedException when set doesn't contain the specified element and used in a logical-and expression") {
        val set1 = mutable.Set(1, 2)
        val caught1 = intercept[TestFailedException] {
          set1 should { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === set1 + " did not contain element 5")

        val set2 = mutable.Set(1, 2)
        val caught2 = intercept[TestFailedException] {
          set2 should (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === set2 + " did not contain element 5")
      }

      it("should throw a TestFailedException when set doesn't contain the specified element and used in a logical-or expression") {
        val set1 = mutable.Set(1, 2) 
        val caught1 = intercept[TestFailedException] {(
          set1 should { contain (55) or (contain (22)) }
        )}
        assert(caught1.getMessage === set1 + " did not contain element 55, and " + set1 + " did not contain element 22")

        val set2 = mutable.Set(1, 2)
        val caught2 = intercept[TestFailedException] {
          set2 should (contain (55) or contain (22))
        }
        assert(caught2.getMessage === set2 + " did not contain element 55, and " + set2 + " did not contain element 22")
      }

      it("should throw a TestFailedException when set contains the specified element and used in a logical-and expression with not") {
        val set1 = mutable.Set(1, 2)
        val caught1 = intercept[TestFailedException] {(
          set1 should { not { contain (3) } and not { contain (2) }}
        )}
        assert(caught1.getMessage === set1 + " did not contain element 3, but " + set1 + " contained element 2")
        
        val set2 = mutable.Set(1, 2) 
        val caught2 = intercept[TestFailedException] {(
          set2 should ((not contain (3)) and (not contain (2))))
        }
        assert(caught2.getMessage === set2 + " did not contain element 3, but " + set2 + " contained element 2")
        
        val set3 = mutable.Set(1, 2) 
        val caught3 = intercept[TestFailedException] {(
          set3 should (not contain (3) and not contain (2)))
        }
        assert(caught3.getMessage === set3 + " did not contain element 3, but " + set3 + " contained element 2")
      }

      it("should throw a TestFailedException when set contains the specified element and used in a logical-or expression with not") {
        val set1 = mutable.Set(1, 2)
        val caught1 = intercept[TestFailedException] {(
          set1 should { not { contain (2) } or not { contain (2) } }
        )}
        assert(caught1.getMessage === set1 + " contained element 2, and " + set1 + " contained element 2")
        
        val set2 = mutable.Set(1, 2) 
        val caught2 = intercept[TestFailedException] {(
          set2 should ((not contain (2)) or (not contain (2))))
        }
        assert(caught2.getMessage === set2 + " contained element 2, and " + set2 + " contained element 2")
        
        val set3 = mutable.Set(1, 2) 
        val caught3 = intercept[TestFailedException] {(
          set3 should (not contain (2) or not contain (2)))
        }
        assert(caught3.getMessage === set3 + " contained element 2, and " + set3 + " contained element 2")
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should work on parallel form") {
        mutable.Set(1, 2).par should contain (2)
      }
      // SKIP-SCALATESTJS,NATIVE-END
    }

    describe("on scala.collection.Set ") {

      val set: scala.collection.Set[Int] = Set(1, 2)

      it("should do nothing if set contains the specified element") {
        set should contain (2)
        set should (contain (2))
      }

      it("should do nothing if set does not contain the element and used with should not") {
        set should not { contain (3) }
        set should not contain (3)
      }

      it("should do nothing when set contains the specified element and used in a logical-and expression") {
        set should { contain (2) and (contain (1)) }
        set should ((contain (2)) and (contain (1)))
        set should (contain (2) and contain (1))
       }

      it("should do nothing when set contains the specified element and used in a logical-or expression") {
        set should { contain (77) or (contain (2)) }
        set should ((contain (77)) or (contain (2)))
        set should (contain (77) or contain (2))
      }

      it("should do nothing when set doesn't contain the specified element and used in a logical-and expression with not") {
        set should { not { contain (5) } and not { contain (3) }}
        set should ((not contain (5)) and (not contain (3)))
        set should (not contain (5) and not contain (3))
      }

      it("should do nothing when set doesn't contain the specified element and used in a logical-or expression with not") {
        set should { not { contain (1) } or not { contain (3) }}
        set should ((not contain (1)) or (not contain (3)))
        set should (not contain (3) or not contain (2))
      }

      it("should throw TestFailedException if set does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          set should contain (3)
        }
        assert(caught.getMessage === "Set(1, 2) did not contain element 3")
      }

      it("should throw TestFailedException if set contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          set should not contain (2)
        }
        assert(caught1.getMessage === "Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          set should not (contain (2))
        }
        assert(caught2.getMessage === "Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          set should (not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) contained element 2")
      }

      it("should throw a TestFailedException when set doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          set should { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          set should (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 5")
      }

      it("should throw a TestFailedException when set doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          set should { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 55, and Set(1, 2) did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          set should (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 55, and Set(1, 2) did not contain element 22")
      }

      it("should throw a TestFailedException when set contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          set should { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          set should ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          set should (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) did not contain element 3, but Set(1, 2) contained element 2")
      }

      it("should throw a TestFailedException when set contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          set should { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          set should ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          set should (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "Set(1, 2) contained element 2, and Set(1, 2) contained element 2")
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should work on parallel form") {
        set.par should contain (2)
      }
      // SKIP-SCALATESTJS,NATIVE-END
    }

    describe("on scala.collection.immutable.HashSet ") {

      import scala.collection.immutable.HashSet
        
      it("should do nothing if set contains the specified element") {
        HashSet(1, 2) should contain (2)
        HashSet(1, 2) should (contain (2))
      }

      it("should do nothing if set does not contain the element and used with should not") {
        HashSet(1, 2) should not { contain (3) }
        HashSet(1, 2) should not contain (3)
      }

      it("should do nothing when set contains the specified element and used in a logical-and expression") {
        HashSet(1, 2) should { contain (2) and (contain (1)) }
        HashSet(1, 2) should ((contain (2)) and (contain (1)))
        HashSet(1, 2) should (contain (2) and contain (1))
       }

      it("should do nothing when set contains the specified element and used in a logical-or expression") {
        HashSet(1, 2) should { contain (77) or (contain (2)) }
        HashSet(1, 2) should ((contain (77)) or (contain (2)))
        HashSet(1, 2) should (contain (77) or contain (2))
      }

      it("should do nothing when set doesn't contain the specified element and used in a logical-and expression with not") {
        HashSet(1, 2) should { not { contain (5) } and not { contain (3) }}
        HashSet(1, 2) should ((not contain (5)) and (not contain (3)))
        HashSet(1, 2) should (not contain (5) and not contain (3))
      }

      it("should do nothing when set doesn't contain the specified element and used in a logical-or expression with not") {
        HashSet(1, 2) should { not { contain (1) } or not { contain (3) }}
        HashSet(1, 2) should ((not contain (1)) or (not contain (3)))
        HashSet(1, 2) should (not contain (3) or not contain (2))
      }

      it("should throw TestFailedException if set does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          HashSet(1, 2) should contain (3)
        }
        assert(caught.getMessage === HashSet(1, 2) + " did not contain element 3")
      }

      it("should throw TestFailedException if set contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          HashSet(1, 2) should not contain (2)
        }
        assert(caught1.getMessage === HashSet(1, 2) + " contained element 2")

        val caught2 = intercept[TestFailedException] {
          HashSet(1, 2) should not (contain (2))
        }
        assert(caught2.getMessage === HashSet(1, 2) + " contained element 2")

        val caught3 = intercept[TestFailedException] {
          HashSet(1, 2) should (not contain (2))
        }
        assert(caught3.getMessage === HashSet(1, 2) + " contained element 2")
      }

      it("should throw a TestFailedException when set doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          HashSet(1, 2) should { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === HashSet(1, 2) + " did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          HashSet(1, 2) should (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === HashSet(1, 2) + " did not contain element 5")
      }

      it("should throw a TestFailedException when set doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          HashSet(1, 2) should { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === HashSet(1, 2) + " did not contain element 55, and " + HashSet(1, 2) + " did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          HashSet(1, 2) should (contain (55) or contain (22))
        }
        assert(caught2.getMessage === HashSet(1, 2) + " did not contain element 55, and " + HashSet(1, 2) + " did not contain element 22")
      }

      it("should throw a TestFailedException when set contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          HashSet(1, 2) should { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === HashSet(1, 2) + " did not contain element 3, but " + HashSet(1, 2) + " contained element 2")

        val caught2 = intercept[TestFailedException] {
          HashSet(1, 2) should ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === HashSet(1, 2) + " did not contain element 3, but " + HashSet(1, 2) + " contained element 2")

        val caught3 = intercept[TestFailedException] {
          HashSet(1, 2) should (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === HashSet(1, 2) + " did not contain element 3, but " + HashSet(1, 2) + " contained element 2")
      }

      it("should throw a TestFailedException when set contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          HashSet(1, 2) should { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === HashSet(1, 2) + " contained element 2, and " + HashSet(1, 2) + " contained element 2")

        val caught2 = intercept[TestFailedException] {
          HashSet(1, 2) should ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === HashSet(1, 2) + " contained element 2, and " + HashSet(1, 2) + " contained element 2")

        val caught3 = intercept[TestFailedException] {
          HashSet(1, 2) should (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === HashSet(1, 2) + " contained element 2, and " + HashSet(1, 2) + " contained element 2")
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should work on parallel form") {
        HashSet(1, 2).par should contain (2)
      }
      // SKIP-SCALATESTJS,NATIVE-END
    }

    describe("on scala.collection.mutable.HashSet ") {

      import scala.collection.mutable

      it("should do nothing if set contains the specified element") {
        mutable.HashSet(1, 2) should contain (2)
        mutable.HashSet(1, 2) should (contain (2))
      }

      it("should do nothing if set does not contain the element and used with should not") {
        mutable.HashSet(1, 2) should not { contain (3) }
        mutable.HashSet(1, 2) should not contain (3)
      }

      it("should do nothing when set contains the specified element and used in a logical-and expression") {
        mutable.HashSet(1, 2) should { contain (2) and (contain (1)) }
        mutable.HashSet(1, 2) should ((contain (2)) and (contain (1)))
        mutable.HashSet(1, 2) should (contain (2) and contain (1))
       }

      it("should do nothing when set contains the specified element and used in a logical-or expression") {
        mutable.HashSet(1, 2) should { contain (77) or (contain (2)) }
        mutable.HashSet(1, 2) should ((contain (77)) or (contain (2)))
        mutable.HashSet(1, 2) should (contain (77) or contain (2))
      }

      it("should do nothing when set doesn't contain the specified element and used in a logical-and expression with not") {
        mutable.HashSet(1, 2) should { not { contain (5) } and not { contain (3) }}
        mutable.HashSet(1, 2) should ((not contain (5)) and (not contain (3)))
        mutable.HashSet(1, 2) should (not contain (5) and not contain (3))
      }

      it("should do nothing when set doesn't contain the specified element and used in a logical-or expression with not") {
        mutable.HashSet(1, 2) should { not { contain (1) } or not { contain (3) }}
        mutable.HashSet(1, 2) should ((not contain (1)) or (not contain (3)))
        mutable.HashSet(1, 2) should (not contain (3) or not contain (2))
      }

      it("should throw TestFailedException if set does not contain the specified element") {
        val set = mutable.HashSet(1, 2)
        val caught = intercept[TestFailedException] {
          set should contain (3)
        }
        assert(caught.getMessage === set + " did not contain element 3")
      }

      it("should throw TestFailedException if set contains the specified element, when used with not") {
        val set1 = mutable.HashSet(1, 2)
        val caught1 = intercept[TestFailedException] {
          set1 should not contain (2)
        }
        assert(caught1.getMessage === set1 + " contained element 2")

        val set2 = mutable.HashSet(1, 2)
        val caught2 = intercept[TestFailedException] {
          set2 should not (contain (2))
        }
        assert(caught2.getMessage === set2 + " contained element 2")

        val set3 = mutable.HashSet(1, 2)
        val caught3 = intercept[TestFailedException] {
          set3 should (not contain (2))
        }
        assert(caught3.getMessage === set3 + " contained element 2")
      }

      it("should throw a TestFailedException when set doesn't contain the specified element and used in a logical-and expression") {
        val set1 = mutable.HashSet(1, 2)
        val caught1 = intercept[TestFailedException] {
           set1 should { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === set1 + " did not contain element 5")
        
        val set2 = mutable.HashSet(1, 2) 
        val caught2 = intercept[TestFailedException] {(
          set2 should (contain (5) and contain (2 - 1)))
        }
        assert(caught2.getMessage === set2 + " did not contain element 5")
      }

      it("should throw a TestFailedException when set doesn't contain the specified element and used in a logical-or expression") {
        val set1 = mutable.HashSet(1, 2)
        val caught1 = intercept[TestFailedException] {
          set1 should { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === set1 + " did not contain element 55, and " + set1 + " did not contain element 22")

        val set2 = mutable.HashSet(1, 2)
        val caught2 = intercept[TestFailedException] {
          set2 should (contain (55) or contain (22))
        }
        assert(caught2.getMessage === set2 + " did not contain element 55, and " + set2 + " did not contain element 22")
      }

      it("should throw a TestFailedException when set contains the specified element and used in a logical-and expression with not") {
        val set1 = mutable.HashSet(1, 2)
        val caught1 = intercept[TestFailedException] {
           set1 should { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === set1 + " did not contain element 3, but " + set1 + " contained element 2")
        
        val set2 = mutable.HashSet(1, 2) 
        val caught2 = intercept[TestFailedException] {(
          set2 should ((not contain (3)) and (not contain (2))))
        }
        assert(caught2.getMessage === set2 + " did not contain element 3, but " + set2 + " contained element 2")
        
        val set3 = mutable.HashSet(1, 2) 
        val caught3 = intercept[TestFailedException] {(
          set3 should (not contain (3) and not contain (2)))
        }
        assert(caught3.getMessage === set3 + " did not contain element 3, but " + set3 + " contained element 2")
      }

      it("should throw a TestFailedException when set contains the specified element and used in a logical-or expression with not") {
        val set1 = mutable.HashSet(1, 2) 
        val caught1 = intercept[TestFailedException] {
          set1 should { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === set1 + " contained element 2, and " + set1 + " contained element 2")

        val set2 = mutable.HashSet(1, 2)
        val caught2 = intercept[TestFailedException] {
          set2 should ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === set2 + " contained element 2, and " + set2 + " contained element 2")

        val set3 = mutable.HashSet(1, 2)
        val caught3 = intercept[TestFailedException] {
          set3 should (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === set3 + " contained element 2, and " + set3 + " contained element 2")
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should work on parallel form") {
        mutable.HashSet(1, 2).par should contain (2)
      }
      // SKIP-SCALATESTJS,NATIVE-END
    }

    describe("on List") {

      it("should do nothing if list contains the specified element") {
        List(1, 2) should contain (2)
        List(1, 2) should (contain (2))
        forAll((list: List[Int]) => if (list.size != 0) list should contain (list(list.length - 1)) else succeed)
      }

      it("should do nothing if list does not contain the element and used with should not") {
        List(1, 2) should not { contain (3) }
        List(1, 2) should not contain (3)
        forAll((list: List[Int], i: Int) => if (!list.exists(_ == i)) list should not { contain (i) } else succeed)
        forAll((list: List[Int], i: Int) => if (!list.exists(_ == i)) list should not contain (i) else succeed)
      }

      it("should do nothing when list contains the specified element and used in a logical-and expression") {
        List(1, 2) should { contain (2) and (contain (1)) }
        List(1, 2) should ((contain (2)) and (contain (1)))
        List(1, 2) should (contain (2) and contain (1))
       }

      it("should do nothing when list contains the specified element and used in a logical-or expression") {
        List(1, 2) should { contain (77) or (contain (2)) }
        List(1, 2) should ((contain (77)) or (contain (2)))
        List(1, 2) should (contain (77) or contain (2))
      }

      it("should do nothing when list doesn't contain the specified element and used in a logical-and expression with not") {
        List(1, 2) should { not { contain (5) } and not { contain (3) }}
        List(1, 2) should ((not contain (5)) and (not contain (3)))
        List(1, 2) should (not contain (5) and not contain (3))
      }

      it("should do nothing when list doesn't contain the specified element and used in a logical-or expression with not") {
        List(1, 2) should { not { contain (1) } or not { contain (3) }}
        List(1, 2) should ((not contain (1)) or (not contain (3)))
        List(1, 2) should (not contain (3) or not contain (2))
      }

      it("should throw TestFailedException if list does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          List(1, 2) should contain (3)
        }
        assert(caught.getMessage === "List(1, 2) did not contain element 3")
        forAll((list: List[String], s: String) => if (!list.exists(_ == s)) assertThrows[TestFailedException](list should contain (s)) else succeed)
      }

      it("should throw TestFailedException if list contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          List(1, 2) should not contain (2)
        }
        assert(caught1.getMessage === "List(1, 2) contained element 2")
        forAll((list: List[String]) => if (list.length > 0) assertThrows[TestFailedException](list should not contain (list(0))) else succeed)

        val caught2 = intercept[TestFailedException] {
          List(1, 2) should not (contain (2))
        }
        assert(caught2.getMessage === "List(1, 2) contained element 2")
        forAll((list: List[String]) => if (list.length > 0) assertThrows[TestFailedException](list should not (contain (list(0)))) else succeed)

        val caught3 = intercept[TestFailedException] {
          List(1, 2) should (not contain (2))
        }
        assert(caught3.getMessage === "List(1, 2) contained element 2")
        forAll((list: List[String]) => if (list.length > 0) assertThrows[TestFailedException](list should not (contain (list(0)))) else succeed)
      }

      it("should throw a TestFailedException when list doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          List(1, 2) should { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "List(1, 2) did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          List(1, 2) should (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "List(1, 2) did not contain element 5")
      }

      it("should throw a TestFailedException when list doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          List(1, 2) should { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "List(1, 2) did not contain element 55, and List(1, 2) did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          List(1, 2) should (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "List(1, 2) did not contain element 55, and List(1, 2) did not contain element 22")
      }

      it("should throw a TestFailedException when list contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          List(1, 2) should { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "List(1, 2) did not contain element 3, but List(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          List(1, 2) should ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "List(1, 2) did not contain element 3, but List(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          List(1, 2) should (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "List(1, 2) did not contain element 3, but List(1, 2) contained element 2")
      }

      it("should throw a TestFailedException when list contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          List(1, 2) should { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "List(1, 2) contained element 2, and List(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          List(1, 2) should ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "List(1, 2) contained element 2, and List(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          List(1, 2) should (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "List(1, 2) contained element 2, and List(1, 2) contained element 2")
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should work on parallel form") {
        List(1, 2).par should contain (2)
      }
      // SKIP-SCALATESTJS,NATIVE-END
    }

    describe("on Vector") {

      it("should do nothing if vector contains the specified element") {
        Vector(1, 2) should contain (2)
        Vector(1, 2) should (contain (2))
        // check((vector: Vector[Int]) => vector.size != 0 ==> returnsNormally(vector should contain (vector(vector.length - 1))))
      }

      it("should do nothing if vector does not contain the element and used with should not") {
        Vector(1, 2) should not { contain (3) }
        Vector(1, 2) should not contain (3)
        // check((vector: Vector[Int], i: Int) => !vector.exists(_ == i) ==> returnsNormally(vector should not { contain (i) }))
        // check((vector: Vector[Int], i: Int) => !vector.exists(_ == i) ==> returnsNormally(vector should not contain (i)))
      }

      it("should do nothing when vector contains the specified element and used in a logical-and expression") {
        Vector(1, 2) should { contain (2) and (contain (1)) }
        Vector(1, 2) should ((contain (2)) and (contain (1)))
        Vector(1, 2) should (contain (2) and contain (1))
       }

      it("should do nothing when vector contains the specified element and used in a logical-or expression") {
        Vector(1, 2) should { contain (77) or (contain (2)) }
        Vector(1, 2) should ((contain (77)) or (contain (2)))
        Vector(1, 2) should (contain (77) or contain (2))
      }

      it("should do nothing when vector doesn't contain the specified element and used in a logical-and expression with not") {
        Vector(1, 2) should { not { contain (5) } and not { contain (3) }}
        Vector(1, 2) should ((not contain (5)) and (not contain (3)))
        Vector(1, 2) should (not contain (5) and not contain (3))
      }

      it("should do nothing when vector doesn't contain the specified element and used in a logical-or expression with not") {
        Vector(1, 2) should { not { contain (1) } or not { contain (3) }}
        Vector(1, 2) should ((not contain (1)) or (not contain (3)))
        Vector(1, 2) should (not contain (3) or not contain (2))
      }

      it("should throw TestFailedException if vector does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          Vector(1, 2) should contain (3)
        }
        assert(caught.getMessage === "Vector(1, 2) did not contain element 3")
        // check((vector: Vector[String], s: String) => !vector.exists(_ == s) ==> throwsTestFailedException(vector should contain (s)))
      }

      it("should throw TestFailedException if vector contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          Vector(1, 2) should not contain (2)
        }
        assert(caught1.getMessage === "Vector(1, 2) contained element 2")
        // check((vector: Vector[String]) => vector.length > 0 ==> throwsTestFailedException(vector should not contain (vector(0))))

        val caught2 = intercept[TestFailedException] {
          Vector(1, 2) should not (contain (2))
        }
        assert(caught2.getMessage === "Vector(1, 2) contained element 2")
        // check((vector: Vector[String]) => vector.length > 0 ==> throwsTestFailedException(vector should not (contain (vector(0)))))

        val caught3 = intercept[TestFailedException] {
          Vector(1, 2) should (not contain (2))
        }
        assert(caught3.getMessage === "Vector(1, 2) contained element 2")
        // check((vector: Vector[String]) => vector.length > 0 ==> throwsTestFailedException(vector should not (contain (vector(0)))))
      }

      it("should throw a TestFailedException when vector doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          Vector(1, 2) should { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "Vector(1, 2) did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          Vector(1, 2) should (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "Vector(1, 2) did not contain element 5")
      }

      it("should throw a TestFailedException when vector doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          Vector(1, 2) should { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "Vector(1, 2) did not contain element 55, and Vector(1, 2) did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          Vector(1, 2) should (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "Vector(1, 2) did not contain element 55, and Vector(1, 2) did not contain element 22")
      }

      it("should throw a TestFailedException when vector contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          Vector(1, 2) should { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "Vector(1, 2) did not contain element 3, but Vector(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          Vector(1, 2) should ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "Vector(1, 2) did not contain element 3, but Vector(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          Vector(1, 2) should (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "Vector(1, 2) did not contain element 3, but Vector(1, 2) contained element 2")
      }

      it("should throw a TestFailedException when vector contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          Vector(1, 2) should { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "Vector(1, 2) contained element 2, and Vector(1, 2) contained element 2")

        val caught2 = intercept[TestFailedException] {
          Vector(1, 2) should ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "Vector(1, 2) contained element 2, and Vector(1, 2) contained element 2")

        val caught3 = intercept[TestFailedException] {
          Vector(1, 2) should (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "Vector(1, 2) contained element 2, and Vector(1, 2) contained element 2")
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should work on parallel form") {
        Vector(1, 2).par should contain (2)
      }
      // SKIP-SCALATESTJS,NATIVE-END
    }

    // SKIP-SCALATESTJS,NATIVE-START
    describe("on java.util.List") {

      val javaList: java.util.List[Int] = new java.util.ArrayList
      javaList.add(1)
      javaList.add(2)
      
      it("should do nothing if list contains the specified element") {
        javaList should contain (2)
        javaList should (contain (2))
      }

      it("should do nothing if list does not contain the element and used with should not") {
        javaList should (not contain (3))
        javaList should not { contain (3) }
        javaList should not contain (3)
      }

      it("should do nothing when list contains the specified element and used in a logical-and expression") {
        javaList should { contain (2) and (contain (1)) }
        javaList should ((contain (2)) and (contain (1)))
        javaList should (contain (2) and contain (1))
       }

      it("should do nothing when list contains the specified element and used in a logical-or expression") {
        javaList should { contain (77) or (contain (2)) }
        javaList should ((contain (77)) or (contain (2)))
        javaList should (contain (77) or contain (2))
      }

      it("should do nothing when list doesn't contain the specified element and used in a logical-and expression with not") {
        javaList should { not { contain (5) } and not { contain (3) }}
        javaList should ((not contain (5)) and (not contain (3)))
        javaList should (not contain (5) and not contain (3))
      }

      it("should do nothing when list doesn't contain the specified element and used in a logical-or expression with not") {
        javaList should { not { contain (1) } or not { contain (3) }}
        javaList should ((not contain (1)) or (not contain (3)))
        javaList should (not contain (3) or not contain (2))
      }

      it("should throw TestFailedException if list does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          javaList should contain (3)
        }
        assert(caught.getMessage === "[1, 2] did not contain element 3")
      }

      it("should throw TestFailedException if list contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          javaList should not contain (2)
        }
        assert(caught1.getMessage === "[1, 2] contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaList should not (contain (2))
        }
        assert(caught2.getMessage === "[1, 2] contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaList should (not contain (2))
        }
        assert(caught3.getMessage === "[1, 2] contained element 2")
      }

      it("should throw a TestFailedException when list doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          javaList should { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "[1, 2] did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          javaList should (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "[1, 2] did not contain element 5")
      }

      it("should throw a TestFailedException when list doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          javaList should { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "[1, 2] did not contain element 55, and [1, 2] did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          javaList should (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "[1, 2] did not contain element 55, and [1, 2] did not contain element 22")
      }

      it("should throw a TestFailedException when list contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          javaList should { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "[1, 2] did not contain element 3, but [1, 2] contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaList should ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "[1, 2] did not contain element 3, but [1, 2] contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaList should (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "[1, 2] did not contain element 3, but [1, 2] contained element 2")
      }

      it("should throw a TestFailedException when list contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          javaList should { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "[1, 2] contained element 2, and [1, 2] contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaList should ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "[1, 2] contained element 2, and [1, 2] contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaList should (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "[1, 2] contained element 2, and [1, 2] contained element 2")
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END

    describe("on scala.collection.immutable.Map ") {

      it("should do nothing if map contains specified element") {
        Map("one" -> 1, "two" -> 2) should contain ("two" -> 2)
        Map("one" -> 1, "two" -> 2) should (contain ("two" -> 2))
        Map(1 -> "one", 2 -> "two") should contain (2 -> "two")
      }

      it("should do nothing if map does not contain the specified element and used with not") {
        Map("one" -> 1, "two" -> 2) should not { contain ("three" -> 3) }
        Map("one" -> 1, "two" -> 2) should not contain ("three" -> 3)
        Map("one" -> 1, "two" -> 2) should (not contain ("three" -> 3))
      }

      it("should do nothing when map contains specified element and used in a logical-and expression") {
        Map("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain ("one" -> 1)) }
        Map("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain ("one" -> 1)))
        Map("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain ("one" -> 1))
      }

      it("should do nothing when map contains specified element and used in a logical-or expression") {
        Map("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain ("one" -> 1)) }
        Map("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain ("one" -> 1)))
        Map("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain ("one" -> 1))
      }

      it("should do nothing when map does not contain the specified element and used in a logical-and expression with not") {
        Map("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain ("three" -> 3) }}
        Map("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain ("three" -> 3)))
        Map("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain ("three" -> 3))
      }

      it("should do nothing when map does not contain the specified element and used in a logical-or expression with not") {
        Map("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain ("three" -> 3) }}
        Map("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain ("three" -> 3)))
        Map("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain ("three" -> 3))
      }

      it("should throw TestFailedException if map does not contain the specified element") {
        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should contain ("three" -> 3)
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (three,3)")
      }

      it("should throw TestFailedException if contains the specified element when used with not") {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (not contain ("two" -> 2))
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should not (contain ("two" -> 2))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should not contain ("two" -> 2)
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")
      }

      it("should throw an TestFailedException when map doesn't contain specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should { contain ("five" -> 5) and (contain ("two" -> 2)) }
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (five,5)")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should ((contain ("five" -> 5)) and (contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (five,5)")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (contain ("five" -> 5) and contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (five,5)")
      }

      it("should throw an TestFailedException when map doesn't contain specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should { contain ("fifty five" -> 55) or (contain ("twenty two" -> 22)) }
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (fifty five,55), and Map(\"one\" -> 1, \"two\" -> 2) did not contain element (twenty two,22)")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should ((contain ("fifty five" -> 55)) or (contain ("twenty two" -> 22)))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (fifty five,55), and Map(\"one\" -> 1, \"two\" -> 2) did not contain element (twenty two,22)")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (contain ("fifty five" -> 55) or contain ("twenty two" -> 22))
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (fifty five,55), and Map(\"one\" -> 1, \"two\" -> 2) did not contain element (twenty two,22)")
      }

      it("should throw an TestFailedException when map contains specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should { not { contain ("three" -> 3) } and not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (three,3), but Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should ((not contain ("three" -> 3)) and (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (three,3), but Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (not contain ("three" -> 3) and not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (three,3), but Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")
      }

      it("should throw an TestFailedException when map contains specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2), and Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2), and Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          Map("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2), and Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should work on parallel form") {
        Map("one" -> 1, "two" -> 2).par should contain ("two" -> 2)
      }
      // SKIP-SCALATESTJS,NATIVE-END
    }

    describe("on scala.collection.mutable.Map ") {

      import scala.collection.mutable

      it("should do nothing if map contains specified element") {
        mutable.Map("one" -> 1, "two" -> 2) should contain ("two" -> 2)
        mutable.Map("one" -> 1, "two" -> 2) should (contain ("two" -> 2))
        mutable.Map(1 -> "one", 2 -> "two") should contain (2 -> "two")
      }

      it("should do nothing if map does not contain the specified element and used with not") {
        mutable.Map("one" -> 1, "two" -> 2) should not { contain ("three" -> 3) }
        mutable.Map("one" -> 1, "two" -> 2) should not contain ("three" -> 3)
        mutable.Map("one" -> 1, "two" -> 2) should (not contain ("three" -> 3))
      }

      it("should do nothing when map contains specified element and used in a logical-and expression") {
        mutable.Map("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain ("one" -> 1)) }
        mutable.Map("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain ("one" -> 1)))
        mutable.Map("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain ("one" -> 1))
      }

      it("should do nothing when map contains specified element and used in a logical-or expression") {
        mutable.Map("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain ("one" -> 1)) }
        mutable.Map("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain ("one" -> 1)))
        mutable.Map("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain ("one" -> 1))
      }

      it("should do nothing when map does not contain the specified element and used in a logical-and expression with not") {
        mutable.Map("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain ("three" -> 3) }}
        mutable.Map("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain ("three" -> 3)))
        mutable.Map("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain ("three" -> 3))
      }

      it("should do nothing when map does not contain the specified element and used in a logical-or expression with not") {
        mutable.Map("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain ("three" -> 3) }}
        mutable.Map("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain ("three" -> 3)))
        mutable.Map("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain ("three" -> 3))
      }

      it("should throw TestFailedException if map does not contain the specified element") {
        val map = mutable.Map("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {(
          map should contain ("three" -> 3)
        )}
        assert(caught1.getMessage === decorateToStringValue(prettifier, map) + " did not contain element (three,3)")
      }

      it("should throw TestFailedException if contains the specified element when used with not") {
        val map1 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {(
          map1 should (not contain ("two" -> 2))
        )}
        assert(caught1.getMessage === decorateToStringValue(prettifier, map1) + " contained element (two,2)")
        
        val map2 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught2 = intercept[TestFailedException] {(
          map2 should not (contain ("two" -> 2))
        )}
        assert(caught2.getMessage === decorateToStringValue(prettifier, map2) + " contained element (two,2)")

        val map3 = mutable.Map("one" -> 1, "two" -> 2)
        val caught3 = intercept[TestFailedException] {
           map3 should not contain ("two" -> 2)
        }
        assert(caught3.getMessage === decorateToStringValue(prettifier, map3) + " contained element (two,2)")
      }

      it("should throw an TestFailedException when map doesn't contain specified element and used in a logical-and expression") {
        val map1 = mutable.Map("one" -> 1, "two" -> 2)
        val caught1 = intercept[TestFailedException] {
          map1 should { contain ("five" -> 5) and (contain ("two" -> 2)) }
        }
        assert(caught1.getMessage === decorateToStringValue(prettifier, map1) + " did not contain element (five,5)")

        val map2 = mutable.Map("one" -> 1, "two" -> 2)
        val caught2 = intercept[TestFailedException] {
          map2 should ((contain ("five" -> 5)) and (contain ("two" -> 2)))
        }
        assert(caught2.getMessage === decorateToStringValue(prettifier, map2) + " did not contain element (five,5)")

        val map3 = mutable.Map("one" -> 1, "two" -> 2)
        val caught3 = intercept[TestFailedException] {
          map3 should (contain ("five" -> 5) and contain ("two" -> 2))
        }
        assert(caught3.getMessage === decorateToStringValue(prettifier, map3) + " did not contain element (five,5)")
      }

      it("should throw an TestFailedException when map doesn't contain specified element and used in a logical-or expression") {
        val map1 = mutable.Map("one" -> 1, "two" -> 2)
        val caught1 = intercept[TestFailedException] {
          map1 should { contain ("fifty five" -> 55) or (contain ("twenty two" -> 22)) }
        }
        assert(caught1.getMessage === decorateToStringValue(prettifier, map1) + " did not contain element (fifty five,55), and " + decorateToStringValue(prettifier, map1) + " did not contain element (twenty two,22)")

        val map2 = mutable.Map("one" -> 1, "two" -> 2)
        val caught2 = intercept[TestFailedException] {
          map2 should ((contain ("fifty five" -> 55)) or (contain ("twenty two" -> 22)))
        }
        assert(caught2.getMessage === decorateToStringValue(prettifier, map2) + " did not contain element (fifty five,55), and " + decorateToStringValue(prettifier, map2) + " did not contain element (twenty two,22)")
        
        val map3 = mutable.Map("one" -> 1, "two" -> 2) 
        val caught3 = intercept[TestFailedException] {(
          map3 should (contain ("fifty five" -> 55) or contain ("twenty two" -> 22)))
        }
        assert(caught3.getMessage === decorateToStringValue(prettifier, map3) + " did not contain element (fifty five,55), and " + decorateToStringValue(prettifier, map3) + " did not contain element (twenty two,22)")
      }

      it("should throw an TestFailedException when map contains specified element and used in a logical-and expression with not") {
        val map1 = mutable.Map("one" -> 1, "two" -> 2)
        val caught1 = intercept[TestFailedException] {
          map1 should { not { contain ("three" -> 3) } and not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === decorateToStringValue(prettifier, map1) + " did not contain element (three,3), but " + decorateToStringValue(prettifier, map1) + " contained element (two,2)")

        val map2 = mutable.Map("one" -> 1, "two" -> 2)
        val caught2 = intercept[TestFailedException] {
          map2 should ((not contain ("three" -> 3)) and (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === decorateToStringValue(prettifier, map2) + " did not contain element (three,3), but " + decorateToStringValue(prettifier, map2) + " contained element (two,2)")

        val map3 = mutable.Map("one" -> 1, "two" -> 2)
        val caught3 = intercept[TestFailedException] {
          map3 should (not contain ("three" -> 3) and not contain ("two" -> 2))
        }
        assert(caught3.getMessage === decorateToStringValue(prettifier, map3) + " did not contain element (three,3), but " + decorateToStringValue(prettifier, map3) + " contained element (two,2)")
      }

      it("should throw an TestFailedException when map contains specified element and used in a logical-or expression with not") {
        val map1 = mutable.Map("one" -> 1, "two" -> 2)
        val caught1 = intercept[TestFailedException] {
          map1 should { not { contain ("two" -> 2) } or not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === decorateToStringValue(prettifier, map1) + " contained element (two,2), and " + decorateToStringValue(prettifier, map1) + " contained element (two,2)")
        
        val map2 = mutable.Map("one" -> 1, "two" -> 2)
        val caught2 = intercept[TestFailedException] {
          map2 should ((not contain ("two" -> 2)) or (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === decorateToStringValue(prettifier, map2) + " contained element (two,2), and " + decorateToStringValue(prettifier, map2) + " contained element (two,2)")
        
        val map3 = mutable.Map("one" -> 1, "two" -> 2)
        val caught3 = intercept[TestFailedException] {
          map3 should (not contain ("two" -> 2) or not contain ("two" -> 2))
        }
        assert(caught3.getMessage === decorateToStringValue(prettifier, map3) + " contained element (two,2), and " + decorateToStringValue(prettifier, map3) + " contained element (two,2)")
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should work on parallel form") {
        mutable.Map("one" -> 1, "two" -> 2).par should contain ("two" -> 2)
      }
      // SKIP-SCALATESTJS,NATIVE-END
    }

    describe("on scala.collection.Map ") {

      val map: scala.collection.Map[String, Int] = Map("one" -> 1, "two" -> 2)

      it("should do nothing if map contains specified element") {
        map should contain ("two" -> 2)
        map should (contain ("two" -> 2))
        map should contain ("two" -> 2)
      }

      it("should do nothing if map does not contain the specified element and used with not") {
        map should not { contain ("three" -> 3) }
        map should not contain ("three" -> 3)
        map should (not contain ("three" -> 3))
      }

      it("should do nothing when map contains specified element and used in a logical-and expression") {
        map should { contain ("two" -> 2) and (contain ("one" -> 1)) }
        map should ((contain ("two" -> 2)) and (contain ("one" -> 1)))
        map should (contain ("two" -> 2) and contain ("one" -> 1))
      }

      it("should do nothing when map contains specified element and used in a logical-or expression") {
        map should { contain ("cat" -> 77) or (contain ("one" -> 1)) }
        map should ((contain ("cat" -> 77)) or (contain ("one" -> 1)))
        map should (contain ("cat" -> 77) or contain ("one" -> 1))
      }

      it("should do nothing when map does not contain the specified element and used in a logical-and expression with not") {
        map should { not { contain ("five" -> 5) } and not { contain ("three" -> 3) }}
        map should ((not contain ("five" -> 5)) and (not contain ("three" -> 3)))
        map should (not contain ("five" -> 5) and not contain ("three" -> 3))
      }

      it("should do nothing when map does not contain the specified element and used in a logical-or expression with not") {
        map should { not { contain ("two" -> 2) } or not { contain ("three" -> 3) }}
        map should ((not contain ("two" -> 2)) or (not contain ("three" -> 3)))
        map should (not contain ("two" -> 2) or not contain ("three" -> 3))
      }

      it("should throw TestFailedException if map does not contain the specified element") {
        val caught1 = intercept[TestFailedException] {
          map should contain ("three" -> 3)
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (three,3)")
      }

      it("should throw TestFailedException if contains the specified element when used with not") {

        val caught1 = intercept[TestFailedException] {
          map should (not contain ("two" -> 2))
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          map should not (contain ("two" -> 2))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          map should not contain ("two" -> 2)
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")
      }

      it("should throw an TestFailedException when map doesn't contain specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          map should { contain ("five" -> 5) and (contain ("two" -> 2)) }
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (five,5)")

        val caught2 = intercept[TestFailedException] {
          map should ((contain ("five" -> 5)) and (contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (five,5)")

        val caught3 = intercept[TestFailedException] {
          map should (contain ("five" -> 5) and contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (five,5)")
      }

      it("should throw an TestFailedException when map doesn't contain specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          map should { contain ("fifty five" -> 55) or (contain ("twenty two" -> 22)) }
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (fifty five,55), and Map(\"one\" -> 1, \"two\" -> 2) did not contain element (twenty two,22)")

        val caught2 = intercept[TestFailedException] {
          map should ((contain ("fifty five" -> 55)) or (contain ("twenty two" -> 22)))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (fifty five,55), and Map(\"one\" -> 1, \"two\" -> 2) did not contain element (twenty two,22)")

        val caught3 = intercept[TestFailedException] {
          map should (contain ("fifty five" -> 55) or contain ("twenty two" -> 22))
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (fifty five,55), and Map(\"one\" -> 1, \"two\" -> 2) did not contain element (twenty two,22)")
      }

      it("should throw an TestFailedException when map contains specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          map should { not { contain ("three" -> 3) } and not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (three,3), but Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          map should ((not contain ("three" -> 3)) and (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (three,3), but Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          map should (not contain ("three" -> 3) and not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) did not contain element (three,3), but Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")
      }

      it("should throw an TestFailedException when map contains specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          map should { not { contain ("two" -> 2) } or not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2), and Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught2 = intercept[TestFailedException] {
          map should ((not contain ("two" -> 2)) or (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2), and Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")

        val caught3 = intercept[TestFailedException] {
          map should (not contain ("two" -> 2) or not contain ("two" -> 2))
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2), and Map(\"one\" -> 1, \"two\" -> 2) contained element (two,2)")
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should work on parallel form") {
        map.par should contain ("two" -> 2)
      }
      // SKIP-SCALATESTJS,NATIVE-END
    }

    describe("on scala.collection.immutable.HashMap ") {

      import scala.collection.immutable.HashMap

      it("should do nothing if map contains specified element") {
        HashMap("one" -> 1, "two" -> 2) should contain ("two" -> 2)
        HashMap("one" -> 1, "two" -> 2) should (contain ("two" -> 2))
        HashMap(1 -> "one", 2 -> "two") should contain (2 -> "two")
      }

      it("should do nothing if map does not contain the specified element and used with not") {
        HashMap("one" -> 1, "two" -> 2) should not { contain ("three" -> 3) }
        HashMap("one" -> 1, "two" -> 2) should not contain ("three" -> 3)
        HashMap("one" -> 1, "two" -> 2) should (not contain ("three" -> 3))
      }

      it("should do nothing when map contains specified element and used in a logical-and expression") {
        HashMap("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain ("one" -> 1)) }
        HashMap("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain ("one" -> 1)))
        HashMap("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain ("one" -> 1))
      }

      it("should do nothing when map contains specified element and used in a logical-or expression") {
        HashMap("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain ("one" -> 1)) }
        HashMap("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain ("one" -> 1)))
        HashMap("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain ("one" -> 1))
      }

      it("should do nothing when map does not contain the specified element and used in a logical-and expression with not") {
        HashMap("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain ("three" -> 3) }}
        HashMap("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain ("three" -> 3)))
        HashMap("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain ("three" -> 3))
      }

      it("should do nothing when map does not contain the specified element and used in a logical-or expression with not") {
        HashMap("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain ("three" -> 3) }}
        HashMap("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain ("three" -> 3)))
        HashMap("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain ("three" -> 3))
      }

      it("should throw TestFailedException if map does not contain the specified element") {
        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should contain ("three" -> 3)
        }
        assert(caught1.getMessage == (Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " did not contain element (three,3)"))
      }

      it("should throw TestFailedException if contains the specified element when used with not") {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (not contain ("two" -> 2))
        }
        assert(caught1.getMessage == (Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " contained element (two,2)"))
        
        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should not (contain ("two" -> 2))
        }
        assert(caught2.getMessage == (Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " contained element (two,2)"))

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should not contain ("two" -> 2)
        }
        assert(caught3.getMessage == (Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " contained element (two,2)"))
      }

      it("should throw an TestFailedException when map doesn't contain specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should { contain ("five" -> 5) and (contain ("two" -> 2)) }
        }
        assert(caught1.getMessage == (Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " did not contain element (five,5)"))
        
        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should ((contain ("five" -> 5)) and (contain ("two" -> 2)))
        }
        assert(caught2.getMessage == (Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " did not contain element (five,5)"))
        
        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (contain ("five" -> 5) and contain ("two" -> 2))
        }
        assert(caught3.getMessage == (Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " did not contain element (five,5)"))
      }

      it("should throw an TestFailedException when map doesn't contain specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should { contain ("fifty five" -> 55) or (contain ("twenty two" -> 22)) }
        }
        assert(caught1.getMessage == (Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " did not contain element (fifty five,55), and " + Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " did not contain element (twenty two,22)"))
        
        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should ((contain ("fifty five" -> 55)) or (contain ("twenty two" -> 22)))
        }
        assert(caught2.getMessage == (Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " did not contain element (fifty five,55), and " + Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " did not contain element (twenty two,22)"))
        
        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (contain ("fifty five" -> 55) or contain ("twenty two" -> 22))
        }
        assert(caught3.getMessage == (Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " did not contain element (fifty five,55), and " + Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " did not contain element (twenty two,22)"))
      }

      it("should throw an TestFailedException when map contains specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should { not { contain ("three" -> 3) } and not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === (Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " did not contain element (three,3), but " + Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " contained element (two,2)"))
        
        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should ((not contain ("three" -> 3)) and (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === (Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " did not contain element (three,3), but " + Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " contained element (two,2)"))
        
        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (not contain ("three" -> 3) and not contain ("two" -> 2))
        }
        assert(caught3.getMessage === (Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " did not contain element (three,3), but " + Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " contained element (two,2)"))
      }

      it("should throw an TestFailedException when map contains specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === (Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " contained element (two,2), and " + Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " contained element (two,2)"))
        
        val caught2 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === (Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " contained element (two,2), and " + Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " contained element (two,2)"))

        val caught3 = intercept[TestFailedException] {
          HashMap("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain ("two" -> 2))
        }
        assert(caught3.getMessage === (Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " contained element (two,2), and " + Prettifier.default(HashMap("one" -> 1, "two" -> 2)) + " contained element (two,2)"))
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should work on parallel form") {
        HashMap("one" -> 1, "two" -> 2).par should contain ("two" -> 2)
      }
      // SKIP-SCALATESTJS,NATIVE-END
    }

    describe("on scala.collection.mutable.HashMap ") {

      import scala.collection.mutable

      it("should do nothing if map contains specified element") {
        mutable.HashMap("one" -> 1, "two" -> 2) should contain ("two" -> 2)
        mutable.HashMap("one" -> 1, "two" -> 2) should (contain ("two" -> 2))
        mutable.HashMap(1 -> "one", 2 -> "two") should contain (2 -> "two")
      }

      it("should do nothing if map does not contain the specified element and used with not") {
        mutable.HashMap("one" -> 1, "two" -> 2) should not { contain ("three" -> 3) }
        mutable.HashMap("one" -> 1, "two" -> 2) should not contain ("three" -> 3)
        mutable.HashMap("one" -> 1, "two" -> 2) should (not contain ("three" -> 3))
      }

      it("should do nothing when map contains specified element and used in a logical-and expression") {
        mutable.HashMap("one" -> 1, "two" -> 2) should { contain ("two" -> 2) and (contain ("one" -> 1)) }
        mutable.HashMap("one" -> 1, "two" -> 2) should ((contain ("two" -> 2)) and (contain ("one" -> 1)))
        mutable.HashMap("one" -> 1, "two" -> 2) should (contain ("two" -> 2) and contain ("one" -> 1))
      }

      it("should do nothing when map contains specified element and used in a logical-or expression") {
        mutable.HashMap("one" -> 1, "two" -> 2) should { contain ("cat" -> 77) or (contain ("one" -> 1)) }
        mutable.HashMap("one" -> 1, "two" -> 2) should ((contain ("cat" -> 77)) or (contain ("one" -> 1)))
        mutable.HashMap("one" -> 1, "two" -> 2) should (contain ("cat" -> 77) or contain ("one" -> 1))
      }

      it("should do nothing when map does not contain the specified element and used in a logical-and expression with not") {
        mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain ("five" -> 5) } and not { contain ("three" -> 3) }}
        mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain ("five" -> 5)) and (not contain ("three" -> 3)))
        mutable.HashMap("one" -> 1, "two" -> 2) should (not contain ("five" -> 5) and not contain ("three" -> 3))
      }

      it("should do nothing when map does not contain the specified element and used in a logical-or expression with not") {
        mutable.HashMap("one" -> 1, "two" -> 2) should { not { contain ("two" -> 2) } or not { contain ("three" -> 3) }}
        mutable.HashMap("one" -> 1, "two" -> 2) should ((not contain ("two" -> 2)) or (not contain ("three" -> 3)))
        mutable.HashMap("one" -> 1, "two" -> 2) should (not contain ("two" -> 2) or not contain ("three" -> 3))
      }

      it("should throw TestFailedException if map does not contain the specified element") {
        val map = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught1 = intercept[TestFailedException] {
          map should contain ("three" -> 3)
        }
        assert(caught1.getMessage === decorateToStringValue(prettifier, map) + " did not contain element (three,3)")
      }

      it("should throw TestFailedException if contains the specified element when used with not") {
        val map1 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {(
          map1 should (not contain ("two" -> 2)))
        }
        assert(caught1.getMessage === decorateToStringValue(prettifier, map1) + " contained element (two,2)")
        
        val map2 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught2 = intercept[TestFailedException] {(
          map2 should not (contain ("two" -> 2)))
        }
        assert(caught2.getMessage === decorateToStringValue(prettifier, map2) + " contained element (two,2)")
        
        val map3 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught3 = intercept[TestFailedException] {(
          map3 should not contain ("two" -> 2))
        }
        assert(caught3.getMessage === decorateToStringValue(prettifier, map3) + " contained element (two,2)")
      }

      it("should throw an TestFailedException when map doesn't contain specified element and used in a logical-and expression") {
        val map1 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught1 = intercept[TestFailedException] {
          map1 should { contain ("five" -> 5) and (contain ("two" -> 2)) }
        }
        assert(caught1.getMessage === decorateToStringValue(prettifier, map1) + " did not contain element (five,5)")
        
        val map2 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught2 = intercept[TestFailedException] {(
          map2 should ((contain ("five" -> 5)) and (contain ("two" -> 2))))
        }
        assert(caught2.getMessage === decorateToStringValue(prettifier, map2) + " did not contain element (five,5)")
        
        val map3 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught3 = intercept[TestFailedException] {(
          map3 should (contain ("five" -> 5) and contain ("two" -> 2)))
        }
        assert(caught3.getMessage === decorateToStringValue(prettifier, map3) + " did not contain element (five,5)")
      }

      it("should throw an TestFailedException when map doesn't contain specified element and used in a logical-or expression") {
        val map1 = mutable.HashMap("one" -> 1, "two" -> 2) 
        val caught1 = intercept[TestFailedException] {(
          map1 should { contain ("fifty five" -> 55) or (contain ("twenty two" -> 22)) }
        )}
        assert(caught1.getMessage === decorateToStringValue(prettifier, map1) + " did not contain element (fifty five,55), and " + decorateToStringValue(prettifier, map1) + " did not contain element (twenty two,22)")

        val map2 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught2 = intercept[TestFailedException] {
          map2 should ((contain ("fifty five" -> 55)) or (contain ("twenty two" -> 22)))
        }
        assert(caught2.getMessage === decorateToStringValue(prettifier, map2) + " did not contain element (fifty five,55), and " + decorateToStringValue(prettifier, map2) + " did not contain element (twenty two,22)")

        val map3 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught3 = intercept[TestFailedException] {
          map3 should (contain ("fifty five" -> 55) or contain ("twenty two" -> 22))
        }
        assert(caught3.getMessage === decorateToStringValue(prettifier, map3) + " did not contain element (fifty five,55), and " + decorateToStringValue(prettifier, map3) + " did not contain element (twenty two,22)")
      }

      it("should throw an TestFailedException when map contains specified element and used in a logical-and expression with not") {
        val map1 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught1 = intercept[TestFailedException] {
          map1 should { not { contain ("three" -> 3) } and not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === decorateToStringValue(prettifier, map1) + " did not contain element (three,3), but " + decorateToStringValue(prettifier, map1) + " contained element (two,2)")

        val map2 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught2 = intercept[TestFailedException] {
          map2 should ((not contain ("three" -> 3)) and (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === decorateToStringValue(prettifier, map2) + " did not contain element (three,3), but " + decorateToStringValue(prettifier, map2) + " contained element (two,2)")

        val map3 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught3 = intercept[TestFailedException] {
          map3 should (not contain ("three" -> 3) and not contain ("two" -> 2))
        }
        assert(caught3.getMessage === decorateToStringValue(prettifier, map3) + " did not contain element (three,3), but " + decorateToStringValue(prettifier, map3) + " contained element (two,2)")
      }

      it("should throw an TestFailedException when map contains specified element and used in a logical-or expression with not") {
        val map1 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught1 = intercept[TestFailedException] {
          map1 should { not { contain ("two" -> 2) } or not { contain ("two" -> 2) }}
        }
        assert(caught1.getMessage === decorateToStringValue(prettifier, map1) + " contained element (two,2), and " + decorateToStringValue(prettifier, map1) + " contained element (two,2)")

        val map2 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught2 = intercept[TestFailedException] {
          map2 should ((not contain ("two" -> 2)) or (not contain ("two" -> 2)))
        }
        assert(caught2.getMessage === decorateToStringValue(prettifier, map2) + " contained element (two,2), and " + decorateToStringValue(prettifier, map2) + " contained element (two,2)")

        val map3 = mutable.HashMap("one" -> 1, "two" -> 2)
        val caught3 = intercept[TestFailedException] {
          map3 should (not contain ("two" -> 2) or not contain ("two" -> 2))
        }
        assert(caught3.getMessage === decorateToStringValue(prettifier, map3) + " contained element (two,2), and " + decorateToStringValue(prettifier, map3) + " contained element (two,2)")
      }

      // SKIP-SCALATESTJS,NATIVE-START
      it("should work on parallel form") {
        mutable.HashMap("one" -> 1, "two" -> 2).par should contain ("two" -> 2)
      }
      // SKIP-SCALATESTJS,NATIVE-END
    }

    // SKIP-SCALATESTJS,NATIVE-START
    describe("on java.util.Set") {

      val javaSet: java.util.Set[Int] = new java.util.HashSet
      javaSet.add(1)
      javaSet.add(2)

      it("should do nothing if list contains the specified element") {
        javaSet should contain (2)
        javaSet should (contain (2))
      }

      it("should do nothing if list does not contain the element and used with should not") {
        javaSet should (not contain (3))
        javaSet should not { contain (3) }
        javaSet should not contain (3)
      }

      it("should do nothing when list contains the specified element and used in a logical-and expression") {
        javaSet should { contain (2) and (contain (1)) }
        javaSet should ((contain (2)) and (contain (1)))
        javaSet should (contain (2) and contain (1))
       }

      it("should do nothing when list contains the specified element and used in a logical-or expression") {
        javaSet should { contain (77) or (contain (2)) }
        javaSet should ((contain (77)) or (contain (2)))
        javaSet should (contain (77) or contain (2))
      }

      it("should do nothing when list doesn't contain the specified element and used in a logical-and expression with not") {
        javaSet should { not { contain (5) } and not { contain (3) }}
        javaSet should ((not contain (5)) and (not contain (3)))
        javaSet should (not contain (5) and not contain (3))
      }

      it("should do nothing when list doesn't contain the specified element and used in a logical-or expression with not") {
        javaSet should { not { contain (1) } or not { contain (3) }}
        javaSet should ((not contain (1)) or (not contain (3)))
        javaSet should (not contain (3) or not contain (2))
      }

      it("should throw TestFailedException if list does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          javaSet should contain (3)
        }
        assert(caught.getMessage endsWith "] did not contain element 3")
      }

      it("should throw TestFailedException if list contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          javaSet should not contain (2)
        }
        assert(caught1.getMessage endsWith "] contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaSet should not (contain (2))
        }
        assert(caught2.getMessage endsWith "] contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaSet should (not contain (2))
        }
        assert(caught3.getMessage endsWith "] contained element 2")
      }

      it("should throw a TestFailedException when list doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          javaSet should { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage endsWith "] did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          javaSet should (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage endsWith "] did not contain element 5")
      }

      it("should throw a TestFailedException when list doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          javaSet should { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage endsWith "] did not contain element 22")
        assert(caught1.getMessage.indexOf("] did not contain element 55, and [") != -1)

        val caught2 = intercept[TestFailedException] {
          javaSet should (contain (55) or contain (22))
        }
        assert(caught1.getMessage endsWith "] did not contain element 22")
        assert(caught1.getMessage.indexOf("] did not contain element 55, and [") != -1)
        // assert(caught2.getMessage === "[2, 1] did not contain element 55, and [2, 1] did not contain element 22")
      }

      it("should throw a TestFailedException when list contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          javaSet should { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage endsWith "] contained element 2")
        assert(caught1.getMessage.indexOf("] did not contain element 3, but [") != -1)
        // assert(caught1.getMessage === "[2, 1] did not contain element 3, but [2, 1] contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaSet should ((not contain (3)) and (not contain (2)))
        }
        assert(caught1.getMessage endsWith "] contained element 2")
        assert(caught1.getMessage.indexOf("] did not contain element 3, but [") != -1)
        // assert(caught2.getMessage === "[2, 1] did not contain element 3, but [2, 1] contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaSet should (not contain (3) and not contain (2))
        }
        assert(caught1.getMessage endsWith "] contained element 2")
        assert(caught1.getMessage.indexOf("] did not contain element 3, but [") != -1)
        // assert(caught3.getMessage === "[2, 1] did not contain element 3, but [2, 1] contained element 2")
      }

      it("should throw a TestFailedException when list contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          javaSet should { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage endsWith "] contained element 2")
        assert(caught1.getMessage.indexOf("] contained element 2, and [") != -1)
        // assert(caught1.getMessage === "[2, 1] contained element 2, and [2, 1] contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaSet should ((not contain (2)) or (not contain (2)))
        }
        assert(caught1.getMessage endsWith "] contained element 2")
        assert(caught1.getMessage.indexOf("] contained element 2, and [") != -1)
        // assert(caught2.getMessage === "[2, 1] contained element 2, and [2, 1] contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaSet should (not contain (2) or not contain (2))
        }
        assert(caught1.getMessage endsWith "] contained element 2")
        assert(caught1.getMessage.indexOf("] contained element 2, and [") != -1)
        // assert(caught3.getMessage === "[2, 1] contained element 2, and [2, 1] contained element 2")
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END

/*
    I'm just not going to support this for now. Let them do whatever, and when someone
    comes back with a good suggestion, then I can consider adding it.

    describe("on java.util.Map ") {

      val javaMap: java.util.Map[String, Int] = new java.util.HashMap
      javaMap.put("one",1)
      javaMap.put("two", 2)

      import java.util.Map.Entry

      it("should do nothing if list contains the specified element") {
        javaMap.entrySet should contain ("one" -> 1)
        javaMap.entrySet should (contain ("two" -> 2))
      }

      it("should do nothing if list does not contain the element and used with should not") {
        javaMap should (not contain (3))
        javaMap should not { contain (3) }
        javaMap should not contain (3)
      }

      it("should do nothing when list contains the specified element and used in a logical-and expression") {
        javaMap should { contain (2) and (contain (1)) }
        javaMap should ((contain (2)) and (contain (1)))
        javaMap should (contain (2) and contain (1))
       }

      it("should do nothing when list contains the specified element and used in a logical-or expression") {
        javaMap should { contain (77) or (contain (2)) }
        javaMap should ((contain (77)) or (contain (2)))
        javaMap should (contain (77) or contain (2))
      }

      it("should do nothing when list doesn't contain the specified element and used in a logical-and expression with not") {
        javaMap should { not { contain (5) } and not { contain (3) }}
        javaMap should ((not contain (5)) and (not contain (3)))
        javaMap should (not contain (5) and not contain (3))
      }

      it("should do nothing when list doesn't contain the specified element and used in a logical-or expression with not") {
        javaMap should { not { contain (1) } or not { contain (3) }}
        javaMap should ((not contain (1)) or (not contain (3)))
        javaMap should (not contain (3) or not contain (2))
      }

      it("should throw TestFailedException if list does not contain the specified element") {
        val caught = intercept[TestFailedException] {
          javaMap should contain (3)
        }
        assert(caught.getMessage === "{one=1, two=2} did not contain element 3")
      }

      it("should throw TestFailedException if list contains the specified element, when used with not") {

        val caught1 = intercept[TestFailedException] {
          javaMap should not contain (2)
        }
        assert(caught1.getMessage === "{one=1, two=2} contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaMap should not (contain (2))
        }
        assert(caught2.getMessage === "{one=1, two=2} contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaMap should (not contain (2))
        }
        assert(caught3.getMessage === "{one=1, two=2} contained element 2")
      }

      it("should throw a TestFailedException when list doesn't contain the specified element and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          javaMap should { contain (5) and (contain (2 - 1)) }
        }
        assert(caught1.getMessage === "{one=1, two=2} did not contain element 5")

        val caught2 = intercept[TestFailedException] {
          javaMap should (contain (5) and contain (2 - 1))
        }
        assert(caught2.getMessage === "{one=1, two=2} did not contain element 5")
      }

      it("should throw a TestFailedException when list doesn't contain the specified element and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          javaMap should { contain (55) or (contain (22)) }
        }
        assert(caught1.getMessage === "{one=1, two=2} did not contain element 55, and {one=1, two=2} did not contain element 22")

        val caught2 = intercept[TestFailedException] {
          javaMap should (contain (55) or contain (22))
        }
        assert(caught2.getMessage === "{one=1, two=2} did not contain element 55, and {one=1, two=2} did not contain element 22")
      }

      it("should throw a TestFailedException when list contains the specified element and used in a logical-and expression with not") {

        val caught1 = intercept[TestFailedException] {
          javaMap should { not { contain (3) } and not { contain (2) }}
        }
        assert(caught1.getMessage === "{one=1, two=2} did not contain element 3, but {one=1, two=2} contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaMap should ((not contain (3)) and (not contain (2)))
        }
        assert(caught2.getMessage === "{one=1, two=2} did not contain element 3, but {one=1, two=2} contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaMap should (not contain (3) and not contain (2))
        }
        assert(caught3.getMessage === "{one=1, two=2} did not contain element 3, but {one=1, two=2} contained element 2")
      }

      it("should throw a TestFailedException when list contains the specified element and used in a logical-or expression with not") {

        val caught1 = intercept[TestFailedException] {
          javaMap should { not { contain (2) } or not { contain (2) }}
        }
        assert(caught1.getMessage === "{one=1, two=2} contained element 2, and {one=1, two=2} contained element 2")

        val caught2 = intercept[TestFailedException] {
          javaMap should ((not contain (2)) or (not contain (2)))
        }
        assert(caught2.getMessage === "{one=1, two=2} contained element 2, and {one=1, two=2} contained element 2")

        val caught3 = intercept[TestFailedException] {
          javaMap should (not contain (2) or not contain (2))
        }
        assert(caught3.getMessage === "{one=1, two=2} contained element 2, and {one=1, two=2} contained element 2")
      }
    }
*/
  }
}
