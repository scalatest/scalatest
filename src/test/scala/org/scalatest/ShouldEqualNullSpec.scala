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

import org.scalautils._

class ShouldEqualNullSpec extends Spec with Matchers {

  case class Super(size: Int)
  class Sub(sz: Int) extends Super(sz)

  val super1: Super = new Super(1)
  val sub1: Sub = new Sub(1)
  val super2: Super = new Super(2)
  val sub2: Sub = new Sub(2)
  val nullSuper: Super = null

  object `The should equal syntax` {

    def `should work sensibly if null is passed on the left or right hand side` {

      val caught1 = intercept[TestFailedException] { super1 should equal (null) }
      caught1.getMessage should be ("Super(1) did not equal null")
      val caught2 = intercept[TestFailedException] { super1 should (equal (null)) }
      caught2.getMessage should be ("Super(1) did not equal null")
      val caught1b = intercept[TestFailedException] { super1 shouldEqual null }
      caught1b.getMessage should be ("Super(1) did not equal null")

      super1 should not equal (null)
      super1 should not (equal (null))
      super1 should (not (equal (null)))
      super1 should (not equal (null))

      nullSuper should equal (null)
      nullSuper shouldEqual null
      nullSuper should (equal (null))
      nullSuper should not equal (super1)
      nullSuper should not (equal (super1))
      nullSuper should (not equal (super1))
      nullSuper should (not (equal (super1)))

      val caught3 = intercept[TestFailedException] { nullSuper should not equal (null) }
      caught3.getMessage should be ("null equaled null")
      val caught4 = intercept[TestFailedException] { nullSuper should not (equal (null)) }
      caught4.getMessage should be ("The reference equaled null")
      val caught5 = intercept[TestFailedException] { nullSuper should (not equal (null)) }
      caught5.getMessage should be ("The reference equaled null")
      val caught6 = intercept[TestFailedException] { nullSuper should (not (equal (null))) }
      caught6.getMessage should be ("The reference equaled null")
      val caught7 = intercept[TestFailedException] { nullSuper should equal (super1) }
      caught7.getMessage should be ("null did not equal Super(1)")
      val caught8 = intercept[TestFailedException] { nullSuper should (equal (super1)) }
      caught8.getMessage should be ("null did not equal Super(1)")
    }

    def `should work sensibly if null is passed on the left or right hand side, when used with logical and` {

      val caught1 = intercept[TestFailedException] { super1 should (equal (null) and equal (null)) }
      caught1.getMessage should be ("Super(1) did not equal null")
      val caught2 = intercept[TestFailedException] { super1 should (equal (super1) and equal (null)) }
      caught2.getMessage should be ("Super(1) equaled Super(1), but Super(1) did not equal null")
      super1 should not (equal (null) and equal (null))
      val caught3 = intercept[TestFailedException] { nullSuper should (not equal (null)) }
      caught3.getMessage should be ("The reference equaled null")
      val caught4 = intercept[TestFailedException] { nullSuper should (equal (null) and not (equal (null))) }
      caught4.getMessage should be ("The reference equaled null, but the reference equaled null")
      val caught5 = intercept[TestFailedException] { nullSuper should (equal (null) and not equal (null)) }
      caught5.getMessage should be ("The reference equaled null, but the reference equaled null")
    }

    def `should work sensibly if null is passed on the left or right hand side, when used with logical or` {

      val caught1 = intercept[TestFailedException] { super1 should (equal (null) or equal (null)) }
      caught1.getMessage should be ("Super(1) did not equal null, and Super(1) did not equal null")
      val caught2 = intercept[TestFailedException] { super1 should (equal (null) or (equal (null))) }
      caught2.getMessage should be ("Super(1) did not equal null, and Super(1) did not equal null")

      super1 should not (equal (null) or equal (null))
      super1 should not (equal (null) or (equal (null)))

      super1 should (equal (null) or equal (super1))
      super1 should (equal (super1) or equal (null))
      super1 should (equal (null) or (equal (super1)))
      super1 should (equal (super1) or (equal (null)))

      val caught3 = intercept[TestFailedException] { nullSuper should (not equal (null) or not (equal (null))) }
      caught3.getMessage should be ("The reference equaled null, and the reference equaled null")
      val caught4 = intercept[TestFailedException] { nullSuper should (not equal (null) or not (equal (null))) }
      caught4.getMessage should be ("The reference equaled null, and the reference equaled null")
      val caught5 = intercept[TestFailedException] { nullSuper should (not equal (null) or (not equal (null))) }
      caught5.getMessage should be ("The reference equaled null, and the reference equaled null")
      val caught6 = intercept[TestFailedException] { nullSuper should (not equal (null) or (not (equal (null)))) }
      caught6.getMessage should be ("The reference equaled null, and the reference equaled null")
      val caught7 = intercept[TestFailedException] { nullSuper should (not equal (null) or not equal (null)) }
      caught7.getMessage should be ("The reference equaled null, and the reference equaled null")
    }
  }
}

