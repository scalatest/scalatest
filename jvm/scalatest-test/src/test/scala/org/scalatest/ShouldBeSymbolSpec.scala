/*
 * Copyright 2001-2024 Artima, Inc.
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
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldBeSymbolSpec extends AnyFunSpec with EmptyMocks {

  describe("The be ('symbol) syntax") {

    it("should do nothing if the object has an appropriately named method, which returns true") {
      emptyMock should be (Symbol("empty"))
      isEmptyMock should be (Symbol("empty"))
    }

    it("should throw TestFailedException with an appropriate error message if the object has an appropriately named method, but it returns false") {
      val ex5 = intercept[TestFailedException] {
        List(1, 2) should be (Symbol("empty"))
      }
      assert(ex5.message === Some("List(1, 2) was not empty"))
      assert(ex5.failedCodeFileName === Some("ShouldBeSymbolSpec.scala"))
      assert(ex5.failedCodeLineNumber === Some(thisLineNumber - 4))
    }

    it("should throw TestFailedException if no <symbol> or is<Symbol> method exists") {
      val ex1 = intercept[TestFailedException] {
        noPredicateMock should be (Symbol("empty"))
      }
      ex1.getMessage should equal ("NoPredicateMock has neither an empty nor an isEmpty method")
      // Check message for name that starts with a consonant (should use a instead of an)
      val ex2 = intercept[TestFailedException] {
        noPredicateMock should be (Symbol("full"))
      }
      ex2.getMessage should equal ("NoPredicateMock has neither a full nor an isFull method")
    }

    it("should do nothing if the object has an appropriately named method, which returns true, even if the method contains operator characters") {

      val opNames = new OperatorNames

      opNames should be (Symbol("op_21_!"))
      opNames should be (Symbol("op_23_#"))
      opNames should be (Symbol("op_25_%"))
      opNames should be (Symbol("op_26_&"))
      opNames should be (Symbol("op_2a_*"))
      opNames should be (Symbol("op_2b_+"))
      opNames should be (Symbol("op_2d_-"))
      opNames should be (Symbol("op_2f_/"))
      opNames should be (Symbol("op_3a_:"))
      opNames should be (Symbol("op_3c_<"))
      opNames should be (Symbol("op_3d_="))
      opNames should be (Symbol("op_3e_>"))
      opNames should be (Symbol("op_3f_?"))
      opNames should be (Symbol("op_40_@"))
      opNames should be (Symbol("op_5c_\\"))
      opNames should be (Symbol("op_5e_^"))
      opNames should be (Symbol("op_7c_|"))
      opNames should be (Symbol("op_7e_~"))

      opNames should be (Symbol("!!!"))
      opNames should be (Symbol("###"))
      opNames should be (Symbol("%%%"))
      opNames should be (Symbol("&&&"))
      opNames should be (Symbol("***"))
      opNames should be (Symbol("+++"))
      opNames should be (Symbol("---"))
      opNames should be (Symbol("/"))
      opNames should be (Symbol(":::"))
      opNames should be (Symbol("<<<"))
      opNames should be (Symbol("==="))
      opNames should be (Symbol(">>>"))
      opNames should be (Symbol("???"))
      opNames should be (Symbol("@@@"))
      opNames should be (Symbol("\\\\\\"))
      opNames should be (Symbol("^^^"))
      opNames should be (Symbol("|||"))
      opNames should be (Symbol("~~~"))
    }

    it("should do nothing if the object has an appropriately named method, which returns false when used with not") {
      notEmptyMock should not { be (Symbol("empty")) }
      notEmptyMock should not be (Symbol("empty"))
      isNotEmptyMock should not { be (Symbol("empty")) }
      isNotEmptyMock should not be (Symbol("empty"))
    }

    it("should throw TestFailedException if no <symbol> or is<Symbol> method exists, when used with not") {
      val ex1 = intercept[TestFailedException] {
        noPredicateMock should not { be (Symbol("empty")) }
      }
      ex1.message should equal (Some("NoPredicateMock has neither an empty nor an isEmpty method"))
      ex1.failedCodeFileName should equal (Some("ShouldBeSymbolSpec.scala"))
      ex1.failedCodeLineNumber should equal (Some(thisLineNumber - 4))

      val ex2 = intercept[TestFailedException] {
        noPredicateMock should not (be (Symbol("full")))
      }
      ex2.message should equal (Some("NoPredicateMock has neither a full nor an isFull method"))
      ex2.failedCodeFileName should equal (Some("ShouldBeSymbolSpec.scala"))
      ex2.failedCodeLineNumber should equal (Some(thisLineNumber - 4))

      val ex3 = intercept[TestFailedException] {
        noPredicateMock should not be (Symbol("empty"))
      }
      ex3.message should equal (Some("NoPredicateMock has neither an empty nor an isEmpty method"))
      ex3.failedCodeFileName should equal (Some("ShouldBeSymbolSpec.scala"))
      ex3.failedCodeLineNumber should equal (Some(thisLineNumber - 4))

      val ex4 = intercept[TestFailedException] {
        noPredicateMock should not be (Symbol("full"))
      }
      ex4.message should equal (Some("NoPredicateMock has neither a full nor an isFull method"))
      ex4.failedCodeFileName should equal (Some("ShouldBeSymbolSpec.scala"))
      ex4.failedCodeLineNumber should equal (Some(thisLineNumber - 4))
    }

    it("should do nothing if the object has an appropriately named method, which returns true, when used in a logical-and expression") {
      emptyMock should ((be (Symbol("empty"))) and (be (Symbol("empty"))))
      emptyMock should (be (Symbol("empty")) and (be (Symbol("empty"))))
      emptyMock should (be (Symbol("empty")) and be (Symbol("empty")))
      isEmptyMock should ((be (Symbol("empty"))) and (be (Symbol("empty"))))
      isEmptyMock should (be (Symbol("empty")) and (be (Symbol("empty"))))
      isEmptyMock should (be (Symbol("empty")) and be (Symbol("empty")))
    }

    it("should do nothing if the object has an appropriately named method, which returns true, when used in a logical-or expression") {

      emptyMock should ((be (Symbol("full"))) or (be (Symbol("empty"))))
      emptyMock should (be (Symbol("full")) or (be (Symbol("empty"))))
      emptyMock should (be (Symbol("full")) or be (Symbol("empty")))
      isEmptyMock should ((be (Symbol("full"))) or (be (Symbol("empty"))))
      isEmptyMock should (be (Symbol("full")) or (be (Symbol("empty"))))
      isEmptyMock should (be (Symbol("full")) or be (Symbol("empty")))

      emptyMock should ((be (Symbol("empty"))) or (be (Symbol("full"))))
      emptyMock should (be (Symbol("empty")) or (be (Symbol("full"))))
      emptyMock should (be (Symbol("empty")) or be (Symbol("full")))
      isEmptyMock should ((be (Symbol("empty"))) or (be (Symbol("full"))))
      isEmptyMock should (be (Symbol("empty")) or (be (Symbol("full"))))
      isEmptyMock should (be (Symbol("empty")) or be (Symbol("full")))
    }

    it("should do nothing if the object has an appropriately named method, which returns false, when used in a logical-and expression with not") {

      notEmptyMock should (not (be (Symbol("empty"))) and not (be (Symbol("empty"))))
      notEmptyMock should ((not be (Symbol("empty"))) and (not be (Symbol("empty"))))
      notEmptyMock should (not be (Symbol("empty")) and not be (Symbol("empty")))

      isNotEmptyMock should (not (be (Symbol("empty"))) and not (be (Symbol("empty"))))
      isNotEmptyMock should ((not be (Symbol("empty"))) and (not be (Symbol("empty"))))
      isNotEmptyMock should (not be (Symbol("empty")) and not be (Symbol("empty")))
    }

    it("should do nothing if the object has an appropriately named method, which returns false, when used in a logical-or expression with not") {

      notEmptyMock should (not (be (Symbol("empty"))) or not (be (Symbol("empty"))))
      notEmptyMock should ((not be (Symbol("empty"))) or (not be (Symbol("empty"))))
      notEmptyMock should (not be (Symbol("empty")) or not be (Symbol("empty")))

      isNotEmptyMock should (not (be (Symbol("empty"))) or not (be (Symbol("empty"))))
      isNotEmptyMock should ((not be (Symbol("empty"))) or (not be (Symbol("empty"))))
      isNotEmptyMock should (not be (Symbol("empty")) or not be (Symbol("empty")))

      notEmptyMock should (not (be (Symbol("full"))) or not (be (Symbol("empty"))))
      notEmptyMock should ((not be (Symbol("full"))) or (not be (Symbol("empty"))))
      notEmptyMock should (not be (Symbol("full")) or not be (Symbol("empty")))

      isNotEmptyMock should (not (be (Symbol("full"))) or not (be (Symbol("empty"))))
      isNotEmptyMock should ((not be (Symbol("full"))) or (not be (Symbol("empty"))))
      isNotEmptyMock should (not be (Symbol("full")) or not be (Symbol("empty")))
    }

    it("should throw TestFailedException if the object has an appropriately named method, which returns false") {
      val caught1 = intercept[TestFailedException] {
        notEmptyMock should be (Symbol("empty"))
      }
      assert(caught1.getMessage === "NotEmptyMock was not empty")
      val caught2 = intercept[TestFailedException] {
        isNotEmptyMock should be (Symbol("empty"))
      }
      assert(caught2.getMessage === "IsNotEmptyMock was not empty")
    }

    it("should throw TestFailedException if the object has an appropriately named method, which returns true when used with not") {
      val caught1 = intercept[TestFailedException] {
        emptyMock should not { be (Symbol("empty")) }
      }
      assert(caught1.getMessage === "EmptyMock was empty")
      val caught2 = intercept[TestFailedException] {
        emptyMock should not be (Symbol("empty"))
      }
      assert(caught2.getMessage === "EmptyMock was empty")
      val caught3 = intercept[TestFailedException] {
        isEmptyMock should not { be (Symbol("empty")) }
      }
      assert(caught3.getMessage === "IsEmptyMock was empty")
      val caught4 = intercept[TestFailedException] {
        isEmptyMock should not be (Symbol("empty"))
      }
      assert(caught4.getMessage === "IsEmptyMock was empty")
    }

    it("should throw TestFailedException if the object has an appropriately named method, which returns false, when used in a logical-and expression") {
      val caught1 = intercept[TestFailedException] {
        emptyMock should ((be (Symbol("empty"))) and (be (Symbol("full"))))
      }
      assert(caught1.getMessage === "EmptyMock was empty, but EmptyMock was not full")
      val caught2 = intercept[TestFailedException] {
        emptyMock should (be (Symbol("empty")) and (be (Symbol("full"))))
      }
      assert(caught2.getMessage === "EmptyMock was empty, but EmptyMock was not full")
      val caught3 = intercept[TestFailedException] {
        emptyMock should (be (Symbol("empty")) and be (Symbol("full")))
      }
      assert(caught3.getMessage === "EmptyMock was empty, but EmptyMock was not full")
      val caught4 = intercept[TestFailedException] {
        isEmptyMock should ((be (Symbol("empty"))) and (be (Symbol("full"))))
      }
      assert(caught4.getMessage === "IsEmptyMock was empty, but IsEmptyMock was not full")
      val caught5 = intercept[TestFailedException] {
        isEmptyMock should (be (Symbol("empty")) and (be (Symbol("full"))))
      }
      assert(caught5.getMessage === "IsEmptyMock was empty, but IsEmptyMock was not full")
      val caught6 = intercept[TestFailedException] {
        isEmptyMock should (be (Symbol("empty")) and be (Symbol("full")))
      }
      assert(caught6.getMessage === "IsEmptyMock was empty, but IsEmptyMock was not full")
    }

    it("should throw TestFailedException if the object has an appropriately named method, which returns false, when used in a logical-or expression") {

      val caught1 = intercept[TestFailedException] {
        notEmptyMock should ((be (Symbol("empty"))) or (be (Symbol("empty"))))
      }
      assert(caught1.getMessage === "NotEmptyMock was not empty, and NotEmptyMock was not empty")
      val caught2 = intercept[TestFailedException] {
        notEmptyMock should (be (Symbol("empty")) or (be (Symbol("empty"))))
      }
      assert(caught2.getMessage === "NotEmptyMock was not empty, and NotEmptyMock was not empty")
      val caught3 = intercept[TestFailedException] {
        notEmptyMock should (be (Symbol("empty")) or be (Symbol("empty")))
      }
      assert(caught3.getMessage === "NotEmptyMock was not empty, and NotEmptyMock was not empty")
      val caught4 = intercept[TestFailedException] {
        isNotEmptyMock should ((be (Symbol("empty"))) or (be (Symbol("empty"))))
      }
      assert(caught4.getMessage === "IsNotEmptyMock was not empty, and IsNotEmptyMock was not empty")
      val caught5 = intercept[TestFailedException] {
        isNotEmptyMock should (be (Symbol("empty")) or (be (Symbol("empty"))))
      }
      assert(caught5.getMessage === "IsNotEmptyMock was not empty, and IsNotEmptyMock was not empty")
      val caught6 = intercept[TestFailedException] {
        isNotEmptyMock should (be (Symbol("empty")) or be (Symbol("empty")))
      }
      assert(caught6.getMessage === "IsNotEmptyMock was not empty, and IsNotEmptyMock was not empty")
    }

    it("should throw TestFailedException if the object has an appropriately named method, which returns true, when used in a logical-and expression with not") {

      val caught1 = intercept[TestFailedException] {
        emptyMock should (not (be (Symbol("full"))) and not (be (Symbol("empty"))))
      }
      assert(caught1.getMessage === "EmptyMock was not full, but EmptyMock was empty")
      val caught2 = intercept[TestFailedException] {
        emptyMock should ((not be (Symbol("full"))) and (not be (Symbol("empty"))))
      }
      assert(caught2.getMessage === "EmptyMock was not full, but EmptyMock was empty")
      val caught3 = intercept[TestFailedException] {
        emptyMock should (not be (Symbol("full")) and not be (Symbol("empty")))
      }
      assert(caught3.getMessage === "EmptyMock was not full, but EmptyMock was empty")
      val caught4 = intercept[TestFailedException] {
        isEmptyMock should (not (be (Symbol("full"))) and not (be (Symbol("empty"))))
      }
      assert(caught4.getMessage === "IsEmptyMock was not full, but IsEmptyMock was empty")
      val caught5 = intercept[TestFailedException] {
        isEmptyMock should ((not be (Symbol("full"))) and (not be (Symbol("empty"))))
      }
      assert(caught5.getMessage === "IsEmptyMock was not full, but IsEmptyMock was empty")
      val caught6 = intercept[TestFailedException] {
        isEmptyMock should (not be (Symbol("full")) and not be (Symbol("empty")))
      }
      assert(caught6.getMessage === "IsEmptyMock was not full, but IsEmptyMock was empty")
      // Check that the error message "short circuits"
      val caught7 = intercept[TestFailedException] {
        emptyMock should (not (be (Symbol("empty"))) and not (be (Symbol("full"))))
      }
      assert(caught7.getMessage === "EmptyMock was empty")
    }

    it("should throw TestFailedException if the object has an appropriately named method, which returns true, when used in a logical-or expression with not") {

      val caught1 = intercept[TestFailedException] {
        emptyMock should (not (be (Symbol("empty"))) or not (be (Symbol("empty"))))
      }
      assert(caught1.getMessage === "EmptyMock was empty, and EmptyMock was empty")
      val caught2 = intercept[TestFailedException] {
        emptyMock should ((not be (Symbol("empty"))) or (not be (Symbol("empty"))))
      }
      assert(caught2.getMessage === "EmptyMock was empty, and EmptyMock was empty")
      val caught3 = intercept[TestFailedException] {
        emptyMock should (not be (Symbol("empty")) or not be (Symbol("empty")))
      }
      assert(caught3.getMessage === "EmptyMock was empty, and EmptyMock was empty")
      val caught4 = intercept[TestFailedException] {
        isEmptyMock should (not (be (Symbol("empty"))) or not (be (Symbol("empty"))))
      }
      assert(caught4.getMessage === "IsEmptyMock was empty, and IsEmptyMock was empty")
      val caught5 = intercept[TestFailedException] {
        isEmptyMock should ((not be (Symbol("empty"))) or (not be (Symbol("empty"))))
      }
      assert(caught5.getMessage === "IsEmptyMock was empty, and IsEmptyMock was empty")
      val caught6 = intercept[TestFailedException] {
        isEmptyMock should (not be (Symbol("empty")) or not be (Symbol("empty")))
      }
      assert(caught6.getMessage === "IsEmptyMock was empty, and IsEmptyMock was empty")
    }

    describe("(for the different types that have implicit conversions for should methods)") {

      // FOR: implicit def convertToCollectionShouldWrapper[T](o: Collection[T])...
      it("should work on a scala.Collection") {
        val emptySet = Set[Int]()
        emptySet should be (Symbol("empty"))
        val nonEmptySet = Set(1, 2, 3)
        nonEmptySet should not { be (Symbol("empty")) }
        val caught1 = intercept[TestFailedException] {
          nonEmptySet should be (Symbol("empty"))
        }
        assert(caught1.getMessage === "Set(1, 2, 3) was not empty")
        val caught2 = intercept[TestFailedException] {
          nonEmptySet should not { be (Symbol("hasDefiniteSize")) }
        }
        assert(caught2.getMessage === "Set(1, 2, 3) was hasDefiniteSize")
        val caught3 = intercept[TestFailedException] {
          nonEmptySet should not { be (Symbol("happy")) }
        }
        assert(caught3.getMessage === "Set(1, 2, 3) has neither a happy nor an isHappy method")
      }

      // FOR: implicit def convertToSeqShouldWrapper[T](o: Seq[T])...
      it("should work on a scala.Seq") {
        import scala.collection.mutable.ListBuffer
        val emptyListBuffer = new ListBuffer[Int]
        emptyListBuffer should be (Symbol("empty"))
        val nonEmptyListBuffer = new ListBuffer[Int]
        nonEmptyListBuffer += 1
        nonEmptyListBuffer += 2
        nonEmptyListBuffer += 3
        nonEmptyListBuffer should not { be (Symbol("empty")) }
        val caught1 = intercept[TestFailedException] {
          nonEmptyListBuffer should be (Symbol("empty"))
        }
        assert(caught1.getMessage === "ListBuffer(1, 2, 3) was not empty")
        val caught2 = intercept[TestFailedException] {
          nonEmptyListBuffer should not { be (Symbol("hasDefiniteSize")) }
        }
        assert(caught2.getMessage === "ListBuffer(1, 2, 3) was hasDefiniteSize")
        val caught3 = intercept[TestFailedException] {
          nonEmptyListBuffer should not { be (Symbol("happy")) }
        }
        assert(caught3.getMessage === "ListBuffer(1, 2, 3) has neither a happy nor an isHappy method")
      }

      // implicit def convertToArrayShouldWrapper[T](o: Array[T]): ArrayShouldWrapper[T] = new ArrayShouldWrapper[T](o)
/* This no longer works as of Scala 2.8
      it("should work on a scala.Array") {
        val emptyArray = new Array[Int](0)
        emptyArray should be (Symbol("empty"))
        val nonEmptyArray = Array(1, 2, 3)
        nonEmptyArray should not be (Symbol("empty"))
        val caught1 = intercept[TestFailedException] {
          nonEmptyArray should be (Symbol("empty"))
        }
        assert(caught1.getMessage === "Array(1, 2, 3) was not empty")
        val caught2 = intercept[TestFailedException] {
          nonEmptyArray should not { be ('hasDefiniteSize) }
        }
        assert(caught2.getMessage === "Array(1, 2, 3) was hasDefiniteSize")
        val caught3 = intercept[TestFailedException] {
          nonEmptyArray should not { be (Symbol("happy")) }
        }
        assert(caught3.getMessage === "Array(1, 2, 3) has neither a happy nor an isHappy method")
      }
*/

      // FOR: implicit def convertToListShouldWrapper[T](o: List[T])...
      it("should work on a scala.List") {
        val emptyList = List[Int]()
        emptyList should be (Symbol("empty"))
        val nonEmptyList = List(1, 2, 3)
        nonEmptyList should not { be (Symbol("empty")) }
        val caught1 = intercept[TestFailedException] {
          nonEmptyList should be (Symbol("empty"))
        }
        assert(caught1.getMessage === "List(1, 2, 3) was not empty")
        val caught2 = intercept[TestFailedException] {
          nonEmptyList should not { be (Symbol("hasDefiniteSize")) }
        }
        assert(caught2.getMessage === "List(1, 2, 3) was hasDefiniteSize")
        val caught3 = intercept[TestFailedException] {
          nonEmptyList should not { be (Symbol("happy")) }
        }
        assert(caught3.getMessage === "List(1, 2, 3) has neither a happy nor an isHappy method")
      }

      // implicit def convertToMapShouldWrapper[K, V](o: scala.collection.Map[K, V])...
      it("should work on a scala.Map") {
        val emptyMap = Map[Int, String]()
        emptyMap should be (Symbol("empty"))
        val nonEmptyMap = Map("one" -> 1, "two" -> 2, "three" -> 3)
        nonEmptyMap should not { be (Symbol("empty")) }
        val caught1 = intercept[TestFailedException] {
          nonEmptyMap should be (Symbol("empty"))
        }
        assert(caught1.getMessage === "Map(\"one\" -> 1, \"two\" -> 2, \"three\" -> 3) was not empty")
        val caught2 = intercept[TestFailedException] {
          nonEmptyMap should not { be (Symbol("hasDefiniteSize")) }
        }
        assert(caught2.getMessage === "Map(\"one\" -> 1, \"two\" -> 2, \"three\" -> 3) was hasDefiniteSize")
        val caught3 = intercept[TestFailedException] {
          nonEmptyMap should not { be (Symbol("happy")) }
        }
        assert(caught3.getMessage === "Map(\"one\" -> 1, \"two\" -> 2, \"three\" -> 3) has neither a happy nor an isHappy method")
      }

      // implicit def convertToStringShouldWrapper[K, V](o: String): StringShouldWrapper = new StringShouldWrapper(o)
      it("should work on a String") {
        val caught3 = intercept[TestFailedException] {
          "howdy" should not be (Symbol("happy"))
        }
        assert(caught3.getMessage === "\"howdy\" has neither a happy nor an isHappy method")
      }

      // FOR: implicit def convertToJavaCollectionShouldWrapper[T](o: java.util.Collection[T])...
      it("should work on a java.util.Collection") {
        val emptySet = new java.util.HashSet[Int]
        emptySet should be (Symbol("empty"))
        val nonEmptySet = new java.util.HashSet[Int]
        nonEmptySet.add(1)
        nonEmptySet.add(2)
        nonEmptySet.add(3)
        nonEmptySet should not { be (Symbol("empty")) }
        val caught1 = intercept[TestFailedException] {
          nonEmptySet should be (Symbol("empty"))
        }
        assert(caught1.getMessage endsWith "] was not empty")
        val caught3 = intercept[TestFailedException] {
          nonEmptySet should not { be (Symbol("happy")) }
        }
        assert(caught3.getMessage endsWith "] has neither a happy nor an isHappy method")
      }

      // FOR: implicit def convertToJavaListShouldWrapper[T](o: java.util.List[T])...
      it("should work on a java.util.List") {
        val emptyList = new java.util.ArrayList[Int]
        emptyList should be (Symbol("empty"))
        val nonEmptyList = new java.util.ArrayList[Int]
        nonEmptyList.add(1)
        nonEmptyList.add(2)
        nonEmptyList.add(3)
        nonEmptyList should not { be (Symbol("empty")) }
        val caught1 = intercept[TestFailedException] {
          nonEmptyList should be (Symbol("empty"))
        }
        assert(caught1.getMessage === "[1, 2, 3] was not empty")
        val caught3 = intercept[TestFailedException] {
          nonEmptyList should not { be (Symbol("happy")) }
        }
        assert(caught3.getMessage === "[1, 2, 3] has neither a happy nor an isHappy method")
      }
    }
  }

  describe("The be matcher") {

    describe("(for symbols)") {

      // TODO: Make sure to write a test for each conversion, because some are using ShouldMethodsForAny instead
      // of ShouldMethodsForAnyRef.
      it("should be invokable from be a Symbol and be an Symbol") {
        val emptySet = Set()
        emptySet should be a (Symbol("empty"))
        emptySet should be an (Symbol("empty"))
        val nonEmptySet = Set(1, 2, 3)
        nonEmptySet should not { be a (Symbol("empty")) }
        nonEmptySet should not { be an (Symbol("empty")) }
      }

      it("should call empty when passed Symbol(\"empty\")") {
        class EmptyMock {
          def empty: Boolean = true
        }
        class NonEmptyMock {
          def empty: Boolean = false
        }
        (new EmptyMock) should be (Symbol("empty"))
        (new NonEmptyMock) should not { be (Symbol("empty")) }
        // (new NonEmptyMock) shouldNot be (Symbol("empty"))
      }

// STOLE FROM HERE
      it("should call the Scala=style method if both an empty and an isEmpty method exist") {
        class EmptyMock {
          def empty: Boolean = true
          def isEmpty: Boolean = false
          override def toString = "EmptyMock"
        }
        class NonEmptyMock {
          def empty: Boolean = false
          def isEmpty: Boolean = true
          override def toString = "NonEmptyMock"
        }
        (new EmptyMock) should be (Symbol("empty"))
        (new NonEmptyMock) should not { be (Symbol("empty")) }
      }

      it("should access an Symbol(\"empty\")' val when passed Symbol(\"empty\")") {
        class EmptyMock {
          val empty: Boolean = true
        }
        class NonEmptyMock {
          val empty: Boolean = false
        }
        (new EmptyMock) should be (Symbol("empty"))
        (new NonEmptyMock) should not { be (Symbol("empty")) }
      }
    }
  }

  describe("the be (Symbol(\"empty\")) syntax") {

    it("should call isEmpty") {
      val emptySet = Set[Int]()
      emptySet should be (Symbol("empty"))
      val nonEmptySet = Set(1, 2, 3)
      nonEmptySet should not { be (Symbol("empty")) }
    }

    it("should call empty when passed Symbol(\"empty\")") {
      class EmptyMock {
        def empty: Boolean = true
      }
      class NonEmptyMock {
        def empty: Boolean = false
      }
      (new EmptyMock) should be (Symbol("empty"))
      (new NonEmptyMock) should not { be (Symbol("empty")) }
      // (new NonEmptyMock) shouldNot be (Symbol("empty"))
    }

    it("should throw TestFailedException if no empty or isEmpty method") {
      class EmptyMock {
        override def toString = "EmptyMock"
      }
      class NonEmptyMock {
        override def toString = "NonEmptyMock"
      }
      val ex1 = intercept[TestFailedException] {
        (new EmptyMock) should be (Symbol("empty"))
      }
      ex1.getMessage should equal ("EmptyMock has neither an empty nor an isEmpty method")
      val ex2 = intercept[TestFailedException] {
        (new NonEmptyMock) should not { be (Symbol("empty")) }
      }
      ex2.getMessage should equal ("NonEmptyMock has neither an empty nor an isEmpty method")
    }

    it("should call the Scala-style method if both an empty and an isEmpty method exist") {
      class EmptyMock {
        def empty: Boolean = true
        def isEmpty: Boolean = false
        override def toString = "EmptyMock"
      }
      class NonEmptyMock {
        def empty: Boolean = false
        def isEmpty: Boolean = true
        override def toString = "NonEmptyMock"
      }
      (new EmptyMock) should be (Symbol("empty"))
      (new NonEmptyMock) should not { be (Symbol("empty")) }
    }

    it("should access an Symbol(\"empty\")' val when passed Symbol(\"empty\")") {
      class EmptyMock {
        val empty: Boolean = true
      }
      class NonEmptyMock {
        val empty: Boolean = false
      }
      (new EmptyMock) should be (Symbol("empty"))
      (new NonEmptyMock) should not { be (Symbol("empty")) }
      // (new NonEmptyMock) shouldNot be (Symbol("empty"))
    }
  }

  describe("The be Symbol(\"defined\") syntax") {

    it("should do nothing when used with a Some") {
      val someString: Some[String] = Some("hi")
      someString should be (Symbol("defined"))
      val optionString: Option[String] = Some("hi")
      optionString should be (Symbol("defined"))
    }

    it("should throw TestFailedException when used with a None") {
      val none: None.type = None
      val caught1 = intercept[TestFailedException] {
        none should be (Symbol("defined"))
      }
      assert(caught1.getMessage === "None was not defined")
      val option: Option[Int] = None
      val caught2 = intercept[TestFailedException] {
        option should be (Symbol("defined"))
      }
      assert(caught2.getMessage === "None was not defined")
    }

    it("should call defined") {
      class DefinedMock {
        def defined: Boolean = true
      }
      class NonDefinedMock {
        def defined: Boolean = false
      }
      (new DefinedMock) should be (Symbol("defined"))
      (new NonDefinedMock) should not { be (Symbol("defined")) }
      // (new NonDefinedMock) shouldNot be (Symbol("defined"))
    }

    it("should throw TestFailedException if no defined or isDefined method") {
      class DefinedMock {
        override def toString = "DefinedMock"
      }
      class NonDefinedMock {
        override def toString = "NonDefinedMock"
      }
      val ex1 = intercept[TestFailedException] {
        (new DefinedMock) should be (Symbol("defined"))
      }
      ex1.getMessage should equal ("DefinedMock has neither a defined nor an isDefined method")
      val ex2 = intercept[TestFailedException] {
        (new NonDefinedMock) should not { be (Symbol("defined")) }
      }
      ex2.getMessage should equal ("NonDefinedMock has neither a defined nor an isDefined method")
    }

    it("should call the Scala-style method if both a defined and an isDefined method exist") {
      class DefinedMock {
        def defined: Boolean = true
        def isDefined: Boolean = false
        override def toString = "DefinedMock"
      }
      class NonDefinedMock {
        def defined: Boolean = false
        def isDefined: Boolean = true
        override def toString = "NonDefinedMock"
      }
      (new DefinedMock) should be (Symbol("defined"))
      (new NonDefinedMock) should not { be (Symbol("defined")) }
    }

    it("should access an Symbol(\"defined\")' val") {
      class DefinedMock {
        val defined: Boolean = true
      }
      class NonDefinedMock {
        val defined: Boolean = false
      }
      (new DefinedMock) should be (Symbol("defined"))
      (new NonDefinedMock) should not { be (Symbol("defined")) }
      // (new NonDefinedMock) shouldNot be (Symbol("defined"))
    }
  }
/*
  describe("The "be symbol" syntax") {
    it("should be usable on Any") {
      import scala.collection.GenTraversableOnce
      val travOnce: GenTraversableOnce[Int] = List(1, 2, 3)
      travOnce shouldBe 'traversableAgain
    }
  }
*/
}
