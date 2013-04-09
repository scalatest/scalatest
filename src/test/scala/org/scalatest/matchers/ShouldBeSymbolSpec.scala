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

class ShouldBeSymbolSpec extends Spec with ShouldMatchers with EmptyMocks {

  object `The be ('symbol) syntax` {

    def `should do nothing if the object has an appropriately named method, which returns true` {
      emptyMock should be ('empty)
      isEmptyMock should be ('empty)
    }

    def `should throw TestFailedException if no <symbol> or is<Symbol> method exists` {
      val ex1 = intercept[TestFailedException] {
        noPredicateMock should be ('empty)
      }
      ex1.getMessage should equal ("NoPredicateMock has neither an empty nor an isEmpty method")
      // Check message for name that starts with a consonant (should use a instead of an)
      val ex2 = intercept[TestFailedException] {
        noPredicateMock should be ('full)
      }
      ex2.getMessage should equal ("NoPredicateMock has neither a full nor an isFull method")
    }

    def `should do nothing if the object has an appropriately named method, which returns true, even if the method contains operator characters` {

      val opNames = new OperatorNames

      opNames should be ('op_21_!)
      opNames should be ('op_23_#)
      opNames should be ('op_25_%)
      opNames should be ('op_26_&)
      opNames should be ('op_2a_*)
      opNames should be ('op_2b_+)
      opNames should be ('op_2d_-)
      opNames should be ('op_2f_/)
      opNames should be ('op_3a_:)
      opNames should be ('op_3c_<)
      opNames should be ('op_3d_=)
      opNames should be ('op_3e_>)
      opNames should be ('op_3f_?)
      opNames should be ('op_40_@)
      opNames should be ('op_5c_\)
      opNames should be ('op_5e_^)
      opNames should be ('op_7c_|)
      opNames should be ('op_7e_~)

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

    def `should do nothing if the object has an appropriately named method, which returns false when used with not` {
      notEmptyMock should not { be ('empty) }
      notEmptyMock should not be ('empty)
      isNotEmptyMock should not { be ('empty) }
      isNotEmptyMock should not be ('empty)
    }

    def `should throw TestFailedException if no <symbol> or is<Symbol> method exists, when used with not` {
      val ex1 = intercept[TestFailedException] {
        noPredicateMock should not { be ('empty) }
      }
      ex1.getMessage should equal ("NoPredicateMock has neither an empty nor an isEmpty method")
      val ex2 = intercept[TestFailedException] {
        noPredicateMock should not (be ('full))
      }
      ex2.getMessage should equal ("NoPredicateMock has neither a full nor an isFull method")
      val ex3 = intercept[TestFailedException] {
        noPredicateMock should not be ('empty)
      }
      ex3.getMessage should equal ("NoPredicateMock has neither an empty nor an isEmpty method")
      val ex4 = intercept[TestFailedException] {
        noPredicateMock should not be ('full)
      }
      ex4.getMessage should equal ("NoPredicateMock has neither a full nor an isFull method")
    }

    def `should do nothing if the object has an appropriately named method, which returns true, when used in a logical-and expression` {
      emptyMock should ((be ('empty)) and (be ('empty)))
      emptyMock should (be ('empty) and (be ('empty)))
      emptyMock should (be ('empty) and be ('empty))
      isEmptyMock should ((be ('empty)) and (be ('empty)))
      isEmptyMock should (be ('empty) and (be ('empty)))
      isEmptyMock should (be ('empty) and be ('empty))
    }

    def `should do nothing if the object has an appropriately named method, which returns true, when used in a logical-or expression` {

      emptyMock should ((be ('full)) or (be ('empty)))
      emptyMock should (be ('full) or (be ('empty)))
      emptyMock should (be ('full) or be ('empty))
      isEmptyMock should ((be ('full)) or (be ('empty)))
      isEmptyMock should (be ('full) or (be ('empty)))
      isEmptyMock should (be ('full) or be ('empty))

      emptyMock should ((be ('empty)) or (be ('full)))
      emptyMock should (be ('empty) or (be ('full)))
      emptyMock should (be ('empty) or be ('full))
      isEmptyMock should ((be ('empty)) or (be ('full)))
      isEmptyMock should (be ('empty) or (be ('full)))
      isEmptyMock should (be ('empty) or be ('full))
    }

    def `should do nothing if the object has an appropriately named method, which returns false, when used in a logical-and expression with not` {

      notEmptyMock should (not (be ('empty)) and not (be ('empty)))
      notEmptyMock should ((not be ('empty)) and (not be ('empty)))
      notEmptyMock should (not be ('empty) and not be ('empty))

      isNotEmptyMock should (not (be ('empty)) and not (be ('empty)))
      isNotEmptyMock should ((not be ('empty)) and (not be ('empty)))
      isNotEmptyMock should (not be ('empty) and not be ('empty))
    }

    def `should do nothing if the object has an appropriately named method, which returns false, when used in a logical-or expression with not` {

      notEmptyMock should (not (be ('empty)) or not (be ('empty)))
      notEmptyMock should ((not be ('empty)) or (not be ('empty)))
      notEmptyMock should (not be ('empty) or not be ('empty))

      isNotEmptyMock should (not (be ('empty)) or not (be ('empty)))
      isNotEmptyMock should ((not be ('empty)) or (not be ('empty)))
      isNotEmptyMock should (not be ('empty) or not be ('empty))

      notEmptyMock should (not (be ('full)) or not (be ('empty)))
      notEmptyMock should ((not be ('full)) or (not be ('empty)))
      notEmptyMock should (not be ('full) or not be ('empty))

      isNotEmptyMock should (not (be ('full)) or not (be ('empty)))
      isNotEmptyMock should ((not be ('full)) or (not be ('empty)))
      isNotEmptyMock should (not be ('full) or not be ('empty))
    }

    def `should throw TestFailedException if the object has an appropriately named method, which returns false` {
      val caught1 = intercept[TestFailedException] {
        notEmptyMock should be ('empty)
      }
      assert(caught1.getMessage === "NotEmptyMock was not empty")
      val caught2 = intercept[TestFailedException] {
        isNotEmptyMock should be ('empty)
      }
      assert(caught2.getMessage === "IsNotEmptyMock was not empty")
    }

    def `should throw TestFailedException if the object has an appropriately named method, which returns true when used with not` {
      val caught1 = intercept[TestFailedException] {
        emptyMock should not { be ('empty) }
      }
      assert(caught1.getMessage === "EmptyMock was empty")
      val caught2 = intercept[TestFailedException] {
        emptyMock should not be ('empty)
      }
      assert(caught2.getMessage === "EmptyMock was empty")
      val caught3 = intercept[TestFailedException] {
        isEmptyMock should not { be ('empty) }
      }
      assert(caught3.getMessage === "IsEmptyMock was empty")
      val caught4 = intercept[TestFailedException] {
        isEmptyMock should not be ('empty)
      }
      assert(caught4.getMessage === "IsEmptyMock was empty")
    }

    def `should throw TestFailedException if the object has an appropriately named method, which returns false, when used in a logical-and expression` {
      val caught1 = intercept[TestFailedException] {
        emptyMock should ((be ('empty)) and (be ('full)))
      }
      assert(caught1.getMessage === "EmptyMock was empty, but EmptyMock was not full")
      val caught2 = intercept[TestFailedException] {
        emptyMock should (be ('empty) and (be ('full)))
      }
      assert(caught2.getMessage === "EmptyMock was empty, but EmptyMock was not full")
      val caught3 = intercept[TestFailedException] {
        emptyMock should (be ('empty) and be ('full))
      }
      assert(caught3.getMessage === "EmptyMock was empty, but EmptyMock was not full")
      val caught4 = intercept[TestFailedException] {
        isEmptyMock should ((be ('empty)) and (be ('full)))
      }
      assert(caught4.getMessage === "IsEmptyMock was empty, but IsEmptyMock was not full")
      val caught5 = intercept[TestFailedException] {
        isEmptyMock should (be ('empty) and (be ('full)))
      }
      assert(caught5.getMessage === "IsEmptyMock was empty, but IsEmptyMock was not full")
      val caught6 = intercept[TestFailedException] {
        isEmptyMock should (be ('empty) and be ('full))
      }
      assert(caught6.getMessage === "IsEmptyMock was empty, but IsEmptyMock was not full")
    }

    def `should throw TestFailedException if the object has an appropriately named method, which returns false, when used in a logical-or expression` {

      val caught1 = intercept[TestFailedException] {
        notEmptyMock should ((be ('empty)) or (be ('empty)))
      }
      assert(caught1.getMessage === "NotEmptyMock was not empty, and NotEmptyMock was not empty")
      val caught2 = intercept[TestFailedException] {
        notEmptyMock should (be ('empty) or (be ('empty)))
      }
      assert(caught2.getMessage === "NotEmptyMock was not empty, and NotEmptyMock was not empty")
      val caught3 = intercept[TestFailedException] {
        notEmptyMock should (be ('empty) or be ('empty))
      }
      assert(caught3.getMessage === "NotEmptyMock was not empty, and NotEmptyMock was not empty")
      val caught4 = intercept[TestFailedException] {
        isNotEmptyMock should ((be ('empty)) or (be ('empty)))
      }
      assert(caught4.getMessage === "IsNotEmptyMock was not empty, and IsNotEmptyMock was not empty")
      val caught5 = intercept[TestFailedException] {
        isNotEmptyMock should (be ('empty) or (be ('empty)))
      }
      assert(caught5.getMessage === "IsNotEmptyMock was not empty, and IsNotEmptyMock was not empty")
      val caught6 = intercept[TestFailedException] {
        isNotEmptyMock should (be ('empty) or be ('empty))
      }
      assert(caught6.getMessage === "IsNotEmptyMock was not empty, and IsNotEmptyMock was not empty")
    }

    def `should throw TestFailedException if the object has an appropriately named method, which returns true, when used in a logical-and expression with not` {

      val caught1 = intercept[TestFailedException] {
        emptyMock should (not (be ('full)) and not (be ('empty)))
      }
      assert(caught1.getMessage === "EmptyMock was not full, but EmptyMock was empty")
      val caught2 = intercept[TestFailedException] {
        emptyMock should ((not be ('full)) and (not be ('empty)))
      }
      assert(caught2.getMessage === "EmptyMock was not full, but EmptyMock was empty")
      val caught3 = intercept[TestFailedException] {
        emptyMock should (not be ('full) and not be ('empty))
      }
      assert(caught3.getMessage === "EmptyMock was not full, but EmptyMock was empty")
      val caught4 = intercept[TestFailedException] {
        isEmptyMock should (not (be ('full)) and not (be ('empty)))
      }
      assert(caught4.getMessage === "IsEmptyMock was not full, but IsEmptyMock was empty")
      val caught5 = intercept[TestFailedException] {
        isEmptyMock should ((not be ('full)) and (not be ('empty)))
      }
      assert(caught5.getMessage === "IsEmptyMock was not full, but IsEmptyMock was empty")
      val caught6 = intercept[TestFailedException] {
        isEmptyMock should (not be ('full) and not be ('empty))
      }
      assert(caught6.getMessage === "IsEmptyMock was not full, but IsEmptyMock was empty")
      // Check that the error message "short circuits"
      val caught7 = intercept[TestFailedException] {
        emptyMock should (not (be ('empty)) and not (be ('full)))
      }
      assert(caught7.getMessage === "EmptyMock was empty")
    }

    def `should throw TestFailedException if the object has an appropriately named method, which returns true, when used in a logical-or expression with not` {

      val caught1 = intercept[TestFailedException] {
        emptyMock should (not (be ('empty)) or not (be ('empty)))
      }
      assert(caught1.getMessage === "EmptyMock was empty, and EmptyMock was empty")
      val caught2 = intercept[TestFailedException] {
        emptyMock should ((not be ('empty)) or (not be ('empty)))
      }
      assert(caught2.getMessage === "EmptyMock was empty, and EmptyMock was empty")
      val caught3 = intercept[TestFailedException] {
        emptyMock should (not be ('empty) or not be ('empty))
      }
      assert(caught3.getMessage === "EmptyMock was empty, and EmptyMock was empty")
      val caught4 = intercept[TestFailedException] {
        isEmptyMock should (not (be ('empty)) or not (be ('empty)))
      }
      assert(caught4.getMessage === "IsEmptyMock was empty, and IsEmptyMock was empty")
      val caught5 = intercept[TestFailedException] {
        isEmptyMock should ((not be ('empty)) or (not be ('empty)))
      }
      assert(caught5.getMessage === "IsEmptyMock was empty, and IsEmptyMock was empty")
      val caught6 = intercept[TestFailedException] {
        isEmptyMock should (not be ('empty) or not be ('empty))
      }
      assert(caught6.getMessage === "IsEmptyMock was empty, and IsEmptyMock was empty")
    }

    object `(for the different types that have implicit conversions for should methods)` {

      // FOR: implicit def convertToCollectionShouldWrapper[T](o: Collection[T])...
      def `should work on a scala.Collection` {
        val emptySet = Set[Int]()
        emptySet should be ('empty)
        val nonEmptySet = Set(1, 2, 3)
        nonEmptySet should not { be ('empty) }
        val caught1 = intercept[TestFailedException] {
          nonEmptySet should be ('empty)
        }
        assert(caught1.getMessage === "Set(1, 2, 3) was not empty")
        val caught2 = intercept[TestFailedException] {
          nonEmptySet should not { be ('hasDefiniteSize) }
        }
        assert(caught2.getMessage === "Set(1, 2, 3) was hasDefiniteSize")
        val caught3 = intercept[TestFailedException] {
          nonEmptySet should not { be ('happy) }
        }
        assert(caught3.getMessage === "Set(1, 2, 3) has neither a happy nor an isHappy method")
      }

      // FOR: implicit def convertToSeqShouldWrapper[T](o: Seq[T])...
      def `should work on a scala.Seq` {
        import scala.collection.mutable.ListBuffer
        val emptyListBuffer = new ListBuffer[Int]
        emptyListBuffer should be ('empty)
        val nonEmptyListBuffer = new ListBuffer[Int]
        nonEmptyListBuffer += 1
        nonEmptyListBuffer += 2
        nonEmptyListBuffer += 3
        nonEmptyListBuffer should not { be ('empty) }
        val caught1 = intercept[TestFailedException] {
          nonEmptyListBuffer should be ('empty)
        }
        assert(caught1.getMessage === "ListBuffer(1, 2, 3) was not empty")
        val caught2 = intercept[TestFailedException] {
          nonEmptyListBuffer should not { be ('hasDefiniteSize) }
        }
        assert(caught2.getMessage === "ListBuffer(1, 2, 3) was hasDefiniteSize")
        val caught3 = intercept[TestFailedException] {
          nonEmptyListBuffer should not { be ('happy) }
        }
        assert(caught3.getMessage === "ListBuffer(1, 2, 3) has neither a happy nor an isHappy method")
      }

      // implicit def convertToArrayShouldWrapper[T](o: Array[T]): ArrayShouldWrapper[T] = new ArrayShouldWrapper[T](o)
/* This no longer works as of Scala 2.8
      def `should work on a scala.Array` {
        val emptyArray = new Array[Int](0)
        emptyArray should be ('empty)
        val nonEmptyArray = Array(1, 2, 3)
        nonEmptyArray should not be ('empty)
        val caught1 = intercept[TestFailedException] {
          nonEmptyArray should be ('empty)
        }
        assert(caught1.getMessage === "Array(1, 2, 3) was not empty")
        val caught2 = intercept[TestFailedException] {
          nonEmptyArray should not { be ('hasDefiniteSize) }
        }
        assert(caught2.getMessage === "Array(1, 2, 3) was hasDefiniteSize")
        val caught3 = intercept[TestFailedException] {
          nonEmptyArray should not { be ('happy) }
        }
        assert(caught3.getMessage === "Array(1, 2, 3) has neither a happy nor an isHappy method")
      }
*/

      // FOR: implicit def convertToListShouldWrapper[T](o: List[T])...
      def `should work on a scala.List` {
        val emptyList = List[Int]()
        emptyList should be ('empty)
        val nonEmptyList = List(1, 2, 3)
        nonEmptyList should not { be ('empty) }
        val caught1 = intercept[TestFailedException] {
          nonEmptyList should be ('empty)
        }
        assert(caught1.getMessage === "List(1, 2, 3) was not empty")
        val caught2 = intercept[TestFailedException] {
          nonEmptyList should not { be ('hasDefiniteSize) }
        }
        assert(caught2.getMessage === "List(1, 2, 3) was hasDefiniteSize")
        val caught3 = intercept[TestFailedException] {
          nonEmptyList should not { be ('happy) }
        }
        assert(caught3.getMessage === "List(1, 2, 3) has neither a happy nor an isHappy method")
      }

      // implicit def convertToMapShouldWrapper[K, V](o: scala.collection.Map[K, V])...
      def `should work on a scala.Map` {
        val emptyMap = Map[Int, String]()
        emptyMap should be ('empty)
        val nonEmptyMap = Map("one" -> 1, "two" -> 2, "three" -> 3)
        nonEmptyMap should not { be ('empty) }
        val caught1 = intercept[TestFailedException] {
          nonEmptyMap should be ('empty)
        }
        assert(caught1.getMessage === "Map(one -> 1, two -> 2, three -> 3) was not empty")
        val caught2 = intercept[TestFailedException] {
          nonEmptyMap should not { be ('hasDefiniteSize) }
        }
        assert(caught2.getMessage === "Map(one -> 1, two -> 2, three -> 3) was hasDefiniteSize")
        val caught3 = intercept[TestFailedException] {
          nonEmptyMap should not { be ('happy) }
        }
        assert(caught3.getMessage === "Map(one -> 1, two -> 2, three -> 3) has neither a happy nor an isHappy method")
      }

      // implicit def convertToStringShouldWrapper[K, V](o: String): StringShouldWrapper = new StringShouldWrapper(o)
      def `should work on a String` {
        val caught3 = intercept[TestFailedException] {
          "howdy" should not be ('happy)
        }
        assert(caught3.getMessage === "\"howdy\" has neither a happy nor an isHappy method")
      }

      // FOR: implicit def convertToJavaCollectionShouldWrapper[T](o: java.util.Collection[T])...
      def `should work on a java.util.Collection` {
        val emptySet = new java.util.HashSet[Int]
        emptySet should be ('empty)
        val nonEmptySet = new java.util.HashSet[Int]
        nonEmptySet.add(1)
        nonEmptySet.add(2)
        nonEmptySet.add(3)
        nonEmptySet should not { be ('empty) }
        val caught1 = intercept[TestFailedException] {
          nonEmptySet should be ('empty)
        }
        assert(caught1.getMessage endsWith "] was not empty")
        val caught3 = intercept[TestFailedException] {
          nonEmptySet should not { be ('happy) }
        }
        assert(caught3.getMessage endsWith "] has neither a happy nor an isHappy method")
      }

      // FOR: implicit def convertToJavaListShouldWrapper[T](o: java.util.List[T])...
      def `should work on a java.util.List` {
        val emptyList = new java.util.ArrayList[Int]
        emptyList should be ('empty)
        val nonEmptyList = new java.util.ArrayList[Int]
        nonEmptyList.add(1)
        nonEmptyList.add(2)
        nonEmptyList.add(3)
        nonEmptyList should not { be ('empty) }
        val caught1 = intercept[TestFailedException] {
          nonEmptyList should be ('empty)
        }
        assert(caught1.getMessage === "[1, 2, 3] was not empty")
        val caught3 = intercept[TestFailedException] {
          nonEmptyList should not { be ('happy) }
        }
        assert(caught3.getMessage === "[1, 2, 3] has neither a happy nor an isHappy method")
      }
    }
  }

  object `The be matcher` {

    object `(for symbols)` {

      // TODO: Make sure to write a test for each conversion, because some are using ShouldMethodsForAny instead
      // of ShouldMethodsForAnyRef.
      def `should be invokable from be a Symbol and be an Symbol` {
        val emptySet = Set()
        emptySet should be a ('empty)
        emptySet should be an ('empty)
        val nonEmptySet = Set(1, 2, 3)
        nonEmptySet should not { be a ('empty) }
        nonEmptySet should not { be an ('empty) }
      }

      def `should call empty when passed 'empty` {
        class EmptyMock {
          def empty: Boolean = true
        }
        class NonEmptyMock {
          def empty: Boolean = false
        }
        (new EmptyMock) should be ('empty)
        (new NonEmptyMock) should not { be ('empty) }
        // (new NonEmptyMock) shouldNot be ('empty)
      }

// STOLE FROM HERE
      def `should call the Scala=style method if both an empty and an isEmpty method exist` {
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
        (new EmptyMock) should be ('empty)
        (new NonEmptyMock) should not { be ('empty) }
      }

      def `should access an 'empty' val when passed 'empty` {
        class EmptyMock {
          val empty: Boolean = true
        }
        class NonEmptyMock {
          val empty: Boolean = false
        }
        (new EmptyMock) should be ('empty)
        (new NonEmptyMock) should not { be ('empty) }
      }
    }
  }

  object `the be ('empty) syntax` {

    def `should call isEmpty` {
      val emptySet = Set[Int]()
      emptySet should be ('empty)
      val nonEmptySet = Set(1, 2, 3)
      nonEmptySet should not { be ('empty) }
    }

    def `should call empty when passed 'empty` {
      class EmptyMock {
        def empty: Boolean = true
      }
      class NonEmptyMock {
        def empty: Boolean = false
      }
      (new EmptyMock) should be ('empty)
      (new NonEmptyMock) should not { be ('empty) }
      // (new NonEmptyMock) shouldNot be ('empty)
    }

    def `should throw TestFailedException if no empty or isEmpty method` {
      class EmptyMock {
        override def toString = "EmptyMock"
      }
      class NonEmptyMock {
        override def toString = "NonEmptyMock"
      }
      val ex1 = intercept[TestFailedException] {
        (new EmptyMock) should be ('empty)
      }
      ex1.getMessage should equal ("EmptyMock has neither an empty nor an isEmpty method")
      val ex2 = intercept[TestFailedException] {
        (new NonEmptyMock) should not { be ('empty) }
      }
      ex2.getMessage should equal ("NonEmptyMock has neither an empty nor an isEmpty method")
    }

    def `should call the Scala-style method if both an empty and an isEmpty method exist` {
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
      (new EmptyMock) should be ('empty)
      (new NonEmptyMock) should not { be ('empty) }
    }

    def `should access an 'empty' val when passed 'empty` {
      class EmptyMock {
        val empty: Boolean = true
      }
      class NonEmptyMock {
        val empty: Boolean = false
      }
      (new EmptyMock) should be ('empty)
      (new NonEmptyMock) should not { be ('empty) }
      // (new NonEmptyMock) shouldNot be ('empty)
    }
  }

  object `The be 'defined syntax` {

    def `should do nothing when used with a Some` {
      val someString: Some[String] = Some("hi")
      someString should be ('defined)
      val optionString: Option[String] = Some("hi")
      optionString should be ('defined)
    }

    def `should throw TestFailedException when used with a None` {
      val none: None.type = None
      val caught1 = intercept[TestFailedException] {
        none should be ('defined)
      }
      assert(caught1.getMessage === "None was not defined")
      val option: Option[Int] = None
      val caught2 = intercept[TestFailedException] {
        option should be ('defined)
      }
      assert(caught2.getMessage === "None was not defined")
    }

    def `should call defined` {
      class DefinedMock {
        def defined: Boolean = true
      }
      class NonDefinedMock {
        def defined: Boolean = false
      }
      (new DefinedMock) should be ('defined)
      (new NonDefinedMock) should not { be ('defined) }
      // (new NonDefinedMock) shouldNot be ('defined)
    }

    def `should throw TestFailedException if no defined or isDefined method` {
      class DefinedMock {
        override def toString = "DefinedMock"
      }
      class NonDefinedMock {
        override def toString = "NonDefinedMock"
      }
      val ex1 = intercept[TestFailedException] {
        (new DefinedMock) should be ('defined)
      }
      ex1.getMessage should equal ("DefinedMock has neither a defined nor an isDefined method")
      val ex2 = intercept[TestFailedException] {
        (new NonDefinedMock) should not { be ('defined) }
      }
      ex2.getMessage should equal ("NonDefinedMock has neither a defined nor an isDefined method")
    }

    def `should call the Scala-style method if both a defined and an isDefined method exist` {
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
      (new DefinedMock) should be ('defined)
      (new NonDefinedMock) should not { be ('defined) }
    }

    def `should access an 'defined' val` {
      class DefinedMock {
        val defined: Boolean = true
      }
      class NonDefinedMock {
        val defined: Boolean = false
      }
      (new DefinedMock) should be ('defined)
      (new NonDefinedMock) should not { be ('defined) }
      // (new NonDefinedMock) shouldNot be ('defined)
    }
  }
}
