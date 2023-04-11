/*
 * Copyright 2001-2016 Artima, Inc.
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
package org.scalactic

import org.scalatest._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.SharedHelpers.thisLineNumber

class DifferSpec extends funspec.AnyFunSpec {

  import matchers.should.Matchers._

  describe("Differ") {

    describe("when used with default Equality") {

      it("should include analysis in TestFailedException thrown from a shouldEqual (b) syntax when both a and b is String") {
        val e = intercept[TestFailedException] {
          "test2" shouldEqual ("test")
        }
        assert(e.message == Some("\"test[2]\" did not equal \"test[]\""))
        assert(e.analysis == Vector("\"test[2]\" -> \"test[]\""))
      }

      it("should not include analysis in TestFailedException thrown from a shouldEqual (b) syntax when a is String and b is Int") {
        val e = intercept[TestFailedException] {
          "test2" shouldEqual (3)
        }
        assert(e.message == Some("\"test2\" did not equal 3"))
        assert(e.analysis == Vector.empty)
      }

      it("should not include analysis in TestFailedException thrown from a shouldEqual (b) syntax when a is Int and b is String") {
        val e = intercept[TestFailedException] {
          3 shouldEqual ("test2")
        }
        assert(e.message == Some("3 did not equal \"test2\""))
        assert(e.analysis == Vector.empty)
      }

      it("should not include analysis in TestFailedException thrown from a shouldEqual (b) syntax when a is Int and b is Int") {
        val e = intercept[TestFailedException] {
          3 shouldEqual (2)
        }
        assert(e.message == Some("3 did not equal 2"))
        assert(e.analysis == Vector.empty)
      }

      it("should include analysis in TestFailedException thrown from a should equal (b) syntax when both a and b is String") {
        val e = intercept[TestFailedException] {
          "test2" should equal("test")
        }
        assert(e.message == Some("\"test[2]\" did not equal \"test[]\""))
        assert(e.analysis == Vector("\"test[2]\" -> \"test[]\""))
      }

      it("should not include analysis in TestFailedException thrown from a should equal (b) syntax when a is String and b is Int") {
        val e = intercept[TestFailedException] {
          "test2" should equal (2)
        }
        assert(e.message == Some("\"test2\" did not equal 2"))
        assert(e.analysis == Vector.empty)
      }

      it("should not include analysis in TestFailedException thrown from a should equal (b) syntax when a is Int and b is String") {
        val e = intercept[TestFailedException] {
          2 should equal ("test2")
        }
        assert(e.message == Some("2 did not equal \"test2\""))
        assert(e.analysis == Vector.empty)
      }

      it("should not include analysis in TestFailedException thrown from a should equal (b) syntax when a is Int and b is Int") {
        val e = intercept[TestFailedException] {
          2 should equal (3)
        }
        assert(e.message == Some("2 did not equal 3"))
        assert(e.analysis == Vector.empty)
      }

      it("should include analysis in TestFailedException thrown from all(a) shouldEqual (b) syntax when both a and b is String") {
        val e = intercept[TestFailedException] {
          all(List("test", "test2", "test")) shouldEqual ("test")
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 1, \"test[2]\" did not equal \"test[]\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(\"test\", \"test2\", \"test\")"))
        assert(e.analysis == Vector("\"test[2]\" -> \"test[]\""))
      }

      it("should not include analysis in TestFailedException thrown from all(a) shouldEqual (b) syntax when both a is String and b is Int") {
        val e = intercept[TestFailedException] {
          all(List("test", "test2", "test")) shouldEqual (2)
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, \"test\" did not equal 2 (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(\"test\", \"test2\", \"test\")"))
        assert(e.analysis == Vector.empty)
      }

      it("should not include analysis in TestFailedException thrown from all(a) shouldEqual (b) syntax when both a is Int and b is String") {
        val e = intercept[TestFailedException] {
          all(List(1, 2, 3)) shouldEqual ("test")
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, 1 did not equal \"test\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(1, 2, 3)"))
        assert(e.analysis == Vector.empty)
      }

      it("should not include analysis in TestFailedException thrown from all(a) shouldEqual (b) syntax when both a is Int and b is Int") {
        val e = intercept[TestFailedException] {
          all(List(1, 2, 3)) shouldEqual (2)
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, 1 did not equal 2 (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(1, 2, 3)"))
        assert(e.analysis == Vector.empty)
      }

      it("should include analysis in TestFailedException thrown from all(a) should equal (b) syntax when both a and b is String") {
        val e = intercept[TestFailedException] {
          all(List("test", "test2", "test")) should equal("test")
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 1, \"test[2]\" did not equal \"test[]\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(\"test\", \"test2\", \"test\")"))
        assert(e.analysis == Vector("\"test[2]\" -> \"test[]\""))
      }

      it("should not include analysis in TestFailedException thrown from all(a) should equal (b) syntax when a is String and b is Int") {
        val e = intercept[TestFailedException] {
          all(List("test", "test2", "test")) should equal(2)
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, \"test\" did not equal 2 (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(\"test\", \"test2\", \"test\")"))
        assert(e.analysis == Vector.empty)
      }

      it("should not include analysis in TestFailedException thrown from all(a) should equal (b) syntax when a is Int and b is String") {
        val e = intercept[TestFailedException] {
          all(List(1, 2, 3)) should equal("test")
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, 1 did not equal \"test\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(1, 2, 3)"))
        assert(e.analysis == Vector.empty)
      }

      it("should not include analysis in TestFailedException thrown from all(a) should equal (b) syntax when a is Int and b is Int") {
        val e = intercept[TestFailedException] {
          all(List(1, 2, 3)) should equal(2)
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 0, 1 did not equal 2 (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(1, 2, 3)"))
        assert(e.analysis == Vector.empty)
      }

      it("should include analysis in TestFailedException thrown from assert(a === b) syntax when both a and b is String") {
        val e = intercept[TestFailedException] {
          assert("test2" === "test")
        }
        assert(e.message == Some("\"test[2]\" did not equal \"test[]\""))
        assert(e.analysis == Vector(("\"test[2]\" -> \"test[]\"")))
      }

      it("should include analysis in TestFailedException thrown from assert(a === b) syntax when a is String and b is Int") {
        val e = intercept[TestFailedException] {
          assert("test2" === 3)
        }
        assert(e.message == Some("\"test2\" did not equal 3"))
        assert(e.analysis == Vector.empty)
      }

      it("should include analysis in TestFailedException thrown from assert(a === b) syntax when a is Int and b is String") {
        val e = intercept[TestFailedException] {
          assert(3 === "test2")
        }
        assert(e.message == Some("3 did not equal \"test2\""))
        assert(e.analysis == Vector.empty)
      }

      it("should include analysis in TestFailedException thrown from assert(a === b) syntax when a is Int and b is Int") {
        val e = intercept[TestFailedException] {
          assert(3 === 2)
        }
        assert(e.message == Some("3 did not equal 2"))
        assert(e.analysis == Vector.empty)
      }

    }

    describe("when used with custom Equality that has custom prettifier implementation") {

      implicit val prettifier =
        new Prettifier {
          override def apply(o: Any): String = Prettifier.default(o)

          override def apply(left: Any, right: Any): PrettyPair = {
            val prettyPair = AnyDiffer.difference(left, right, this)
            prettyPair.copy(
              left = ("\"**" + prettyPair.left.substring(1, prettyPair.left.length - 1) + "\""),
              right = ("\"" + prettyPair.right.substring(1, prettyPair.right.length - 1) + "**\"")
            )
          }
        }

      it("can be used with a shouldEqual (b) syntax") {
        val e = intercept[TestFailedException] {
          "test2" shouldEqual ("test")
        }
        assert(e.message == Some("\"**test[2]\" did not equal \"test[]**\""))
        assert(e.analysis == Vector(("\"test[2]\" -> \"test[]\"")))
      }

      it("can be used with a should equal (b) syntax") {
        val e = intercept[TestFailedException] {
          "test2" should equal("test")
        }
        assert(e.message == Some("\"**test[2]\" did not equal \"test[]**\""))
        assert(e.analysis == Vector(("\"test[2]\" -> \"test[]\"")))
      }

      it("can be used with all(a) shouldEqual (b) syntax") {
        val e = intercept[TestFailedException] {
          all(List("test", "test2", "test")) shouldEqual ("test")
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 1, \"**test[2]\" did not equal \"test[]**\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(\"test\", \"test2\", \"test\")"))
        assert(e.analysis == Vector(("\"test[2]\" -> \"test[]\"")))
      }

      it("can be used with all(a) should equal (b) syntax") {
        val e = intercept[TestFailedException] {
          all(List("test", "test2", "test")) should equal("test")
        }
        assert(e.message == Some("'all' inspection failed, because: \n" +
          "  at index 1, \"**test[2]\" did not equal \"test[]**\" (DifferSpec.scala:" + (thisLineNumber - 3) + ") \n" +
          "in List(\"test\", \"test2\", \"test\")"))
        assert(e.analysis == Vector(("\"test[2]\" -> \"test[]\"")))
      }
    }

  }

  val EOL = scala.compat.Platform.EOL

  sealed trait Parent
  case class Bar( s: String, private val i: Int ) extends Parent
  case class Foo( bar: Bar, b: List[Int], parent: Option[Parent] ) extends Parent

  // SKIP-SCALATESTNATIVE-START
  describe("ObjectDiffer") {

    it("should produce difference of 2 Bars correctly") {
      val a = Bar("asdf", 5)
      val b = Bar("asdf", 6)
      val c = Bar("asf", 6)

      assert(ObjectDiffer.difference(a, b, Prettifier.default).analysis == Some("DifferSpec$Bar(i: 5 -> 6)"))
      assert(ObjectDiffer.difference(b, c, Prettifier.default).analysis == Some("DifferSpec$Bar(s: \"as[d]f\" -> \"as[]f\")"))
      assert(ObjectDiffer.difference(a, c, Prettifier.default).analysis == Some("DifferSpec$Bar(i: 5 -> 6, s: \"as[d]f\" -> \"as[]f\")"))
    }

    it("should produce difference of 2 Foos correctly") {
      val a: Foo = Foo(
        Bar( "asdf", 5 ),
        List( 123, 1234 ),
        Some( Bar( "asdf", 5 ) )
      )
      val b: Foo = Foo(
        Bar( "asdf", 66 ),
        List( 1234 ),
        Some( Bar( "qwer", 5 ) )
      )

      assert(
        ObjectDiffer.difference(a, b, Prettifier.default).analysis ==
          (
            if (ScalacticVersions.BuiltForScalaVersion == "2.12" || ScalacticVersions.BuiltForScalaVersion == "2.13" || ScalacticVersions.BuiltForScalaVersion.startsWith("3."))
              Some("DifferSpec$Foo(b: List(0: 123 -> 1234, 1: 1234 -> ), bar: DifferSpec$Bar(i: 5 -> 66), parent: Some(value: DifferSpec$Bar(s: \"[asdf]\" -> \"[qwer]\")))")
            else
              Some("DifferSpec$Foo(b: List(0: 123 -> 1234, 1: 1234 -> ), bar: DifferSpec$Bar(i: 5 -> 66), parent: Some(x: DifferSpec$Bar(s: \"[asdf]\" -> \"[qwer]\")))")
            )
      )
    }

    it("should be used when case class is being compared with shouldEqual matcher") {
      val a = Bar("asdf", 5)
      val b = Bar("asbdf", 6)

      val e = intercept[TestFailedException] {
        a shouldEqual b
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "DifferSpec$Bar(i: 5 -> 6, s: \"as[]df\" -> \"as[b]df\")")
    }

    it("should be used when case class is being compared with should equal matcher") {
      val a = Bar("asdf", 5)
      val b = Bar("asbdf", 6)

      val e = intercept[TestFailedException] {
        a should equal (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "DifferSpec$Bar(i: 5 -> 6, s: \"as[]df\" -> \"as[b]df\")")
    }

    it("should be used when case class is being compared with 'all' shouldEqual matcher") {
      val a = Bar("asdf", 5)
      val b = Bar("asbdf", 6)

      val e = intercept[TestFailedException] {
        all(List(a)) shouldEqual (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "DifferSpec$Bar(i: 5 -> 6, s: \"as[]df\" -> \"as[b]df\")")
    }
    it("should be used when case class is being compared with 'all' should equal matcher") {
      val a = Bar("asdf", 5)
      val b = Bar("asbdf", 6)

      val e = intercept[TestFailedException] {
        all(List(a)) should equal (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "DifferSpec$Bar(i: 5 -> 6, s: \"as[]df\" -> \"as[b]df\")")
    }

    it("should not produce difference when left and right have the same elements in same order") {
      assert(ObjectDiffer.difference((1, 2, 3), (1, 2, 3), Prettifier.default).analysis == None)
    }

    it("should produce difference when element in left and right is different") {
      assert(ObjectDiffer.difference((1, 2, 3), (1, 6, 3), Prettifier.default).analysis == Some("Tuple3(_2: 2 -> 6)"))
    }

    it("should product difference when element exist in left, but not in right") {
      assert(ObjectDiffer.difference((1, 2, 3), (1, 2), Prettifier.default).analysis == Some("Tuple3(_3: 3 -> )"))
    }

    it("should product difference when element exist in right, but not in left") {
      assert(ObjectDiffer.difference((1, 2), (1, 2, 3), Prettifier.default).analysis == Some("Tuple2(_3: -> 3)"))
    }

    it("should produce difference when elements in left and right is same but in different order") {
      assert(ObjectDiffer.difference((1, 2, 3), (3, 2, 1), Prettifier.default).analysis == Some("Tuple3(_1: 1 -> 3, _3: 3 -> 1)"))
    }

    it("should be used when Tuple is being compared with shouldEqual matcher") {
      val e = intercept[TestFailedException] {
        (1, 2, 3) shouldEqual (1, 6, 3)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "Tuple3(_2: 2 -> 6)")
    }

    it("should be used when Tuple is being compared with should equal matcher") {
      val e = intercept[TestFailedException] {
        (1, 2, 3) should equal ((1, 6, 3))
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "Tuple3(_2: 2 -> 6)")
    }

    it("should be used when Tuple is being compared with 'all' shouldEqual matcher") {
      val e = intercept[TestFailedException] {
        all(List((1, 2, 3))) shouldEqual ((1, 6, 3))
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "Tuple3(_2: 2 -> 6)")
    }

    it("should be used when Tuple is being compared with 'all' should equal matcher") {
      val e = intercept[TestFailedException] {
        all(List((1, 2, 3))) should equal ((1, 6, 3))
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "Tuple3(_2: 2 -> 6)")
    }

    it("should handle cyclic object diff correctly without causing stack overflow exception") {
      case class Cyclic(str: String, var c: Cyclic) {
        override def toString = str
      }

      val a = Cyclic("a", null)
      val b = Cyclic("b", a)
      a.c = b 
      
      val e = intercept[TestFailedException] {
        assert(a == b)
      }
      assert(e.getMessage == "a did not equal b")
      assert(e.analysis.length == 1)
      assert(e.analysis(0).contains("Cyclic value detected, name: a -> b, str: \"[b]\" -> \"[a]\"), str: \"[a]\" -> \"[b]\""))
    }

  }
  // SKIP-SCALATESTNATIVE-END

  describe("GenSeqDiffer") {

    it("should not produce difference when left and right have the same elements in same order") {
      assert(GenSeqDiffer.difference(List(1, 2, 3), List(1, 2, 3), Prettifier.default).analysis == None)
    }

    it("should produce difference when element in left and right is different") {
      assert(GenSeqDiffer.difference(List(1, 2, 3), List(1, 6, 3), Prettifier.default).analysis == Some("List(1: 2 -> 6)"))
    }

    it("should product difference when element exist in left, but not in right") {
      assert(GenSeqDiffer.difference(List(1, 2, 3), List(1, 2), Prettifier.default).analysis == Some("List(2: 3 -> )"))
    }

    it("should product difference when element exist in right, but not in left") {
      assert(GenSeqDiffer.difference(List(1, 2), List(1, 2, 3), Prettifier.default).analysis == Some("List(2: -> 3)"))
    }

    it("should produce difference when elements in left and right is same but in different order") {
      assert(GenSeqDiffer.difference(List(1, 2, 3), List(3, 2, 1), Prettifier.default).analysis == Some("List(0: 1 -> 3, 2: 3 -> 1)"))
    }

    it("should be used when GenSeq is being compared with shouldEqual matcher") {
      val a = List(1, 2, 3)
      val b = List(1, 6, 3)

      val e = intercept[TestFailedException] {
        a shouldEqual b
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "List(1: 2 -> 6)")
    }

    it("should be used when GenSeq is being compared with should equal matcher") {
      val a = List(1, 2, 3)
      val b = List(1, 6, 3)

      val e = intercept[TestFailedException] {
        a should equal (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "List(1: 2 -> 6)")
    }

    it("should be used when GenSeq is being compared with 'all' shouldEqual matcher") {
      val a = List(1, 2, 3)
      val b = List(1, 6, 3)

      val e = intercept[TestFailedException] {
        all(List(a)) shouldEqual (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "List(1: 2 -> 6)")
    }

    it("should be used when GenSeq is being compared with 'all' should equal matcher") {
      val a = List(1, 2, 3)
      val b = List(1, 6, 3)

      val e = intercept[TestFailedException] {
        all(List(a)) should equal (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "List(1: 2 -> 6)")
    }

    it("should use passed in prettifier to prettify element values") {
      val a = List("one", "two", "three")
      val b = List("one", "six", "three")

      val e = intercept[TestFailedException] {
        a should equal (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "List(1: \"two\" -> \"six\")")
    }

    it("should find diff elements according to limit size of prettifier when available") {
      val a = List(1, 2, 3, 4, 5)
      val b = List(1, 3, 2, 5, 6)
      implicit val prettifier = Prettifier.truncateAt(SizeLimit(3))
      val e = intercept[TestFailedException] {
        a should equal (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "List(1: 2 -> 3, 2: 3 -> 2, 3: 4 -> 5, ...)")
    }

  }

  describe("GenSetDiffer") {

    it("should not produce difference when left and right have the same elements in same order") {
      assert(GenSetDiffer.difference(Set(1, 2, 3), Set(1, 2, 3), Prettifier.default).analysis == None)
    }

    it("should produce difference when element in left and right is different") {
      assert(GenSetDiffer.difference(Set(1, 2, 3), Set(1, 6, 3), Prettifier.default).analysis == Some("Set(missingInLeft: [6], missingInRight: [2])"))
    }

    it("should product difference when element exist in left, but not in right") {
      assert(GenSetDiffer.difference(Set(1, 2, 3), Set(1, 2), Prettifier.default).analysis == Some("Set(missingInRight: [3])"))
    }

    it("should product difference when element exist in right, but not in left") {
      assert(GenSetDiffer.difference(Set(1, 2), Set(1, 2, 3), Prettifier.default).analysis == Some("Set(missingInLeft: [3])"))
    }

    it("should not produce difference when elements in left and right is same but in different order") {
      assert(GenSetDiffer.difference(Set(1, 2, 3), Set(3, 2, 1), Prettifier.default).analysis == None)
    }

    it("should be used when GenSet is being compared with shouldEqual matcher") {
      val a = Set(1, 2, 3)
      val b = Set(1, 6, 3)

      val e = intercept[TestFailedException] {
        a shouldEqual b
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "Set(missingInLeft: [6], missingInRight: [2])")
    }

    it("should be used when GenSet is being compared with should equal matcher") {
      val a = Set(1, 2, 3)
      val b = Set(1, 6, 3)

      val e = intercept[TestFailedException] {
        a should equal (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "Set(missingInLeft: [6], missingInRight: [2])")
    }

    it("should be used when GenSet is being compared with 'all' shouldEqual matcher") {
      val a = Set(1, 2, 3)
      val b = Set(1, 6, 3)

      val e = intercept[TestFailedException] {
        all(List(a)) shouldEqual (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "Set(missingInLeft: [6], missingInRight: [2])")
    }

    it("should be used when GenSet is being compared with 'all' should equal matcher") {
      val a = Set(1, 2, 3)
      val b = Set(1, 6, 3)

      val e = intercept[TestFailedException] {
        all(List(a)) should equal (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "Set(missingInLeft: [6], missingInRight: [2])")
    }

    it("should use passed in prettifier to prettify element values") {
      val a = Set("one", "two", "three")
      val b = Set("one", "six", "three")

      val e = intercept[TestFailedException] {
        a should equal (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "Set(missingInLeft: [\"six\"], missingInRight: [\"two\"])")
    }

    it("should find diff elements according to limit size of prettifier when available") {
      val a = Set(1, 2, 3, 4, 5)
      val b = Set(4, 5, 6, 7, 8)
      implicit val prettifier = Prettifier.truncateAt(SizeLimit(2))
      val e = intercept[TestFailedException] {
        a should equal (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) endsWith "(missingInLeft: [6, 7, ...], missingInRight: [1, 2, ...])")
    }

  }

  describe("GenMapDiffer") {

    it("should not produce difference when left and right have the same elements in same order") {
      assert(GenMapDiffer.difference(Map(1 -> "one", 2 -> "two", 3 -> "three"), List(1 -> "one", 2 -> "two", 3 -> "three"), Prettifier.default).analysis == None)
    }

    it("should produce difference when element in left and right is different") {
      assert(GenMapDiffer.difference(Map(1 -> "one", 2 -> "two", 3 -> "three"), Map(1 -> "one", 6 -> "six", 3 -> "three"), Prettifier.default).analysis == Some("Map(2: \"two\" -> , 6: -> \"six\")"))
    }

    it("should product difference when element exist in left, but not in right") {
      assert(GenMapDiffer.difference(Map(1 -> "one", 2 -> "two", 3 -> "three"), Map(1 -> "one", 2 -> "two"), Prettifier.default).analysis == Some("Map(3: \"three\" -> )"))
    }

    it("should product difference when element exist in right, but not in left") {
      assert(GenMapDiffer.difference(Map(1 -> "one", 2 -> "two"), Map(1 -> "one", 2 -> "two", 3 -> "three"), Prettifier.default).analysis == Some("Map(3: -> \"three\")"))
    }

    it("should not produce difference when elements in left and right is same but in different order") {
      assert(GenMapDiffer.difference(Map(1 -> "one", 2 -> "two", 3 -> "three"), Map(3 -> "three", 2 -> "two", 1 -> "one"), Prettifier.default).analysis == None)
    }

    it("should be used when GenMap is being compared with shouldEqual matcher") {
      val a = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val b = Map(1 -> "one", 6 -> "six", 3 -> "three")

      val e = intercept[TestFailedException] {
        a shouldEqual b
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "Map(2: \"two\" -> , 6: -> \"six\")")
    }

    it("should be used when GenMap is being compared with should equal matcher") {
      val a = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val b = Map(1 -> "one", 6 -> "six", 3 -> "three")

      val e = intercept[TestFailedException] {
        a should equal (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "Map(2: \"two\" -> , 6: -> \"six\")")
    }

    it("should be used when GenMap is being compared with 'all' shouldEqual matcher") {
      val a = Map(1 -> "one", 2-> "two", 3 -> "three")
      val b = Map(1-> "one", 6-> "six", 3 -> "three")

      val e = intercept[TestFailedException] {
        all(List(a)) shouldEqual (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "Map(2: \"two\" -> , 6: -> \"six\")")
    }

    it("should be used when GenMap is being compared with 'all' should equal matcher") {
      val a = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val b = Map(1 -> "one", 6 -> "six", 3 -> "three")

      val e = intercept[TestFailedException] {
        all(List(a)) should equal (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "Map(2: \"two\" -> , 6: -> \"six\")")
    }

    it("should use passed in prettifier to prettify keys and values") {
      val a = Map("1" -> "one", "2" -> "two", "3" -> "three")
      val b = Map("1" -> "one", "6" -> "six", "3" -> "three")

      val e = intercept[TestFailedException] {
        a should equal (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) == "Map(\"2\": \"two\" -> , \"6\": -> \"six\")")
    }

    it("should find diff elements according to limit size of prettifier when available") {
      val a = Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five")
      val b = Map(1 -> "one", 2 -> "t2wo", 3 -> "th3ree", 4 -> "fou4r", 5 -> "f5ive")
      implicit val prettifier = Prettifier.truncateAt(SizeLimit(3))
      val e = intercept[TestFailedException] {
        a should equal (b)
      }
      assert(e.analysis.length == 1)
      assert(e.analysis(0) endsWith "(5: \"five\" -> \"f5ive\", 2: \"two\" -> \"t2wo\", 3: \"three\" -> \"th3ree\", ...)")
    }

  }

}