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
package org.scalactic

import org.scalatest._
import SharedHelpers.{javaList, javaSortedMap}
import scala.collection.mutable.WrappedArray
import scala.util.Success

// SKIP-SCALATESTJS,NATIVE-START
import scala.xml.NodeSeq
// SKIP-SCALATESTJS,NATIVE-END

class PrettifierSpec extends funspec.AnyFunSpec with matchers.should.Matchers {

  describe("A Prettifier") {
    it("should convert an Any to a String") {
      val f =
        new Prettifier {
          def apply(o: Any) = o.toString
        }

      f("hi") should be ("hi")
      f(List("hi")) should be ("List(hi)")
    }
    it("can be composed with another Prettifier") {
      case class Yell(secret: String)
      val myLittlePretty =
        new Prettifier {
          def apply(o: Any) =
            o match {
              case Yell(secret) => secret.toUpperCase + "!!!"
              case _ => Prettifier.default(o)
            }
        }
      myLittlePretty(Yell("I like fruit loops")) should be ("I LIKE FRUIT LOOPS!!!")
      myLittlePretty("hi") should be ("\"hi\"")
      myLittlePretty("h\u0000i") should be ("\"h\\" + "u0000i\"")
      myLittlePretty("h\ti") should be (raw""""h\ti"""")
      myLittlePretty('h') should be ("'h'")
      myLittlePretty(Array(1, 2, 3)) should be ("Array(1, 2, 3)")
      myLittlePretty(WrappedArray.make(Array(1, 2, 3))) should be ("Array(1, 2, 3)")
      myLittlePretty(null) should be ("null")
      myLittlePretty(()) should be ("<(), the Unit value>")
      myLittlePretty(List("1", "2", "3")) should be ("List(\"1\", \"2\", \"3\")")
    }
    // SKIP-DOTTY-START
    // no runtime reflection in Dotty
    it("should by default offer an apply method that takes two args that returns a PrettyPair whose left and right are prettified by the one-arg apply and hint is None.") {

      case class Yell(secret: String)
      val myLittlePretty =
        new Prettifier {
          def apply(o: Any) =
            o match {
              case Yell(secret) => secret.toUpperCase + "!!!"
              case _ => Prettifier.default(o)
            }
        }
      val left = Yell("I like fruit loops")
      myLittlePretty(left, Yell("I like raisin bran")) shouldBe PrettyPair("I LIKE FRUIT LOOPS!!!", "I LIKE RAISIN BRAN!!!", Some(Differ.simpleClassName(left) + "(secret: \"I like [fruit loops]\" -> \"I like [raisin bran]\")"))
    }
    // SKIP-DOTTY-END
  }

  describe("the basic Prettifier") {
    it("should put double quotes around strings") {
      Prettifier.basic("hi") should be ("\"hi\"")
    }
    it("should put single quotes around chars") {
      Prettifier.basic('h') should be ("'h'")
    }
    it("should pretty print arrays") {
      Prettifier.basic(Array(1, 2, 3)) should be ("Array(1, 2, 3)")
    }
    it("should pretty print wrapped arrays") {
      Prettifier.basic(WrappedArray.make(Array(1, 2, 3))) should be ("Array(1, 2, 3)")
    }
    it("should pretty print string arrays") {
      Prettifier.basic(Array("1", "2", "3")) should be ("Array(1, 2, 3)")
    }
    it("should pretty print nested string arrays") {
      Prettifier.basic(Array(Array("1", "2", "3"))) should be ("Array(Array(1, 2, 3))")
    }
    it("should pretty print wrapped string arrays") {
      Prettifier.basic(WrappedArray.make(Array("1", "2", "3"))) should be ("Array(1, 2, 3)")
    }
    it("should show null as \"null\"") {
      Prettifier.basic(null) should be ("null")
    }
    it("should clarify the Unit value") {
      Prettifier.basic(()) should be ("<(), the Unit value>")
    }
    it("should just call toString on anything not specially treated") {
      Prettifier.basic(List("1", "2", "3")) should be ("List(1, 2, 3)")
    }
    it("should pretty print Iterable") {
      Prettifier.basic(List(1, 2, 3)) should be ("List(1, 2, 3)")
    }
    it("should pretty print string Iterable") {
      Prettifier.basic(List("1", "2", "3")) should be ("List(1, 2, 3)")
    }
    it("should pretty print nested string Iterable") {
      Prettifier.basic(List(List("1", "2", "3"))) should be ("List(List(1, 2, 3))")
    }
    it("should pretty print Some(Int)") {
      Prettifier.basic(Some(8)) should be ("Some(8)")
    }
    it("should pretty print Some(String)") {
      Prettifier.basic(Some("8")) should be ("Some(8)")
    }
    it("should pretty print nested Some(String)") {
      Prettifier.basic(Some(Some("8"))) should be ("Some(Some(8))")
    }
    it("should pretty print Success(Int)") {
      Prettifier.basic(Success(8)) should be ("Success(8)")
    }
    it("should pretty print Success(String)") {
      Prettifier.basic(Success("8")) should be ("Success(8)")
    }
    it("should pretty print nested Success(String)") {
      Prettifier.basic(Success(Success("8"))) should be ("Success(Success(8))")
    }
    it("should pretty print Left(Int)") {
      Prettifier.basic(Left(8)) should be ("Left(8)")
    }
    it("should pretty print Left(String)") {
      Prettifier.basic(Left("8")) should be ("Left(8)")
    }
    it("should pretty print nested Left(String)") {
      Prettifier.basic(Left(Left("8"))) should be ("Left(Left(8))")
    }
    it("should pretty print Right(Int)") {
      Prettifier.basic(Right(8)) should be ("Right(8)")
    }
    it("should pretty print Right(String)") {
      Prettifier.basic(Right("8")) should be ("Right(8)")
    }
    it("should pretty print nested Right(String)") {
      Prettifier.basic(Right(Right("8"))) should be ("Right(Right(8))")
    }
    it("should pretty print Good(Int)") {
      Prettifier.basic(Good(8)) should be ("Good(8)")
    }
    it("should pretty print Good(String)") {
      Prettifier.basic(Good("8")) should be ("Good(8)")
    }
    it("should pretty print nested Good(String)") {
      Prettifier.basic(Good(Good("8"))) should be ("Good(Good(8))")
    }
    it("should pretty print Bad(Int)") {
      Prettifier.basic(Bad(8)) should be ("Bad(8)")
    }
    it("should pretty print Bad(String)") {
      Prettifier.basic(Bad("8")) should be ("Bad(8)")
    }
    it("should pretty print nested Bad(String)") {
      Prettifier.basic(Bad(Bad("8"))) should be ("Bad(Bad(8))")
    }
    it("should pretty print One(Int)") {
      Prettifier.basic(One(8)) should be ("One(8)")
    }
    it("should pretty print One(String)") {
      Prettifier.basic(One("8")) should be ("One(8)")
    }
    it("should pretty print nested One(String)") {
      Prettifier.basic(One(One("8"))) should be ("One(One(8))")
    }
    it("should pretty print Many(Int)") {
      Prettifier.basic(Many(1, 2, 3)) should be ("Many(1, 2, 3)")
    }
    it("should pretty print Many(String)") {
      Prettifier.basic(Many("1", "2", "3")) should be ("Many(1, 2, 3)")
    }
    it("should pretty print nested Many(String)") {
      Prettifier.basic(Many(Many("1", "2", "3"), Many("7", "8", "9"))) should be ("Many(Many(1, 2, 3), Many(7, 8, 9))")
    }
    it("should support custom Differ") {
      val d = 
        new Differ {
          def difference(a: Any, b: Any, prettifier: Prettifier): PrettyPair = {
            PrettyPair(a.toString, b.toString, None)
          }
        }
      val p = Prettifier(Prettifier.basic, d)  
      val pair = p("test 1", "test 2")
      pair shouldBe PrettyPair("test 1", "test 2", None)
    }
    // SKIP-SCALATESTJS,NATIVE-START
    it("should pretty print Java List") {
      Prettifier.basic(javaList(1, 2, 3)) should be ("[1, 2, 3]")
    }
    it("should pretty print string Java List") {
      Prettifier.basic(javaList("1", "2", "3")) should be ("[1, 2, 3]")
    }
    it("should pretty print nested string Java List") {
      Prettifier.basic(javaList(javaList("1", "2", "3"))) should be ("[[1, 2, 3]]")
    }
    it("should pretty print Java Map") {
      Prettifier.basic(javaSortedMap(Entry(1, 2), Entry(2, 3), Entry(3, 8))) should be ("{1=2, 2=3, 3=8}")
    }
    it("should pretty print string Java Map") {
      Prettifier.basic(javaSortedMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))) should be ("{1=one, 2=two, 3=three}")
    }
    it("should pretty print nested string Java Map") {
      Prettifier.basic(javaSortedMap(Entry("akey", javaSortedMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))))) should be ("{akey={1=one, 2=two, 3=three}}")
    }
    // SKIP-SCALATESTJS,NATIVE-END
  }

  describe("the default Prettifier") {
    it("should put double quotes around strings") {
      Prettifier.default("hi") should be ("\"hi\"")
    }
    it("should put double quotes around scala.collection.immutable.StringOps") {
      Prettifier.default(new scala.collection.immutable.StringOps("hi")) should be ("\"hi\"")
    }
    it("should put single quotes around chars") {
      Prettifier.default('h') should be ("'h'")
    }
    it("should pretty print arrays") {
      Prettifier.default(Array(1, 2, 3)) should be ("Array(1, 2, 3)")
    }
    it("should pretty print wrapped arrays") {
      Prettifier.default(WrappedArray.make(Array(1, 2, 3))) should be ("Array(1, 2, 3)")
    }
    it("should pretty print string arrays") {
      Prettifier.default(Array("1", "2", "3")) should be ("Array(\"1\", \"2\", \"3\")")
    }
    it("should pretty print nested string arrays") {
      Prettifier.default(Array(Array("1", "2", "3"))) should be ("Array(Array(\"1\", \"2\", \"3\"))")
    }
    it("should pretty print wrapped string arrays") {
      Prettifier.default(WrappedArray.make(Array("1", "2", "3"))) should be ("Array(\"1\", \"2\", \"3\")")
    }
    it("should pretty print array ops") {
      Prettifier.default(ArrayHelper.arrayOpsOfInt(Array(1, 2, 3))) should be ("Array(1, 2, 3)")
    }
    it("should show null as \"null\"") {
      Prettifier.default(null) should be ("null")
    }
    it("should clarify the Unit value") {
      Prettifier.default(()) should be ("<(), the Unit value>")
    }
    it("should just call toString on anything not specially treated") {
      Prettifier.default(List("1", "2", "3")) should be ("List(\"1\", \"2\", \"3\")")
    }
    it("should pretty print Iterable") {
      Prettifier.default(List(1, 2, 3)) should be ("List(1, 2, 3)")
    }
    it("should pretty print string Iterable") {
      Prettifier.default(List("1", "2", "3")) should be ("List(\"1\", \"2\", \"3\")")
    }
    it("should pretty print nested string Iterable") {
      Prettifier.default(List(List("1", "2", "3"))) should be ("List(List(\"1\", \"2\", \"3\"))")
    }
    it("should pretty print Some(Int)") {
      Prettifier.default(Some(8)) should be ("Some(8)")
    }
    it("should pretty print Some(String)") {
      Prettifier.default(Some("8")) should be ("Some(\"8\")")
    }
    it("should pretty print nested Some(String)") {
      Prettifier.default(Some(Some("8"))) should be ("Some(Some(\"8\"))")
    }
    it("should pretty print Success(Int)") {
      Prettifier.default(Success(8)) should be ("Success(8)")
    }
    it("should pretty print Success(String)") {
      Prettifier.default(Success("8")) should be ("Success(\"8\")")
    }
    it("should pretty print nested Success(String)") {
      Prettifier.default(Success(Success("8"))) should be ("Success(Success(\"8\"))")
    }
    it("should pretty print Left(Int)") {
      Prettifier.default(Left(8)) should be ("Left(8)")
    }
    it("should pretty print Left(String)") {
      Prettifier.default(Left("8")) should be ("Left(\"8\")")
    }
    it("should pretty print nested Left(String)") {
      Prettifier.default(Left(Left("8"))) should be ("Left(Left(\"8\"))")
    }
    it("should pretty print Right(Int)") {
      Prettifier.default(Right(8)) should be ("Right(8)")
    }
    it("should pretty print Right(String)") {
      Prettifier.default(Right("8")) should be ("Right(\"8\")")
    }
    it("should pretty print nested Right(String)") {
      Prettifier.default(Right(Right("8"))) should be ("Right(Right(\"8\"))")
    }
    it("should pretty print Good(Int)") {
      Prettifier.default(Good(8)) should be ("Good(8)")
    }
    it("should pretty print Good(String)") {
      Prettifier.default(Good("8")) should be ("Good(\"8\")")
    }
    it("should pretty print nested Good(String)") {
      Prettifier.default(Good(Good("8"))) should be ("Good(Good(\"8\"))")
    }
    it("should pretty print Bad(Int)") {
      Prettifier.default(Bad(8)) should be ("Bad(8)")
    }
    it("should pretty print Bad(String)") {
      Prettifier.default(Bad("8")) should be ("Bad(\"8\")")
    }
    it("should pretty print nested Bad(String)") {
      Prettifier.default(Bad(Bad("8"))) should be ("Bad(Bad(\"8\"))")
    }
    it("should pretty print One(Int)") {
      Prettifier.default(One(8)) should be ("One(8)")
    }
    it("should pretty print One(String)") {
      Prettifier.default(One("8")) should be ("One(\"8\")")
    }
    it("should pretty print nested One(String)") {
      Prettifier.default(One(One("8"))) should be ("One(One(\"8\"))")
    }
    it("should pretty print Many(Int)") {
      Prettifier.default(Many(1, 2, 3)) should be ("Many(1, 2, 3)")
    }
    it("should pretty print Many(String)") {
      Prettifier.default(Many("1", "2", "3")) should be ("Many(\"1\", \"2\", \"3\")")
    }
    it("should pretty print nested Many(String)") {
      Prettifier.default(Many(Many("1", "2", "3"), Many("7", "8", "9"))) should be ("Many(Many(\"1\", \"2\", \"3\"), Many(\"7\", \"8\", \"9\"))")
    }
    it("should support custom Differ") {
      val d = 
        new Differ {
          def difference(a: Any, b: Any, prettifier: Prettifier): PrettyPair = {
            PrettyPair(a.toString, b.toString, None)
          }
        }
      val p = Prettifier(Prettifier.default, d)
      val pair = p("test 1", "test 2")
      pair shouldBe PrettyPair("test 1", "test 2", None)
    }
    // SKIP-SCALATESTJS,NATIVE-START
    it("should pretty print Java List") {
      Prettifier.default(javaList(1, 2, 3)) should be ("[1, 2, 3]")
    }
    it("should pretty print string Java List") {
      Prettifier.default(javaList("1", "2", "3")) should be ("[\"1\", \"2\", \"3\"]")
    }
    it("should pretty print nested string Java List") {
      Prettifier.default(javaList(javaList("1", "2", "3"))) should be ("[[\"1\", \"2\", \"3\"]]")
    }
    it("should pretty print Java Map") {
      Prettifier.default(javaSortedMap(Entry(1, 2), Entry(2, 3), Entry(3, 8))) should be ("{1=2, 2=3, 3=8}")
    }
    it("should pretty print string Java Map") {
      Prettifier.default(javaSortedMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))) should be ("{1=\"one\", 2=\"two\", 3=\"three\"}")
    }
    it("should pretty print nested string Java Map") {
      Prettifier.default(javaSortedMap(Entry("akey", javaSortedMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))))) should be ("{\"akey\"={1=\"one\", 2=\"two\", 3=\"three\"}}")
    }
    case class CaseClazzWithArray(data: Array[Int])
    it("should pretty print data inside a case class") {
      Prettifier.default(CaseClazzWithArray(Array(1,2,3))) should be ("CaseClazzWithArray(Array(1, 2, 3))")
    }
    it("should pretty print xml <a></a>") {
      Prettifier.default(<a></a>) should be ("<a></a>")
    }
    it("should pretty print xml <a/>") {
      Prettifier.default(<a/>) should be ("<a/>")
    }
    it("should pretty print xml <a><b/></a>") {
      Prettifier.default(<a><b/></a>) should be ("<a><b/></a>")
    }
    it("should pretty print xml <a/><b/>") {
      Prettifier.default(<a/><b/>) should be ("<a/><b/>")
    }
    it("should pretty print xml.NodeSeq <a/><b/>") {
      val ab: NodeSeq = <a/><b/>;
      Prettifier.default(ab) should be ("<a/><b/>")
    }
    // SKIP-SCALATESTJS,NATIVE-END
    // SKIP-DOTTY-START
    it("should handle runaway recursion gracefully, if not necessarily quickly") {
      /*
        You'd think no one would do this, but:

        scala> val me = <a></a>
        me: scala.xml.Elem = <a></a>

        scala> val you = me.iterator.next
        you: scala.xml.Node = <a></a>

        scala> me eq you
        res0: Boolean = true
      */
      class Fred extends scala.collection.Seq[Fred] { thisFred =>
        //override def toIterator: Iterator[Fred] = iterator
        def iterator: Iterator[Fred] =
          new Iterator[Fred] {
            private var hasNextElement: Boolean = true
            def next(): Fred = {
              if (hasNextElement) {
                hasNextElement = false
                thisFred
              }
              else throw new NoSuchElementException
            }
            def hasNext: Boolean = hasNextElement
          }
        def apply(idx: Int): Fred = if (idx == 0) thisFred else throw new NoSuchElementException
        def length: Int = 1
        override def toString = "It's Fred all the way down"
      }
      Prettifier.default(new Fred) shouldBe "It's Fred all the way down"
    }
    // SKIP-DOTTY-END

    case class Person(name: String, age: Int)

    it("should pretty print case class") {
      val p = Person("John Lee", 35)
      Prettifier.default(p) should be ("Person(\"John Lee\", 35)")
    }

    it("should pretty print Tuple") {
      Prettifier.default(("John Lee", 35)) should be ("(\"John Lee\", 35)")
    }
  }

  describe("the truncating Prettifier") {
    it("should truncate collection") {
      val col = List(1, 2, 3)
      val prettifier = Prettifier.truncateAt(SizeLimit(2))
      prettifier(col) shouldBe "List(1, 2, ...)"
    }

    case class CaseClazz(data: List[Int])

    it("should truncate collection inside of a case class") {
      val caseClass = CaseClazz(List(1, 2, 3))
      val prettifier = Prettifier.truncateAt(SizeLimit(2))
      prettifier(caseClass) shouldBe "CaseClazz(List(1, 2, ...))"
    }

    it("should pretty print Tuple") {
      val prettifier = Prettifier.truncateAt(SizeLimit(2))
      prettifier(("John Lee", 35)) should be ("(\"John Lee\", 35)")
    }

    it("should support custom Differ") {
      val d = 
        new Differ {
          def difference(a: Any, b: Any, prettifier: Prettifier): PrettyPair = {
            PrettyPair(a.toString, b.toString, None)
          }
        }
      val p = Prettifier(Prettifier.truncateAt(SizeLimit(2)), d)  
      val pair = p("test 1", "test 2")
      pair shouldBe PrettyPair("test 1", "test 2", None)
    }
  }
}

