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
package org.scalautils

import org.scalatest._
import scala.collection.mutable.WrappedArray
import scala.util.Success
import SharedHelpers.{javaList, javaSortedMap}

class PrettifierSpec extends Spec with Matchers {
  object `A Prettifier` {
    def `should convert an Any to a String` {
      val f =
        new Prettifier {
          def apply(o: Any) = o.toString
        }

      f("hi") should be ("hi")
      f(List("hi")) should be ("List(hi)")
    }
    def `can be composed with another Prettifier` {
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
      myLittlePretty('h') should be ("'h'")
      myLittlePretty(Array(1, 2, 3)) should be ("Array(1, 2, 3)")
      myLittlePretty(WrappedArray.make(Array(1, 2, 3))) should be ("Array(1, 2, 3)")
      myLittlePretty(null) should be ("null")
      myLittlePretty(()) should be ("<(), the Unit value>")
      myLittlePretty(List("1", "2", "3")) should be ("List(\"1\", \"2\", \"3\")")
    }
  }

  object `the basic Prettifier` {
    def `should put double quotes around strings` {
      Prettifier.basic("hi") should be ("\"hi\"")
    }
    def `should put single quotes around chars` {
      Prettifier.basic('h') should be ("'h'")
    }
    def `should pretty print arrays` {
      Prettifier.basic(Array(1, 2, 3)) should be ("Array(1, 2, 3)")
    }
    def `should pretty print wrapped arrays` {
      Prettifier.basic(WrappedArray.make(Array(1, 2, 3))) should be ("Array(1, 2, 3)")
    }
    def `should pretty print string arrays` {
      Prettifier.basic(Array("1", "2", "3")) should be ("Array(1, 2, 3)")
    }
    def `should pretty print nested string arrays` {
      Prettifier.basic(Array(Array("1", "2", "3"))) should be ("Array(Array(1, 2, 3))")
    }
    def `should pretty print wrapped string arrays` {
      Prettifier.basic(WrappedArray.make(Array("1", "2", "3"))) should be ("Array(1, 2, 3)")
    }
    def `should show null as "null"` {
      Prettifier.basic(null) should be ("null")
    }
    def `should clarify the Unit value` {
      Prettifier.basic(()) should be ("<(), the Unit value>")
    }
    def `should just call toString on anything not specially treated` {
      Prettifier.basic(List("1", "2", "3")) should be ("List(1, 2, 3)")
    }
    def `should pretty print GenTraversable` {
      Prettifier.basic(List(1, 2, 3)) should be ("List(1, 2, 3)")
    }
    def `should pretty print string GenTraversable` {
      Prettifier.basic(List("1", "2", "3")) should be ("List(1, 2, 3)")
    }
    def `should pretty print nested string GenTraversable` {
      Prettifier.basic(List(List("1", "2", "3"))) should be ("List(List(1, 2, 3))")
    }
    def `should pretty print Some(Int)` {
      Prettifier.basic(Some(8)) should be ("Some(8)")
    }
    def `should pretty print Some(String)` {
      Prettifier.basic(Some("8")) should be ("Some(8)")
    }
    def `should pretty print nested Some(String)` {
      Prettifier.basic(Some(Some("8"))) should be ("Some(Some(8))")
    }
    def `should pretty print Success(Int)` {
      Prettifier.basic(Success(8)) should be ("Success(8)")
    }
    def `should pretty print Success(String)` {
      Prettifier.basic(Success("8")) should be ("Success(8)")
    }
    def `should pretty print nested Success(String)` {
      Prettifier.basic(Success(Success("8"))) should be ("Success(Success(8))")
    }
    def `should pretty print Left(Int)` {
      Prettifier.basic(Left(8)) should be ("Left(8)")
    }
    def `should pretty print Left(String)` {
      Prettifier.basic(Left("8")) should be ("Left(8)")
    }
    def `should pretty print nested Left(String)` {
      Prettifier.basic(Left(Left("8"))) should be ("Left(Left(8))")
    }
    def `should pretty print Right(Int)` {
      Prettifier.basic(Right(8)) should be ("Right(8)")
    }
    def `should pretty print Right(String)` {
      Prettifier.basic(Right("8")) should be ("Right(8)")
    }
    def `should pretty print nested Right(String)` {
      Prettifier.basic(Right(Right("8"))) should be ("Right(Right(8))")
    }
    def `should pretty print Good(Int)` {
      Prettifier.basic(Good(8)) should be ("Good(8)")
    }
    def `should pretty print Good(String)` {
      Prettifier.basic(Good("8")) should be ("Good(8)")
    }
    def `should pretty print nested Good(String)` {
      Prettifier.basic(Good(Good("8"))) should be ("Good(Good(8))")
    }
    def `should pretty print Bad(Int)` {
      Prettifier.basic(Bad(8)) should be ("Bad(8)")
    }
    def `should pretty print Bad(String)` {
      Prettifier.basic(Bad("8")) should be ("Bad(8)")
    }
    def `should pretty print nested Bad(String)` {
      Prettifier.basic(Bad(Bad("8"))) should be ("Bad(Bad(8))")
    }
    def `should pretty print One(Int)` {
      Prettifier.basic(One(8)) should be ("One(8)")
    }
    def `should pretty print One(String)` {
      Prettifier.basic(One("8")) should be ("One(8)")
    }
    def `should pretty print nested One(String)` {
      Prettifier.basic(One(One("8"))) should be ("One(One(8))")
    }
    def `should pretty print Many(Int)` {
      Prettifier.basic(Many(1, 2, 3)) should be ("Many(1, 2, 3)")
    }
    def `should pretty print Many(String)` {
      Prettifier.basic(Many("1", "2", "3")) should be ("Many(1, 2, 3)")
    }
    def `should pretty print nested Many(String)` {
      Prettifier.basic(Many(Many("1", "2", "3"), Many("7", "8", "9"))) should be ("Many(Many(1, 2, 3), Many(7, 8, 9))")
    }
    def `should pretty print Java List` {
      Prettifier.basic(javaList(1, 2, 3)) should be ("[1, 2, 3]")
    }
    def `should pretty print string Java List` {
      Prettifier.basic(javaList("1", "2", "3")) should be ("[1, 2, 3]")
    }
    def `should pretty print nested string Java List` {
      Prettifier.basic(javaList(javaList("1", "2", "3"))) should be ("[[1, 2, 3]]")
    }
    def `should pretty print Java Map` {
      Prettifier.basic(javaSortedMap(Entry(1, 2), Entry(2, 3), Entry(3, 8))) should be ("{1=2, 2=3, 3=8}")
    }
    def `should pretty print string Java Map` {
      Prettifier.basic(javaSortedMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))) should be ("{1=one, 2=two, 3=three}")
    }
    def `should pretty print nested string Java Map` {
      Prettifier.basic(javaSortedMap(Entry("akey", javaSortedMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))))) should be ("{akey={1=one, 2=two, 3=three}}")
    }
  }

  object `the default Prettifier` {
    def `should put double quotes around strings` {
      Prettifier.default("hi") should be ("\"hi\"")
    }
    def `should put single quotes around chars` {
      Prettifier.default('h') should be ("'h'")
    }
    def `should pretty print arrays` {
      Prettifier.default(Array(1, 2, 3)) should be ("Array(1, 2, 3)")
    }
    def `should pretty print wrapped arrays` {
      Prettifier.default(WrappedArray.make(Array(1, 2, 3))) should be ("Array(1, 2, 3)")
    }
    def `should pretty print string arrays` {
      Prettifier.default(Array("1", "2", "3")) should be ("Array(\"1\", \"2\", \"3\")")
    }
    def `should pretty print nested string arrays` {
      Prettifier.default(Array(Array("1", "2", "3"))) should be ("Array(Array(\"1\", \"2\", \"3\"))")
    }
    def `should pretty print wrapped string arrays` {
      Prettifier.default(WrappedArray.make(Array("1", "2", "3"))) should be ("Array(\"1\", \"2\", \"3\")")
    }
    def `should show null as "null"` {
      Prettifier.default(null) should be ("null")
    }
    def `should clarify the Unit value` {
      Prettifier.default(()) should be ("<(), the Unit value>")
    }
    def `should just call toString on anything not specially treated` {
      Prettifier.default(List("1", "2", "3")) should be ("List(\"1\", \"2\", \"3\")")
    }
    def `should pretty print GenTraversable` {
      Prettifier.default(List(1, 2, 3)) should be ("List(1, 2, 3)")
    }
    def `should pretty print string GenTraversable` {
      Prettifier.default(List("1", "2", "3")) should be ("List(\"1\", \"2\", \"3\")")
    }
    def `should pretty print nested string GenTraversable` {
      Prettifier.default(List(List("1", "2", "3"))) should be ("List(List(\"1\", \"2\", \"3\"))")
    }
    def `should pretty print Some(Int)` {
      Prettifier.default(Some(8)) should be ("Some(8)")
    }
    def `should pretty print Some(String)` {
      Prettifier.default(Some("8")) should be ("Some(\"8\")")
    }
    def `should pretty print nested Some(String)` {
      Prettifier.default(Some(Some("8"))) should be ("Some(Some(\"8\"))")
    }
    def `should pretty print Success(Int)` {
      Prettifier.default(Success(8)) should be ("Success(8)")
    }
    def `should pretty print Success(String)` {
      Prettifier.default(Success("8")) should be ("Success(\"8\")")
    }
    def `should pretty print nested Success(String)` {
      Prettifier.default(Success(Success("8"))) should be ("Success(Success(\"8\"))")
    }
    def `should pretty print Left(Int)` {
      Prettifier.default(Left(8)) should be ("Left(8)")
    }
    def `should pretty print Left(String)` {
      Prettifier.default(Left("8")) should be ("Left(\"8\")")
    }
    def `should pretty print nested Left(String)` {
      Prettifier.default(Left(Left("8"))) should be ("Left(Left(\"8\"))")
    }
    def `should pretty print Right(Int)` {
      Prettifier.default(Right(8)) should be ("Right(8)")
    }
    def `should pretty print Right(String)` {
      Prettifier.default(Right("8")) should be ("Right(\"8\")")
    }
    def `should pretty print nested Right(String)` {
      Prettifier.default(Right(Right("8"))) should be ("Right(Right(\"8\"))")
    }
    def `should pretty print Good(Int)` {
      Prettifier.default(Good(8)) should be ("Good(8)")
    }
    def `should pretty print Good(String)` {
      Prettifier.default(Good("8")) should be ("Good(\"8\")")
    }
    def `should pretty print nested Good(String)` {
      Prettifier.default(Good(Good("8"))) should be ("Good(Good(\"8\"))")
    }
    def `should pretty print Bad(Int)` {
      Prettifier.default(Bad(8)) should be ("Bad(8)")
    }
    def `should pretty print Bad(String)` {
      Prettifier.default(Bad("8")) should be ("Bad(\"8\")")
    }
    def `should pretty print nested Bad(String)` {
      Prettifier.default(Bad(Bad("8"))) should be ("Bad(Bad(\"8\"))")
    }
    def `should pretty print One(Int)` {
      Prettifier.default(One(8)) should be ("One(8)")
    }
    def `should pretty print One(String)` {
      Prettifier.default(One("8")) should be ("One(\"8\")")
    }
    def `should pretty print nested One(String)` {
      Prettifier.default(One(One("8"))) should be ("One(One(\"8\"))")
    }
    def `should pretty print Many(Int)` {
      Prettifier.default(Many(1, 2, 3)) should be ("Many(1, 2, 3)")
    }
    def `should pretty print Many(String)` {
      Prettifier.default(Many("1", "2", "3")) should be ("Many(\"1\", \"2\", \"3\")")
    }
    def `should pretty print nested Many(String)` {
      Prettifier.default(Many(Many("1", "2", "3"), Many("7", "8", "9"))) should be ("Many(Many(\"1\", \"2\", \"3\"), Many(\"7\", \"8\", \"9\"))")
    }
    def `should pretty print Java List` {
      Prettifier.default(javaList(1, 2, 3)) should be ("[1, 2, 3]")
    }
    def `should pretty print string Java List` {
      Prettifier.default(javaList("1", "2", "3")) should be ("[\"1\", \"2\", \"3\"]")
    }
    def `should pretty print nested string Java List` {
      Prettifier.default(javaList(javaList("1", "2", "3"))) should be ("[[\"1\", \"2\", \"3\"]]")
    }
    def `should pretty print Java Map` {
      Prettifier.default(javaSortedMap(Entry(1, 2), Entry(2, 3), Entry(3, 8))) should be ("{1=2, 2=3, 3=8}")
    }
    def `should pretty print string Java Map` {
      Prettifier.default(javaSortedMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))) should be ("{1=\"one\", 2=\"two\", 3=\"three\"}")
    }
    def `should pretty print nested string Java Map` {
      Prettifier.default(javaSortedMap(Entry("akey", javaSortedMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))))) should be ("{\"akey\"={1=\"one\", 2=\"two\", 3=\"three\"}}")
    }
    def `should pretty print xml` {
      Prettifier.default(<a></a>) should be ("<a></a>")
    }
  }
}

