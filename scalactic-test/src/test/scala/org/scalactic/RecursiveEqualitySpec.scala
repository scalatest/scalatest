/*
 * Copyright 2001-2014 Artima, Inc.
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

class RecursiveEqualitySpec extends FunSpec with Matchers with NonImplicitAssertions {

  describe("A List") {
    implicit val strEq = StringNormalizations.lowerCased.toEquality

    it("should NOT do recursive equality under all policies by default") {
      new UncheckedEquality {
        List("HI", "There") shouldEqual List("HI", "There")
        List("HI", "There") should not equal List("HI", "There", "foo")
        List("HI", "There") should not equal List("hi", "there")

        List(List("HI", "There"), List("aBc", "DeF")) shouldEqual List(List("HI", "There"), List("aBc", "DeF"))
        List(List("HI", "There"), List("aBc", "DeF")) should not equal List(List("HI", "There"), List("aBc", "DeF"), "foo")
        List(List("HI", "There"), List("aBc", "DeF")) should not equal List(List("hi", "there"), List("abc", "def"))

        List(List(List("HI", "There"))) should not equal List(List(List("hi", "there")))
        List(List(List("HIGH", "Their"))) should not equal List(List(List("hi", "there")))

        List(Nil) shouldEqual List(Nil)
        List(List(Nil), List(Nil)) shouldEqual List(List(Nil), List(Nil))
        List(List(Nil)) should not equal List(Nil)
      }

      new CheckedEquality {
        List("HI", "There") shouldEqual List("HI", "There")
        List("HI", "There") should not equal List("HI", "There", "foo")
        List("HI", "There") should not equal List("hi", "there")

        List(List("HI", "There"), List("aBc", "DeF")) shouldEqual List(List("HI", "There"), List("aBc", "DeF"))
        List(List("HI", "There"), List("aBc", "DeF")) should not equal List(List("HI", "There"), List("aBc", "DeF", "foo"))
        List(List("HI", "There"), List("aBc", "DeF")) should not equal List(List("hi", "there"), List("abc", "def"), "foo")

        List(List(List("HI", "There"))) should not equal List(List(List("hi", "there")))
        List(List(List("HIGH", "Their"))) should not equal List(List(List("hi", "there")))
      }
    }

    it("should do recursive equality under any policy under RecursiveSeqEquality") {
      import RecursiveSeqEquality._

      new UncheckedEquality {
        List("HI", "There") shouldEqual List("HI", "There")
        List("HI", "There") should not equal List("HI", "There", "foo")
        List("HI", "There") shouldEqual List("hi", "there")

        List(List("HI", "There"), List("aBc", "DeF")) shouldEqual List(List("HI", "There"), List("aBc", "DeF"))
        List(List("HI", "There"), List("aBc", "DeF")) should not equal List(List("HI", "There"), List("aBc", "DeF", "foo"))
        List(List("HI", "There"), List("aBc", "DeF")) shouldEqual List(List("hi", "there"), List("abc", "def"))


        List(List(List("HI", "There"))) shouldEqual List(List(List("hi", "there")))
        List(List(List("HIGH", "Their"))) should not equal List(List(List("hi", "there")))

        List(Nil) shouldEqual List(Nil)
        List(List(Nil), List(Nil)) shouldEqual List(List(Nil), List(Nil))
        List(List(Nil)) should not equal List(Nil)
      }

      new CheckedEquality {
        List("HI", "There") shouldEqual List("HI", "There")
        List("HI", "There") should not equal List("HI", "There", "foo")
        List("HI", "There") shouldEqual List("hi", "there")

        List(List("HI", "There"), List("aBc", "DeF")) shouldEqual List(List("HI", "There"), List("aBc", "DeF"))
        List(List("HI", "There"), List("aBc", "DeF")) should not equal List(List("HI", "There"), List("aBc", "DeF", "foo"))
        List(List("HI", "There"), List("aBc", "DeF")) shouldEqual List(List("hi", "there"), List("abc", "def"))


        List(List(List("HI", "There"))) shouldEqual List(List(List("hi", "there")))
        List(List(List("HIGH", "Their"))) should not equal List(List(List("hi", "there")))
      }
    }
  }

  describe("A Set") {
    implicit val strEq = StringNormalizations.lowerCased.toEquality

    it("should NOT do recursive equality under all policies by default") {
      new UncheckedEquality {
        Set("HI", "There") shouldEqual Set("There", "HI")
        Set("HI", "There") should not equal Set("There", "HI", "foo")
        Set("HI", "There") should not equal Set("there", "hi")

        Set(Set("HI", "There"), Set("aBc", "DeF")) shouldEqual Set(Set("There", "HI"), Set("aBc", "DeF"))
        Set(Set("HI", "There"), Set("aBc", "DeF")) should not equal Set(Set("DeF", "aBc"), Set("There", "HI"), "foo")
        Set(Set("HI", "There"), Set("aBc", "DeF")) should not equal Set(Set("def", "abc"), Set("there", "hi"))

        Set(Set(Set("HIGH", "Their"))) should not equal Set(Set(Set("hi", "there")))
        Set(Set(Set("HI", "There"))) should not equal Set(Set(Set("hi", "there")))

        Set(Nil) shouldEqual Set(Nil)
        Set(Set(Nil), Set(Nil)) shouldEqual Set(Set(Nil), Set(Nil))
        Set(Set(Nil)) should not equal Set(Nil)
      }

      new CheckedEquality {
        Set("HI", "There") shouldEqual Set("There", "HI")
        Set("HI", "There") should not equal Set("There", "HI", "foo")
        Set("HI", "There") should not equal Set("there", "hi")

        Set(Set("HI", "There"), Set("aBc", "DeF")) shouldEqual Set(Set("There", "HI"), Set("aBc", "DeF"))
        Set(Set("HI", "There"), Set("aBc", "DeF")) should not equal Set(Set("DeF", "aBc"), Set("There", "HI"), "foo")
        Set(Set("HI", "There"), Set("aBc", "DeF")) should not equal Set(Set("def", "abc"), Set("there", "hi"))

        Set(Set(Set("HIGH", "Their"))) should not equal Set(Set(Set("hi", "there")))
        Set(Set(Set("HI", "There"))) should not equal Set(Set(Set("hi", "there")))

        Set(Nil) shouldEqual Set(Nil)
        Set(Set(Nil), Set(Nil)) shouldEqual Set(Set(Nil), Set(Nil))
      }
    }

    it("should do recursive equality under any policy under RecursiveSetEquality") {
      import RecursiveSetEquality._

      new UncheckedEquality {

        Set("HI", "There") shouldEqual Set("There", "HI")
        Set("HI", "There") should not equal Set("There", "HI", "foo")
        Set("HI", "There") shouldEqual Set("there", "hi")

        Set(Set("HI", "There"), Set("aBc", "DeF")) shouldEqual Set(Set("There", "HI"), Set("aBc", "DeF"))
        Set(Set("HI", "There"), Set("aBc", "DeF")) should not equal Set(Set("There", "HI"), Set("DeF", "aBc"), "foo")
        Set(Set("HI", "There"), Set("aBc", "DeF")) shouldEqual Set(Set("there", "hi"), Set("def", "abc"))

        Set(Set(Set("HIGH", "Their"))) should not equal Set(Set(Set("hi", "there")))
        Set(Set(Set("HI", "There"))) shouldEqual Set(Set(Set("hi", "there")))

        Set(Nil) shouldEqual Set(Nil)
        Set(Set(Nil), Set(Nil)) shouldEqual Set(Set(Nil), Set(Nil))
      }

      new CheckedEquality {

        Set("HI", "There") shouldEqual Set("There", "HI")
        Set("HI", "There") should not equal Set("There", "HI", "foo")
        Set("HI", "There") shouldEqual Set("there", "hi")

        Set(Set("HI", "There"), Set("aBc", "DeF")) shouldEqual Set(Set("There", "HI"), Set("aBc", "DeF"))
        Set(Set("HI", "There"), Set("aBc", "DeF")) should not equal Set(Set("There", "HI"), Set("DeF", "aBc"), "foo")
        Set(Set("HI", "There"), Set("aBc", "DeF")) shouldEqual Set(Set("there", "hi"), Set("def", "abc"))

        Set(Set(Set("HIGH", "Their"))) should not equal Set(Set(Set("hi", "there")))
        Set(Set(Set("HI", "There"))) shouldEqual Set(Set(Set("hi", "there")))

        Set(Nil) shouldEqual Set(Nil)
        Set(Set(Nil), Set(Nil)) shouldEqual Set(Set(Nil), Set(Nil))
      }
    }
  }

  describe("A Map") {
    implicit val strEq = StringNormalizations.lowerCased.toEquality

    it("should NOT do recursive equality under all policies by default") {
      new UncheckedEquality {
        Map("HI" -> "High", "THERE" -> "Their") shouldEqual Map("HI" -> "High", "THERE" -> "Their")
        Map("HI" -> "High", "THERE" -> "Their") should not equal Map("hi" -> "high", "there" -> "their")

        Map("Key1" -> Map("HI" -> "Hello")) should not equal Map("key1" -> Map("hi" -> "hello"))
        Map("Key1" -> Map("HI" -> "Hello"), "Key2" -> Map("aBc" -> "DeF")) shouldEqual Map("Key2" -> Map("aBc" -> "DeF"), "Key1" -> Map("HI" -> "Hello"))
        Map("Key1" -> Map("HI" -> "Hello"), "Key2" -> Map("aBc" -> "DeF")) should not equal Map("key2" -> Map("abc" -> "def"), "key1" -> Map("hi" -> "hello"))
        Map("Key1" -> Map.empty[String,String], "Key2" -> Map.empty[String,String]) should not equal Map("key2" -> Map.empty[String,String], "key1" -> Map.empty[String,String])
      }

      new CheckedEquality {
        Map("HI" -> "High", "THERE" -> "Their") shouldEqual Map("HI" -> "High", "THERE" -> "Their")
        Map("HI" -> "High", "THERE" -> "Their") should not equal Map("hi" -> "high", "there" -> "their")

        Map("Key1" -> Map("HI" -> "Hello")) should not equal Map("key1" -> Map("hi" -> "hello"))
        Map("Key1" -> Map("HI" -> "Hello"), "Key2" -> Map("aBc" -> "DeF")) shouldEqual Map("Key2" -> Map("aBc" -> "DeF"), "Key1" -> Map("HI" -> "Hello"))
        Map("Key1" -> Map("HI" -> "Hello"), "Key2" -> Map("aBc" -> "DeF")) should not equal Map("key2" -> Map("abc" -> "def"), "key1" -> Map("hi" -> "hello"))
        Map("Key1" -> Map.empty[String,String], "Key2" -> Map.empty[String,String]) should not equal Map("key2" -> Map.empty[String,String], "key1" -> Map.empty[String,String])
      }
    }

    it("should do recursive equality under any policy under RecursiveSetEquality") {
      import RecursiveMapEquality._

      new UncheckedEquality {
        Map("HI" -> "High", "THERE" -> "Their") shouldEqual Map("HI" -> "High", "THERE" -> "Their")
        Map("HI" -> "High", "THERE" -> "Their") shouldEqual Map("hi" -> "high", "there" -> "their")

        Map("Key1" -> Map("HI" -> "Hello")) shouldEqual Map("key1" -> Map("hi" -> "hello"))
        Map("Key1" -> Map("HI" -> "Hello"), "Key2" -> Map("aBc" -> "DeF")) shouldEqual Map("Key2" -> Map("aBc" -> "DeF"), "Key1" -> Map("HI" -> "Hello"))
        Map("Key1" -> Map("HI" -> "Hello"), "Key2" -> Map("aBc" -> "DeF")) shouldEqual Map("key2" -> Map("abc" -> "def"), "key1" -> Map("hi" -> "hello"))
        Map("Key1" -> Map.empty[String,String], "Key2" -> Map.empty[String,String]) shouldEqual Map("key2" -> Map.empty[String,String], "key1" -> Map.empty[String,String])
      }

      new CheckedEquality {
        Map("HI" -> "High", "THERE" -> "Their") shouldEqual Map("HI" -> "High", "THERE" -> "Their")
        Map("HI" -> "High", "THERE" -> "Their") shouldEqual Map("hi" -> "high", "there" -> "their")

        Map("Key1" -> Map("HI" -> "Hello")) shouldEqual Map("key1" -> Map("hi" -> "hello"))
        Map("Key1" -> Map("HI" -> "Hello"), "Key2" -> Map("aBc" -> "DeF")) shouldEqual Map("Key2" -> Map("aBc" -> "DeF"), "Key1" -> Map("HI" -> "Hello"))
        Map("Key1" -> Map("HI" -> "Hello"), "Key2" -> Map("aBc" -> "DeF")) shouldEqual Map("key2" -> Map("abc" -> "def"), "key1" -> Map("hi" -> "hello"))
        Map("Key1" -> Map.empty[String,String], "Key2" -> Map.empty[String,String]) shouldEqual Map("key2" -> Map.empty[String,String], "key1" -> Map.empty[String,String])
      }
    }
  }

  describe("An Option") {

    implicit val strEq = StringNormalizations.lowerCased.toEquality

    it("should NOT do recursive equality under the all policies by default") {

      // New policies
      // Both sides Some
      new UncheckedEquality {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      new CheckedEquality {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      new EnabledEquality {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      // Both sides Option
      new UncheckedEquality {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      new CheckedEquality {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      new EnabledEquality {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      // Left side Some, right side Option
      new UncheckedEquality {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      new CheckedEquality {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      new EnabledEquality {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      // Left side Option, right side Some
      new UncheckedEquality {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }
      new CheckedEquality {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }
      new EnabledEquality {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }

      // Deprecated policies
      // Both sides Some
      new TripleEquals {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      new TypeCheckedTripleEquals {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      new ConversionCheckedTripleEquals {
        Some("hi") should not equal Some("HI")
        Some(Some("hi")) should not equal Some(Some("HI"))
        Option(Some("hi")) should not equal Option(Some("HI"))
        Some(Some("hi")) should not equal Option(Some("HI"))
        Option(Some("hi")) should not equal Some(Some("HI"))
      }
      // Both sides Option
      new TripleEquals {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      new TypeCheckedTripleEquals {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      new ConversionCheckedTripleEquals {
        Option("hi") should not equal Option("HI")
        Some(Option("hi")) should not equal Some(Option("HI"))
        Option(Option("hi")) should not equal Option(Option("HI"))
        Some(Option("hi")) should not equal Option(Option("HI"))
        Option(Option("hi")) should not equal Some(Option("HI"))
      }
      // Left side Some, right side Option
      new TripleEquals {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      new TypeCheckedTripleEquals {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      new ConversionCheckedTripleEquals {
        Some("hi") should not equal Option("HI")
        Some(Some("hi")) should not equal Some(Option("HI"))
        Option(Some("hi")) should not equal Option(Option("HI"))
        Some(Some("hi")) should not equal Option(Option("HI"))
        Option(Some("hi")) should not equal Some(Option("HI"))
      }
      // Left side Option, right side Some
      new TripleEquals {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }
      new TypeCheckedTripleEquals {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }
      new ConversionCheckedTripleEquals {
        Option("hi") should not equal Some("HI")
        Some(Option("hi")) should not equal Some(Some("HI"))
        Option(Option("hi")) should not equal Option(Some("HI"))
        Some(Option("hi")) should not equal Option(Some("HI"))
        Option(Option("hi")) should not equal Some(Some("HI"))
      }
    }

    it("should do recursive equality under any policy under RecursiveOptionEquality") {

      import RecursiveOptionEquality._

      // New policies
      // Both sides Some
      new UncheckedEquality {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      new CheckedEquality {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      new EnabledEquality {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      // Both sides Option
      new UncheckedEquality {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      new CheckedEquality {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      new EnabledEquality {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      // Left side Some, right side Option
      new UncheckedEquality {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      new CheckedEquality {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      new EnabledEquality {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      // Left side Option, right side Some
      new UncheckedEquality {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }
      new CheckedEquality {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }
      new EnabledEquality {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }

      // Deprecated policies
      // Both sides Some
      new TripleEquals {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      new TypeCheckedTripleEquals {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      new ConversionCheckedTripleEquals {
        Some("hi") shouldEqual Some("HI")
        Some(Some("hi")) shouldEqual Some(Some("HI"))
        Option(Some("hi")) shouldEqual Option(Some("HI"))
        Some(Some("hi")) shouldEqual Option(Some("HI"))
        Option(Some("hi")) shouldEqual Some(Some("HI"))
      }
      // Both sides Option
      new TripleEquals {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      new TypeCheckedTripleEquals {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      new ConversionCheckedTripleEquals {
        Option("hi") shouldEqual Option("HI")
        Some(Option("hi")) shouldEqual Some(Option("HI"))
        Option(Option("hi")) shouldEqual Option(Option("HI"))
        Some(Option("hi")) shouldEqual Option(Option("HI"))
        Option(Option("hi")) shouldEqual Some(Option("HI"))
      }
      // Left side Some, right side Option
      new TripleEquals {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      new TypeCheckedTripleEquals {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      new ConversionCheckedTripleEquals {
        Some("hi") shouldEqual Option("HI")
        Some(Some("hi")) shouldEqual Some(Option("HI"))
        Option(Some("hi")) shouldEqual Option(Option("HI"))
        Some(Some("hi")) shouldEqual Option(Option("HI"))
        Option(Some("hi")) shouldEqual Some(Option("HI"))
      }
      // Left side Option, right side Some
      new TripleEquals {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }
      new TypeCheckedTripleEquals {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }
      new ConversionCheckedTripleEquals {
        Option("hi") shouldEqual Some("HI")
        Some(Option("hi")) shouldEqual Some(Some("HI"))
        Option(Option("hi")) shouldEqual Option(Some("HI"))
        Some(Option("hi")) shouldEqual Option(Some("HI"))
        Option(Option("hi")) shouldEqual Some(Some("HI"))
      }
    }
  }
}

