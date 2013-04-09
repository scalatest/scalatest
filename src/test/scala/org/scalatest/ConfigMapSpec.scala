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
package org.scalatest

import org.scalautils.LegacyTripleEquals
import exceptions.TestCanceledException

class ConfigMapSpec extends Spec with LegacyTripleEquals {

    class Fruit {
      override def toString = "a Fruit"
    }

    class Apple extends Fruit {
      override def toString = "an Apple"
    }

    val fruit = new Fruit
    val apple = new Apple
    val cm = ConfigMap(
      "string" -> "aStringValue",
      "boolean" -> true,
      "Boolean" -> new java.lang.Boolean(true),
      "byte" -> 1.toByte,
      "Byte" -> new java.lang.Byte(1.toByte),
      "short" -> 1.toShort,
      "Short" -> new java.lang.Short(1.toShort),
      "int" -> 1,
      "Integer" -> new java.lang.Integer(1),
      "long" -> Long.MaxValue,
      "Long" -> new java.lang.Long(Long.MaxValue),
      "char" -> 'c',
      "Char" -> new java.lang.Character('c'),
      "float" -> 1.0F,
      "Float" -> new java.lang.Float(1.0F),
      "double" -> 1.0,
      "Double" -> new java.lang.Double(1.0),
      "apple" -> apple,
      "fruit" -> fruit
    )

  object `A ConfigMap` {

    def `provides a nice syntax for getting a required entry` {
      assert(cm.getRequired[String]("string") === "aStringValue")
      assert(cm.getRequired[Int]("int") === 1)
    }

    def `throws a TestCanceledException if a required entry is missing` {
      val caught =
        intercept[TestCanceledException] {
          cm.getRequired[String]("t")
        }
      assert(caught.getMessage === Resources("configMapEntryNotFound", "t"))
    }

    def `throws a TestCanceledException if a required entry has an unexpected type` {

      // Ensure supertype and subype is done correctly
      assert(cm.getRequired[Apple]("apple") === apple)
      assert(cm.getRequired[Fruit]("apple") === apple)
      val caught1 =
        intercept[TestCanceledException] {
          cm.getRequired[Apple]("fruit")
        }
      assert(caught1.getMessage === Resources("configMapEntryHadUnexpectedType", "fruit", "class " + fruit.getClass.getName, "class " + apple.getClass.getName, "a Fruit"))

      // Ensure Boolean works
      assert(cm.getRequired[Boolean]("boolean") === true)
      assert(cm.getRequired[Boolean]("Boolean") === new java.lang.Boolean(true))
      val caught2 =
        intercept[TestCanceledException] {
          cm.getRequired[Boolean]("string")
        }

      assert(caught2.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "boolean", "aStringValue"))

      // Ensure Byte works
      assert(cm.getRequired[Byte]("byte") === 1.toByte)
      assert(cm.getRequired[Byte]("Byte") === new java.lang.Byte(1.toByte))
      val caught3 =
        intercept[TestCanceledException] {
          cm.getRequired[Byte]("string")
        }

      assert(caught3.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "byte", "aStringValue"))

      // Ensure Short works
      assert(cm.getRequired[Short]("short") === 1.toShort)
      assert(cm.getRequired[Short]("Short") === new java.lang.Short(1.toShort))
      val caught4 =
        intercept[TestCanceledException] {
          cm.getRequired[Short]("string")
        }
      assert(caught4.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "short", "aStringValue"))

      // Ensure Int works
      assert(cm.getRequired[Int]("int") === 1)
      assert(cm.getRequired[Int]("Integer") === new java.lang.Integer(1))
      val caught5 =
        intercept[TestCanceledException] {
          cm.getRequired[Int]("string")
        }
      assert(caught5.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "int", "aStringValue"))

      // Ensure Long works
      assert(cm.getRequired[Long]("long") === Long.MaxValue)
      assert(cm.getRequired[Long]("Long") === new java.lang.Long(Long.MaxValue))
      val caught6 =
        intercept[TestCanceledException] {
          cm.getRequired[Long]("string")
        }
      assert(caught6.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "long", "aStringValue"))

      // Ensure Char works
      assert(cm.getRequired[Char]("char") === 'c')
      assert(cm.getRequired[Char]("Char") === new java.lang.Character('c'))
// 510 287 1900
      val caught7 =
        intercept[TestCanceledException] {
          cm.getRequired[Char]("string")
        }
      assert(caught7.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "char", "aStringValue"))

      // Ensure Float works
      assert(cm.getRequired[Float]("float") === 1.0F)
      assert(cm.getRequired[Float]("Float") === new java.lang.Float(1.0F))
      val caught8 =
        intercept[TestCanceledException] {
          cm.getRequired[Float]("string")
        }
      assert(caught8.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "float", "aStringValue"))

      // Ensure Double works
      assert(cm.getRequired[Double]("double") === 1.0)
      assert(cm.getRequired[Double]("Double") === new java.lang.Double(1.0))
      val caught9 =
        intercept[TestCanceledException] {
          cm.getRequired[Double]("string")
        }
      assert(caught9.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "double", "aStringValue"))
    }

    def `provides a nice syntax for getting an optional entry` {
      assert(cm.getOptional[String]("string") === Some("aStringValue"))
      assert(cm.getOptional[Int]("int") === Some(1))
    }

    def `returns None if an optional entry is missing` {
      assert(cm.getOptional[String]("t") === None)
    }

    def `throws a TestCanceledException if an optional entry has an unexpected type` {

      // Ensure supertype and subype is done correctly
      assert(cm.getOptional[Apple]("apple") === Some(apple))
      assert(cm.getOptional[Fruit]("apple") === Some(apple))
      val caught1 =
        intercept[TestCanceledException] {
          cm.getOptional[Apple]("fruit")
        }
      assert(caught1.getMessage === Resources("configMapEntryHadUnexpectedType", "fruit", "class " + fruit.getClass.getName, "class " + apple.getClass.getName, "a Fruit"))

      // Ensure Boolean works
      assert(cm.getOptional[Boolean]("boolean") === Some(true))
      assert(cm.getOptional[Boolean]("Boolean") === Some(new java.lang.Boolean(true)))
      val caught2 =
        intercept[TestCanceledException] {
          cm.getOptional[Boolean]("string")
        }

      assert(caught2.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "boolean", "aStringValue"))

      // Ensure Byte works
      assert(cm.getOptional[Byte]("byte") === Some(1.toByte))
      assert(cm.getOptional[Byte]("Byte") === Some(new java.lang.Byte(1.toByte)))
      val caught3 =
        intercept[TestCanceledException] {
          cm.getOptional[Byte]("string")
        }

      assert(caught3.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "byte", "aStringValue"))

      // Ensure Short works
      assert(cm.getOptional[Short]("short") === Some(1.toShort))
      assert(cm.getOptional[Short]("Short") === Some(new java.lang.Short(1.toShort)))
      val caught4 =
        intercept[TestCanceledException] {
          cm.getOptional[Short]("string")
        }
      assert(caught4.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "short", "aStringValue"))

      // Ensure Int works
      assert(cm.getOptional[Int]("int") === Some(1))
      assert(cm.getOptional[Int]("Integer") === Some(new java.lang.Integer(1)))
      val caught5 =
        intercept[TestCanceledException] {
          cm.getOptional[Int]("string")
        }
      assert(caught5.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "int", "aStringValue"))

      // Ensure Long works
      assert(cm.getOptional[Long]("long") === Some(Long.MaxValue))
      assert(cm.getOptional[Long]("Long") === Some(new java.lang.Long(Long.MaxValue)))
      val caught6 =
        intercept[TestCanceledException] {
          cm.getOptional[Long]("string")
        }
      assert(caught6.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "long", "aStringValue"))

      // Ensure Char works
      assert(cm.getOptional[Char]("char") === Some('c'))
      assert(cm.getOptional[Char]("Char") === Some(new java.lang.Character('c')))
// 510 287 1900
      val caught7 =
        intercept[TestCanceledException] {
          cm.getOptional[Char]("string")
        }
      assert(caught7.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "char", "aStringValue"))

      // Ensure Float works
      assert(cm.getOptional[Float]("float") === Some(1.0F))
      assert(cm.getOptional[Float]("Float") === Some(new java.lang.Float(1.0F)))
      val caught8 =
        intercept[TestCanceledException] {
          cm.getOptional[Float]("string")
        }
      assert(caught8.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "float", "aStringValue"))

      // Ensure Double works
      assert(cm.getOptional[Double]("double") === Some(1.0))
      assert(cm.getOptional[Double]("Double") === Some(new java.lang.Double(1.0)))
      val caught9 =
        intercept[TestCanceledException] {
          cm.getOptional[Double]("string")
        }
      assert(caught9.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "double", "aStringValue"))
    }

    def `provides a nice syntax for getting an optional entry with a default value` {
      assert(cm.getWithDefault[String]("string", "theDefault") === "aStringValue")
      assert(cm.getWithDefault[Int]("int", 0) === 1)
      assert(cm.getWithDefault[Int]("t", 0) === 0)
      assert(cm.getWithDefault[String]("t", "theDefault") === "theDefault")
    }

    def `throws a TestCanceledException if an optional entry requested with a default has an unexpected type` {

      // Ensure supertype and subype is done correctly
      assert(cm.getWithDefault[Apple]("apple", new Apple) === apple)
      assert(cm.getWithDefault[Fruit]("apple", new Fruit) === apple)
      val caught1 =
        intercept[TestCanceledException] {
          cm.getWithDefault[Apple]("fruit", new Apple)
        }
      assert(caught1.getMessage === Resources("configMapEntryHadUnexpectedType", "fruit", "class " + fruit.getClass.getName, "class " + apple.getClass.getName, "a Fruit"))

      // Ensure Boolean works
      assert(cm.getWithDefault[Boolean]("boolean", false) === true)
      assert(cm.getWithDefault[Boolean]("Boolean", false) === new java.lang.Boolean(true))
      val caught2 =
        intercept[TestCanceledException] {
          cm.getWithDefault[Boolean]("string", false)
        }

      assert(caught2.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "boolean", "aStringValue"))

      // Ensure Byte works
      assert(cm.getWithDefault[Byte]("byte", 2.toByte) === 1.toByte)
      assert(cm.getWithDefault[Byte]("Byte", 2.toByte) === new java.lang.Byte(1.toByte))
      val caught3 =
        intercept[TestCanceledException] {
          cm.getWithDefault[Byte]("string", 2.toByte)
        }

      assert(caught3.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "byte", "aStringValue"))

      // Ensure Short works
      assert(cm.getWithDefault[Short]("short", 2.toShort) === 1.toShort)
      assert(cm.getWithDefault[Short]("Short", 2.toShort) === new java.lang.Short(1.toShort))
      val caught4 =
        intercept[TestCanceledException] {
          cm.getWithDefault[Short]("string", 2.toShort)
        }
      assert(caught4.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "short", "aStringValue"))

      // Ensure Int works
      assert(cm.getWithDefault[Int]("int", 2) === 1)
      assert(cm.getWithDefault[Int]("Integer", 2) === new java.lang.Integer(1))
      val caught5 =
        intercept[TestCanceledException] {
          cm.getWithDefault[Int]("string", 2)
        }
      assert(caught5.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "int", "aStringValue"))

      // Ensure Long works
      assert(cm.getWithDefault[Long]("long", 2.toLong) === Long.MaxValue)
      assert(cm.getWithDefault[Long]("Long", 2.toLong) === new java.lang.Long(Long.MaxValue))
      val caught6 =
        intercept[TestCanceledException] {
          cm.getWithDefault[Long]("string", 2.toLong)
        }
      assert(caught6.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "long", "aStringValue"))

      // Ensure Char works
      assert(cm.getWithDefault[Char]("char", 'z') === 'c')
      assert(cm.getWithDefault[Char]("Char", 'z') === new java.lang.Character('c'))
// 510 287 1900
      val caught7 =
        intercept[TestCanceledException] {
          cm.getWithDefault[Char]("string", 'z')
        }
      assert(caught7.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "char", "aStringValue"))

      // Ensure Float works
      assert(cm.getWithDefault[Float]("float", 2.0F) === 1.0F)
      assert(cm.getWithDefault[Float]("Float", 2.0F) === new java.lang.Float(1.0F))
      val caught8 =
        intercept[TestCanceledException] {
          cm.getWithDefault[Float]("string", 2.0F)
        }
      assert(caught8.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "float", "aStringValue"))

      // Ensure Double works
      assert(cm.getWithDefault[Double]("double", 2.0) === 1.0)
      assert(cm.getWithDefault[Double]("Double", 2.0) === new java.lang.Double(1.0))
      val caught9 =
        intercept[TestCanceledException] {
          cm.getWithDefault[Double]("string", 2.0)
        }
      assert(caught9.getMessage === Resources("configMapEntryHadUnexpectedType", "string", "class java.lang.String", "double", "aStringValue"))
    }
  }

  object `The ConfigMap companion object` {
    def `should provide a factory method for constructing new ConfigMaps` {

      assert(cm.size === 19)

      val expected = new ConfigMap(
        Map(
          "string" -> "aStringValue",
          "boolean" -> true,
          "Boolean" -> new java.lang.Boolean(true),
          "byte" -> 1.toByte,
          "Byte" -> new java.lang.Byte(1.toByte),
          "short" -> 1.toShort,
          "Short" -> new java.lang.Short(1.toShort),
          "int" -> 1,
          "Integer" -> new java.lang.Integer(1),
          "long" -> Long.MaxValue,
          "Long" -> new java.lang.Long(Long.MaxValue),
          "char" -> 'c',
          "Char" -> new java.lang.Character('c'),
          "float" -> 1.0F,
          "Float" -> new java.lang.Float(1.0F),
          "double" -> 1.0,
          "Double" -> new java.lang.Double(1.0),
          "apple" -> apple,
          "fruit" -> fruit
        )
      )

      assert(cm === expected)
    }

    def `should provide a factory method for constructing empty ConfigMaps` {
      val emptyCm: ConfigMap = ConfigMap.empty
      assert(emptyCm.size === 0)
    }
  }
}

