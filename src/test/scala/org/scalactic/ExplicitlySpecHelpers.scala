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

trait ExplicitlySpecHelpers {

  val intInequality = 
    new Equality[Int] {
      def areEqual(a: Int, b: Any): Boolean = a != b
    }

  val intInequivalence = 
    new Equivalence[Int] {
      def areEquivalent(a: Int, b: Int): Boolean = a != b
    }

  case class Fruit(name: String)
  class Apple extends Fruit("apple")
  class Orange extends Fruit("orange")

  val fruitInequality = 
    new Equality[Fruit] {
      def areEqual(a: Fruit, b: Any): Boolean = a != b
    }

  val fruitInequivalence = 
    new Equivalence[Fruit] {
      def areEquivalent(a: Fruit, b: Fruit): Boolean = a != b
    }

  class Pomme

  import scala.language.implicitConversions
  implicit def convertPommeToFruit(pomme: Pomme): Fruit = new Fruit("apple")

  val downCased: Normalization[String] =
    new Normalization[String] {
      def normalized(s: String): String = s.toLowerCase
    }

  val chopped: Normalization[String] =
    new Normalization[String] {
      def normalized(s: String): String = s.trim
    }

  val stringInequality = 
    new Equality[String] {
      def areEqual(a: String, b: Any): Boolean = a != b
    }

  val stringInequivalence = 
    new Equivalence[String] {
      def areEquivalent(a: String, b: String): Boolean = a != b
    }
}
