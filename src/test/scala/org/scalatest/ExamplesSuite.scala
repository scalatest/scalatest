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

/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.DuplicateTestNameException
*/

class ExamplesSuite extends FunSuite {

  test("that duplicate specTexts result in a thrown exception at construction time") {

    class MySpec extends FunSpec {

      def myOtherExamples() {
        it("should lead the whole game") {}
        it("should lead just part of the game") {}
      }

      myOtherExamples()

      def myExamples() {
        it("should lead the whole game") {}
        it("should lead the whole game") {}
      }

      intercept[DuplicateTestNameException] {
        myExamples()
      }
    }

    new MySpec
  }

  test("duplicate testNames should result in an exception when one is in the Examples and the other in the Spec") {
    class MySpec extends FunSpec {
      def myOtherExamples() {
        it("should lead the whole game") {}
        it("should lead just part of the game") {}
      }
      myOtherExamples()
      it("should lead the whole game") {}
    }
    intercept[DuplicateTestNameException] {
      new MySpec  
    }
    class MyOtherSpec extends FunSpec {
      def myOtherExamples() {
        it("should lead the whole game") {}
        it("should lead just part of the game") {}
      }
      it("should lead the whole game") {}
      myOtherExamples()
    }
    intercept[DuplicateTestNameException] {
      new MyOtherSpec  
    }
  }

  test("that a null specText results in a thrown NPE at construction time") {

    class MySpec extends FunSpec {

      def examples() {
        it(null) {}
      }
      intercept[NullPointerException] {
        examples()
      }
    }
    new MySpec
  }

  test("tags work correctly in Examples") {

    val a = new FunSpec {
      def aExamples() {
        it("test this", mytags.SlowAsMolasses) {}
        ignore("test that", mytags.SlowAsMolasses) {}
      }
      aExamples()
    }
    assertResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses"), "test that" -> Set("org.scalatest.Ignore", "org.scalatest.SlowAsMolasses"))) {
      a.tags
    }

    val b = new FunSpec {
      def bExamples() {}
      bExamples()
    }
    assertResult(Map()) {
      b.tags
    }

    val c = new FunSpec {
      def cExamples() {
        it("test this", mytags.SlowAsMolasses, mytags.WeakAsAKitten) {}
        it("test that", mytags.SlowAsMolasses) {}
      }
      cExamples()
    }
    assertResult(Map("test this" -> Set("org.scalatest.SlowAsMolasses", "org.scalatest.WeakAsAKitten"), "test that" -> Set("org.scalatest.SlowAsMolasses"))) {
      c.tags
    }
  }
}
