/*
 * Copyright 2001-2025 Artima, Inc.
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

 import org.scalatest.funspec._

 class AssertionsCanEqualSpec extends AnyFunSpec {

   describe("assertResult without clue") {

     it("should allow 2 unrelated types when strictEquality is not enabled") {
       assertCompiles(
         """
         case class Apple(size: Int)
         case class Orange(size: Int)
         val x = Apple(42)
         val y = Orange(42)
         assertResult(x)(y)
         """  
       )
     }

     it("should now allow 2 unrelated types when strictEquality is enabled") {
       assertDoesNotCompile(
         """
         import scala.language.strictEquality
         case class Apple(size: Int)
         case class Orange(size: Int)
         val x = Apple(42)
         val y = Orange(42)
         assertResult(x)(y)
         """  
       )
     }

   }

   describe("org.scalatest.Assertions.assertResult without clue") {

     it("should allow 2 unrelated types when strictEquality is not enabled") {
       assertCompiles(
         """
         case class Apple(size: Int)
         case class Orange(size: Int)
         val x = Apple(42)
         val y = Orange(42)
         org.scalatest.Assertions.assertResult(x)(y)
         """  
       )
     }

     it("should now allow 2 unrelated types when strictEquality is enabled") {
       assertDoesNotCompile(
         """
         import scala.language.strictEquality
         case class Apple(size: Int)
         case class Orange(size: Int)
         val x = Apple(42)
         val y = Orange(42)
         org.scalatest.Assertions.assertResult(x)(y)
         """  
       )
     }

   }

   describe("assertResult with clue") {

     it("should allow 2 unrelated types when strictEquality is not enabled") {
       assertCompiles(
         """
         case class Apple(size: Int)
         case class Orange(size: Int)
         val x = Apple(42)
         val y = Orange(42)
         assertResult(x, "test clue")(y)
         """  
       )
     }

     it("should now allow 2 unrelated types when strictEquality is enabled") {
       assertDoesNotCompile(
         """
         import scala.language.strictEquality
         case class Apple(size: Int)
         case class Orange(size: Int)
         val x = Apple(42)
         val y = Orange(42)
         assertResult(x, "test clue")(y)
         """  
       )
     }

   }

   describe("org.scalatest.Assertions.assertResult with clue") {

     it("should allow 2 unrelated types when strictEquality is not enabled") {
       assertCompiles(
         """
         case class Apple(size: Int)
         case class Orange(size: Int)
         val x = Apple(42)
         val y = Orange(42)
         org.scalatest.Assertions.assertResult(x, "test clue")(y)
         """  
       )
     }

     it("should now allow 2 unrelated types when strictEquality is enabled") {
       assertDoesNotCompile(
         """
         import scala.language.strictEquality
         case class Apple(size: Int)
         case class Orange(size: Int)
         val x = Apple(42)
         val y = Orange(42)
         org.scalatest.Assertions.assertResult(x, "test clue")(y)
         """  
       )
     }

   }

 }