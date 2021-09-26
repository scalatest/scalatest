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
package org.scalatest

import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenSet
import scala.collection.GenIterable
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce
import org.scalactic.TripleEquals
import org.scalactic.TypeCheckedTripleEquals
import Matchers._
import exceptions.TestFailedException

class ShouldTripleEqualsSpec extends FunSpec with NonImplicitAssertions {

  case class Super(size: Int)
  class Sub(sz: Int) extends Super(sz)

  val super1: Super = new Super(1)
  val sub1: Sub = new Sub(1)
  val super2: Super = new Super(2)
  val sub2: Sub = new Sub(2)
  val nullSuper: Super = null

  // SKIP-SCALATESTJS,NATIVE-START
  val javaList: java.util.List[Int] = new java.util.ArrayList
  javaList.add(1)
  javaList.add(2)
  val javaArrayList: java.util.ArrayList[Int] = new java.util.ArrayList
  javaArrayList.add(1)
  javaArrayList.add(2)

  val javaMap: java.util.Map[String, Int] = new java.util.HashMap
  javaMap.put("one",1)
  javaMap.put("two", 2)
  val javaHashMap: java.util.HashMap[String, Int] = new java.util.HashMap
  javaHashMap.put("one",1)
  javaHashMap.put("two", 2)

  val javaSet: java.util.Set[Int] = new java.util.HashSet
  javaSet.add(1)
  javaSet.add(2)
  val javaHashSet: java.util.HashSet[Int] = new java.util.HashSet
  javaHashSet.add(1)
  javaHashSet.add(2)
  // SKIP-SCALATESTJS,NATIVE-END

  describe("the custom equality should === (operator") {

    describe("with TripleEquals") {

      it("should compare anything with anything") {

        new TripleEquals {

          1 should === (1)
          intercept[TestFailedException] { 1 should !== (1) }

          1 should === (1L)
          intercept[TestFailedException] { 1 should !== (1L) }

          1L should === (1)
          intercept[TestFailedException] { 1L should !== (1) }

          "1" should !== (1)
          intercept[TestFailedException] { "1" should === (1) }

          1 should !== ("1")
          intercept[TestFailedException] { 1 should === ("1") }

          super1 should !== (super2)
          super1 should !== (sub2)
          sub2 should !== (super1)
          super1 should === (super1)
          super1 should === (sub1)
          sub1 should === (super1)

          val caught1 = intercept[TestFailedException] { super1 should === (null) }
          caught1.getMessage should be ("Super(1) did not equal null")
          super1 should !== (null)

          nullSuper should === (null)
          val caught2 = intercept[TestFailedException] { nullSuper should !== (null) }
          caught2.getMessage should be ("null equaled null")
          val caught3 = intercept[TestFailedException] { nullSuper should === (super1) }
          caught3.getMessage should be ("null did not equal Super(1)")
          nullSuper should !== (super1)

          Map("I" -> 1, "II" -> 2) should === (Map("I" -> 1, "II" -> 2))
          Map("I" -> 1, "II" -> 2) should !== (Map("1" -> 1, "2" -> 2))

          intercept[TestFailedException] { Map("I" -> 1, "II" -> 2) should === (7) }
          Map("I" -> 1, "II" -> 2) should !== (7)

          Set(1, 2, 3) should === (Set(1, 2, 3))
          Set(1, 2, 3) should !== (Set(2, 3, 4))

          intercept[TestFailedException] { Set(1, 2, 3) should === (7) }
          Set(1, 2, 3) should !== (7)

          List(1, 2, 3) should === (List(1, 2, 3))
          List(1, 2, 3) should !== (List(2, 3, 4))

          Array(1, 2, 3) should === (Array(1, 2, 3))
          Array(1, 2, 3) should !== (Array(2, 3, 4))

          Seq(1, 2, 3) should === (Array(1, 2, 3))
          Seq(1, 2, 3) should !== (Array(2, 3, 4))

          Array(1, 2, 3) should === (Seq(1, 2, 3))
          Array(1, 2, 3) should !== (Seq(2, 3, 4))

          // SKIP-SCALATESTJS,NATIVE-START
          javaList should === (javaArrayList)
          intercept[TestFailedException] { javaList should !== (javaArrayList) }

          javaArrayList should === (javaList)
          intercept[TestFailedException] { javaArrayList should !== (javaList) }

          intercept[TestFailedException] { javaList should === (7) }
          javaList should !== (7)

          javaMap should === (javaHashMap)
          intercept[TestFailedException] { javaMap should !== (javaHashMap) }

          javaHashMap should === (javaMap)
          intercept[TestFailedException] { javaHashMap should !== (javaMap) }

          intercept[TestFailedException] { javaMap should === (7) }
          javaMap should !== (7)

          javaSet should === (javaHashSet)
          intercept[TestFailedException] { javaSet should !== (javaHashSet) }

          javaHashSet should === (javaSet)
          intercept[TestFailedException] { javaHashSet should !== (javaSet) }

          intercept[TestFailedException] { javaSet should === (7) }
          javaSet should !== (7)
          // SKIP-SCALATESTJS,NATIVE-END

          () should === (())
          () should !== (7)
        }
      }

      it("should be overridable with TypeCheckedTripleEquals locally when TripleEquals imported") {

        object O extends TripleEquals
        
        new TypeCheckedTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          1 should === (1)
          intercept[TestFailedException] { 1 should !== (1) }

          ap should === (fr)
          fr should === (ap)
          ap should === (cr)
          cr should === (ap)

          super1 should !== (super2)
          super1 should !== (sub2)
          sub2 should !== (super1)
          super1 should === (super1)
          super1 should === (sub1)
          sub1 should === (super1)

          // The rest should not compile
          // 1 should === (1L)
          // 1L should === (1)
          // 1 should !== (1L)
          // 1L should !== (1)

          // "1" should === (1)
          // 1 should === ("1")
          // "1" should !== (1)
          // 1 should !== ("1")

          // fr should === (cr)
          // cr should === (fr)
        }
      }

      it("should be overridable with TypeCheckedTripleEquals locally when TripleEquals mixed in") {

        object O extends TripleEquals {

          new TypeCheckedTripleEquals {

            class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
            trait Crunchy
            class Apple extends Fruit with Crunchy
  
            val fr: Fruit = new Apple
            val cr: Crunchy = new Apple
            val ap: Apple = new Apple
  
            1 should === (1)
            intercept[TestFailedException] { 1 should !== (1) }
  
            ap should === (fr)
            fr should === (ap)
            ap should === (cr)
            cr should === (ap)

            super1 should !== (super2)
            super1 should !== (sub2)
            sub2 should !== (super1)
            super1 should === (super1)
            super1 should === (sub1)
            sub1 should === (super1)
  
            // The rest should not compile
            // 1 should === (1L)
            // 1L should === (1)
            // 1 should !== (1L)
            // 1L should !== (1)
  
            // "1" should === (1)
            // 1 should === ("1")
            // "1" should !== (1)
            // 1 should !== ("1")
  
            // fr should === (cr)
            // cr should === (fr)
          }
        }
      }
    }

    describe("with TypeCheckedTripleEquals") {

      it("should compare supertypes with subtypes on either side") {

        new TypeCheckedTripleEquals {

          class Fruit { override def equals(o: Any) = o.isInstanceOf[Fruit] }
          trait Crunchy
          class Apple extends Fruit with Crunchy

          val fr: Fruit = new Apple
          val cr: Crunchy = new Apple
          val ap: Apple = new Apple

          1 should === (1)
          intercept[TestFailedException] { 1 should !== (1) }

          ap should === (fr)
          fr should === (ap)
          ap should === (cr)
          cr should === (ap)

          super1 should !== (super2)
          super1 should !== (sub2)
          sub2 should !== (super1)
          super1 should === (super1)
          super1 should === (sub1)
          sub1 should === (super1)

          val caught1 = intercept[TestFailedException] { super1 should === (null) }
          caught1.getMessage should be ("Super(1) did not equal null")
          super1 should !== (null)

          nullSuper should === (null)
          val caught2 = intercept[TestFailedException] { nullSuper should !== (null) }
          caught2.getMessage should be ("null equaled null")
          val caught3 = intercept[TestFailedException] { nullSuper should === (super1) }
          caught3.getMessage should be ("null did not equal Super(1)")
          nullSuper should !== (super1)

          Map("I" -> 1, "II" -> 2) should === (Map("I" -> 1, "II" -> 2))
          Map("I" -> 1, "II" -> 2) should !== (Map("1" -> 1, "2" -> 2))

          Set(1, 2, 3) should === (Set(1, 2, 3))
          Set(1, 2, 3) should !== (Set(2, 3, 4))

          List(1, 2, 3) should === (List(1, 2, 3))
          List(1, 2, 3) should !== (List(2, 3, 4))

          Array(1, 2, 3) should === (Array(1, 2, 3))
          Array(1, 2, 3) should !== (Array(2, 3, 4))

          // SKIP-SCALATESTJS,NATIVE-START
          javaList should === (javaArrayList)
          intercept[TestFailedException] { javaList should !== (javaArrayList) }

          javaArrayList should === (javaList)
          intercept[TestFailedException] { javaArrayList should !== (javaList) }

          javaMap should === (javaHashMap)
          intercept[TestFailedException] { javaMap should !== (javaHashMap) }

          javaHashMap should === (javaMap)
          intercept[TestFailedException] { javaHashMap should !== (javaMap) }

          javaSet should === (javaHashSet)
          intercept[TestFailedException] { javaSet should !== (javaHashSet) }

          javaHashSet should === (javaSet)
          intercept[TestFailedException] { javaHashSet should !== (javaSet) }
          // SKIP-SCALATESTJS,NATIVE-END

          () should === (())

          // The rest should not compile
          // () should !== (7)

          // 1 should === (1L)
          // 1L should === (1)
          // 1 should !== (1L)
          // 1L should !== (1)

          // "1" should === (1)
          // 1 should === ("1")
          // "1" should !== (1)
          // 1 should !== ("1")

          // fr should === (cr)
          // cr should === (fr)

          // Array(1, 2, 3) should === (Seq(1, 2, 3))
          // Array(1, 2, 3) should !== (Seq(2, 3, 4))

          // Seq(1, 2, 3) should === (Array(1, 2, 3))
          // Seq(1, 2, 3) should !== (Array(2, 3, 4))

          // intercept[TestFailedException] { Map("I" -> 1, "II" -> 2) should === (7) }
          // Map("I" -> 1, "II" -> 2) should !== (7)

          // intercept[TestFailedException] { Set(1, 2, 3) should === (7) }
          // Set(1, 2, 3) should !== (7)

          // intercept[TestFailedException] { javaList should === (7) }
          // javaList should !== (7)

          // intercept[TestFailedException] { javaMap should === (7) }
          // javaMap should !== (7)

          // intercept[TestFailedException] { javaSet should === (7) }
          // javaSet should !== (7)
        }
      }

      it("should be overridable with TripleEquals locally when TypeCheckedTripleEquals imported") {

        object O extends TypeCheckedTripleEquals
        
        new TripleEquals {

          1 should === (1)
          intercept[TestFailedException] { 1 should !== (1) }

          1 should === (1L)
          intercept[TestFailedException] { 1 should !== (1L) }

          1L should === (1)
          intercept[TestFailedException] { 1L should !== (1) }

          "1" should !== (1)
          intercept[TestFailedException] { "1" should === (1) }

          1 should !== ("1")
          intercept[TestFailedException] { 1 should === ("1") }

          super1 should !== (super2)
          super1 should !== (sub2)
          // sub2 should !== (super1) // compiles on 2.10 but not 2.9
          super1 should === (super1)
          super1 should === (sub1)
          // sub1 should === (super1) // compiles on 2.10 but not 2.9
        }
      }

      it("should be overridable with TripleEquals locally when TypeCheckedTripleEquals mixed in") {

        object O extends TypeCheckedTripleEquals {

          new TripleEquals {

            1 should === (1)
            intercept[TestFailedException] { 1 should !== (1) }

            1 should === (1L)
            intercept[TestFailedException] { 1 should !== (1L) }

            1L should === (1)
            intercept[TestFailedException] { 1L should !== (1) }

            "1" should !== (1)
            intercept[TestFailedException] { "1" should === (1) }

            1 should !== ("1")
            intercept[TestFailedException] { 1 should === ("1") }

            super1 should !== (super2)
            super1 should !== (sub2)
            // sub2 should !== (super1) // compiles on 2.10 but not 2.9
            super1 should === (super1)
            super1 should === (sub1)
            // sub1 should === (super1) // compiles on 2.10 but not 2.9
          }
        }
      }
    }
  }
}

