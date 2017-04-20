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
package org.scalatest.tools

import org.scalatest.Args
import org.scalatest.FunSpec
import org.scalatest.Reporter
import org.scalatest.Suite
import org.scalatest.WrapWith
import org.scalatest.events.Event
import org.scalatest.exceptions.NotAllowedException

class AnnotationHelperSpec extends FunSpec {

  class MarkerAnnotatedAtClass extends Suite

  class MarkerAnnotatedAtSuper extends Suite

  class Marker1AnnotatedAtTrait extends Suite

  class Marker2AnnotatedAtTrait extends Suite

  class DefaultSuperClass

  @WrapWith(classOf[MarkerAnnotatedAtSuper])
  class AnnotatedSuperClass

  trait DefaultTrait

  @WrapWith(classOf[Marker1AnnotatedAtTrait])
  trait AnnotatedTrait1

  @WrapWith(classOf[Marker2AnnotatedAtTrait])
  trait AnnotatedTrait2a

  @WrapWith(classOf[Marker2AnnotatedAtTrait])
  trait AnnotatedTrait2b

  trait IndirectAnnotatedTrait extends AnnotatedTrait1

  describe("distinctEq") {
    it("empty = empty") {
      assert(AnnotationHelper.distinctEq(Seq()) == Seq())
    }
    it("a = a") {
      val a = new DefaultSuperClass
      assert(AnnotationHelper.distinctEq(Seq(a)) == Seq(a))
    }
    it("aa = a") {
      val a = new DefaultSuperClass
      assert(AnnotationHelper.distinctEq(Seq(a, a)) == Seq(a))
    }
    it("ab = ab") {
      val a = new DefaultSuperClass
      val b = new DefaultSuperClass
      assert(AnnotationHelper.distinctEq(Seq(a, b)) == Seq(a, b))
    }
    it("aab = ab") {
      val a = new DefaultSuperClass
      val b = new DefaultSuperClass
      assert(AnnotationHelper.distinctEq(Seq(a, a, b)) == Seq(a, b))
    }
    it("abb = ab") {
      val a = new DefaultSuperClass
      val b = new DefaultSuperClass
      assert(AnnotationHelper.distinctEq(Seq(a, b, b)) == Seq(a, b))
    }
    it("aabb = ab") {
      val a = new DefaultSuperClass
      val b = new DefaultSuperClass
      assert(AnnotationHelper.distinctEq(Seq(a, a, b, b)) == Seq(a, b))
    }
    it("abcaba = acb") {
      val a = new DefaultSuperClass
      val b = new DefaultSuperClass
      val c = new DefaultSuperClass
      assert(AnnotationHelper.distinctEq(Seq(a, b, c, a, b, a)) == Seq(a, b, c))
    }
  }

  describe("TestRunner") {
    @WrapWith(classOf[MarkerAnnotatedAtClass])
    class Subject extends FunSpec with AnnotatedTrait1 {
      describe("describe") {
        it("it") {
          assert(true === true)
        }
      }
    }

    it("run should throw NotAllowedException when more then one @WrapWith Annotation was found") {
      val subject = new Subject()
      intercept[NotAllowedException] {
        subject.run(None, Args(new Reporter() {
          def apply(event: Event): Unit = {
          }
        }))
      }
    }
  }

  describe("DefaultSuperClass") {
    class Subject extends DefaultSuperClass

    val result = AnnotationHelper.findAll(classOf[WrapWith], classOf[Subject])

    it("findAll should detect annotations correctly") {
      assert(result.isEmpty)
    }
  }

  describe("DefaultSuperClass with DefaultTrait") {
    class Subject extends DefaultSuperClass with DefaultTrait

    val result = AnnotationHelper.findAll(classOf[WrapWith], classOf[Subject])

    it("findAll should detect annotations correctly") {
      assert(result.isEmpty)
    }
  }

  describe("DefaultSuperClass with AnnotatedTrait1") {
    class Subject extends DefaultSuperClass with AnnotatedTrait1

    val result = AnnotationHelper.findAll(classOf[WrapWith], classOf[Subject])

    it("findAll should detect annotations correctly") {
      assert(result.length == 1)
      assert(result.head.value === classOf[Marker1AnnotatedAtTrait])
    }
  }

  describe("DefaultSuperClass with IndirectAnnotatedTrait") {
    class Subject extends DefaultSuperClass with IndirectAnnotatedTrait

    val result = AnnotationHelper.findAll(classOf[WrapWith], classOf[Subject])

    it("findAll should detect annotations correctly") {
      assert(result.length == 1)
      assert(result.head.value === classOf[Marker1AnnotatedAtTrait])
    }
  }

  describe("DefaultSuperClass with AnnotatedTrait1 with IndirectAnnotatedTrait") {
    class Subject extends DefaultSuperClass with AnnotatedTrait1 with IndirectAnnotatedTrait

    val result = AnnotationHelper.findAll(classOf[WrapWith], classOf[Subject])

    it("findAll should detect annotations correctly") {
      assert(result.length == 1)
      assert(result.head.value === classOf[Marker1AnnotatedAtTrait])
    }
  }

  describe("AnnotatedSuperClass") {
    class Subject extends AnnotatedSuperClass

    val result = AnnotationHelper.findAll(classOf[WrapWith], classOf[Subject])

    it("findAll should detect annotations correctly") {
      assert(result.length == 1)
      assert(result.head.value === classOf[MarkerAnnotatedAtSuper])
    }
  }

  describe("AnnotatedSuperClass with AnnotatedTrait1") {
    class Subject extends AnnotatedSuperClass with AnnotatedTrait1

    val result = AnnotationHelper.findAll(classOf[WrapWith], classOf[Subject])

    it("findAll should detect annotations correctly") {
      assert(result.length == 2)
      assert(result.head.value === classOf[MarkerAnnotatedAtSuper])
      assert(result.tail.head.value === classOf[Marker1AnnotatedAtTrait])
    }
  }

  describe("AnnotatedSuperClass with DefaultTrait") {
    class Subject extends AnnotatedSuperClass with DefaultTrait

    val result = AnnotationHelper.findAll(classOf[WrapWith], classOf[Subject])

    it("findAll should detect annotations correctly") {
      assert(result.length == 1)
      assert(result.head.value === classOf[MarkerAnnotatedAtSuper])
    }
  }

  describe("AnnotatedSuperClass with DefaultTrait with Annotation") {
    @WrapWith(classOf[MarkerAnnotatedAtClass])
    class Subject extends AnnotatedSuperClass with DefaultTrait

    val result = AnnotationHelper.findAll(classOf[WrapWith], classOf[Subject])

    it("findAll should detect annotations correctly") {
      assert(result.length == 2)
      assert(result.head.value === classOf[MarkerAnnotatedAtClass])
      assert(result.tail.head.value === classOf[MarkerAnnotatedAtSuper])
    }
  }

  describe("AnnotatedSuperClass with AnnotatedTrait1 with AnnotatedTrait2a") {
    class Subject extends AnnotatedSuperClass with AnnotatedTrait1 with AnnotatedTrait2a

    val result = AnnotationHelper.findAll(classOf[WrapWith], classOf[Subject])

    it("findAll should detect annotations correctly") {
      assert(result.length == 3)
      assert(result.head.value === classOf[MarkerAnnotatedAtSuper])
      assert(result.tail.head.value === classOf[Marker1AnnotatedAtTrait])
      assert(result.tail.tail.head.value === classOf[Marker2AnnotatedAtTrait])
    }
  }

  describe("AnnotatedSuperClass with AnnotatedTrait1 with AnnotatedTrait2a with AnnotatedTrait2b") {
    class Subject extends AnnotatedSuperClass with AnnotatedTrait1 with AnnotatedTrait2a with AnnotatedTrait2b

    val result = AnnotationHelper.findAll(classOf[WrapWith], classOf[Subject])

    it("findAll should detect annotations correctly") {
      assert(result.length == 4)
      assert(result.head.value === classOf[MarkerAnnotatedAtSuper])
      assert(result.tail.head.value === classOf[Marker1AnnotatedAtTrait])
      assert(result.tail.tail.head.value === classOf[Marker2AnnotatedAtTrait])
      assert(result.tail.tail.tail.head.value === classOf[Marker2AnnotatedAtTrait])
    }
  }

  describe("AnnotatedSuperClass with MarkerAnnotatedAtSuper on class") {
    @WrapWith(classOf[MarkerAnnotatedAtSuper])
    class Subject extends AnnotatedSuperClass

    val result = AnnotationHelper.findAll(classOf[WrapWith], classOf[Subject])

    it("findAll should detect annotations correctly") {
      assert(result.length == 2)
      assert(result.head.value === classOf[MarkerAnnotatedAtSuper])
      assert(result.tail.head.value === classOf[MarkerAnnotatedAtSuper])
    }
  }
}
