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

class SafeSeqsSpec extends Spec with Matchers with SafeSeqs {

  abstract class Fruit
  case class Apple(name: String) extends Fruit
  case class Orange(name: String) extends Fruit
  val mac = Apple("Mcintosh")
  val navel = Orange("Navel")

  object `The sContains syntax should` {
    def `allow type checked containership tests` {
      def `on Array` {

        (Array(1, 2, 3) sContains 1) shouldBe true
        (Array(1, 2, 3) sContains 5) shouldBe false
        """Array(1, 2, 3) sContains "1"""" shouldNot typeCheck

        Array(mac, navel) sContains mac
      }
      def `on List` {

        (List(1, 2, 3) sContains 1) shouldBe true
        (List(1, 2, 3) sContains 5) shouldBe false
        """List(1, 2, 3) sContains "1"""" shouldNot typeCheck

        List(mac, navel) sContains mac
      }
      def `on Vector` {

        (Vector(1, 2, 3) sContains 1) shouldBe true
        (Vector(1, 2, 3) sContains 5) shouldBe false
        """Vector(1, 2, 3) sContains "1"""" shouldNot typeCheck

        Vector(mac, navel) sContains mac
      }
      def `on ListBuffer` {

        import scala.collection.mutable.ListBuffer

        (ListBuffer(1, 2, 3) sContains 1) shouldBe true
        (ListBuffer(1, 2, 3) sContains 5) shouldBe false
        """ListBuffer(1, 2, 3) sContains "1"""" shouldNot typeCheck

        ListBuffer(mac, navel) sContains mac
      }
    }
  }
}

