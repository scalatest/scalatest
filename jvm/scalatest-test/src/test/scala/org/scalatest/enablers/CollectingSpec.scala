/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest.enablers

import org.scalatest._
import org.scalactic.Equality
import scala.collection.immutable
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CollectingSpec extends AnyFunSpec with Matchers {

  describe("The implicit Containing providers") {
    it("should provide a working iterableFrom method") {

      Collecting.collectingNatureOfIterable[Int, List].iterableFrom(List(1, 2, 3)) shouldEqual List(1, 2, 3)

      Collecting.collectingNatureOfArray[Int].iterableFrom(Array(1, 2, 3)) shouldEqual List(1, 2, 3)

      // SKIP-SCALATESTJS,NATIVE-START
      import collection.JavaConverters._
      val jList: java.util.List[Int] = List(1, 2, 3).asJava
      Collecting.collectingNatureOfJavaCollection[Int, java.util.List].iterableFrom(jList) shouldEqual List(1, 2, 3)
      val jSet: java.util.Set[Int] = Set(1, 2, 3).asJava
      Collecting.collectingNatureOfJavaCollection[Int, java.util.Set].iterableFrom(jSet) shouldEqual Set(1, 3, 2)

      val jMap: java.util.Map[String, Int] = Map("one" -> 1, "two" -> 2, "three" -> 3).asJava
      Collecting.collectingNatureOfJavaMap[String, Int, java.util.Map].iterableFrom(jMap) shouldEqual
        List(Entry("one", 1), Entry("two", 2), Entry("three", 3))
      // SKIP-SCALATESTJS,NATIVE-END
    }
  }
}

