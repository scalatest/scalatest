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
package org.scalatest.prop

import org.scalactic.anyvals._
import org.scalatest._

class ConfigurationSpec extends FunSpec {

  describe("Configuration.Parameter") {

    /*val configurationGenerator =
      new Generator[Configuration.Parameter] {

          def initEdges(maxLength: PosZInt, rnd: Randomizer): (List[Configuration.Parameter], Randomizer) = (Nil, rnd)

          def next(szp: SizeParam, edges: List[Configuration.Parameter], rnd: Randomizer): (Configuration.Parameter, List[Configuration.Parameter], Randomizer) = {
            for (
              minSuccessful <-    : PosInt = PosInt(10),
            maxDiscardedFactor: PosZDouble = PosZDouble(5.0),
            minSize: PosZInt = PosZInt(0),
            sizeRange: PosZInt = PosZInt(100),
            workers: PosInt = PosInt(1)
            )
          }
      }*/

    it("should offer a maxSize method that is minSize + SizeRange") {

    }

  }

}