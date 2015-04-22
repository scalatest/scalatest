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

class AsMethodsSpec extends FunSpec with Matchers with AsMethods {

  describe("trait AsMethods") {
    it("should provide a .as method that will convert any type A to the given type B when an implicit conversion from A => B exists") { 
      Option(3).map(_.as[Complex]) shouldEqual Option(Complex(3, 0))
    }
  }
}

