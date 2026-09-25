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
package org.scalactic

import org.scalatest._

class NameUtilSpec extends funspec.AnyFunSpec with NonImplicitAssertions {

  describe("NameUtil.stripDollars") {

    it("should return the string unchanged when it has no dollar sign") {
      assert(NameUtil.stripDollars("simpleName") == "simpleName")
    }

    it("should return the string unchanged when it has a dollar sign but no $iw$") {
      assert(NameUtil.stripDollars("foo$bar") == "foo$bar")
    }

    it("should strip everything up to and including the last dollar sign when it contains $iw$") {
      assert(NameUtil.stripDollars("line1$iw$Foo") == "Foo")
    }

    it("should strip trailing dollar signs") {
      assert(NameUtil.stripDollars("Foo$$") == "Foo")
    }

    it("should return the string unchanged when it consists entirely of dollar signs") {
      assert(NameUtil.stripDollars("$$$") == "$$$")
    }
  }

  describe("NameUtil.parseSimpleName") {

    it("should return the simple name from a fully qualified name") {
      assert(NameUtil.parseSimpleName("com.example.Foo") == "Foo")
    }

    it("should return the string unchanged when it has no dot") {
      assert(NameUtil.parseSimpleName("Foo") == "Foo")
    }

    it("should handle a dot at the start") {
      assert(NameUtil.parseSimpleName(".Foo") == "Foo")
    }
  }

  describe("NameUtil.getSimpleNameOfAnObjectsClass") {

    it("should return the simple name of an object's class") {
      assert(NameUtil.getSimpleNameOfAnObjectsClass(new Object) == "Object")
    }
  }
}
