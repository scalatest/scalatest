/*
 * Copyright 2001-2011 Artima, Inc.
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
package exceptions

import matchers.ShouldMatchers
import org.scalatest.prop._
/* Uncomment this after removing the deprecated type aliases in the org.scalatest.prop package object
import org.scalatest.exceptions.PropertyCheckFailedException
*/

class GeneratorDrivenPropertyCheckFailedExceptionSuite extends FunSuite with ShouldMatchers {

  test("PropertyCheckFailedException's argNames method should return argN if no argument names are passed to the constructor") {

    val e =
      new GeneratorDrivenPropertyCheckFailedException(
        sde => "msg",
        None,
        sde => 7,
        None,
        "msg",
        List(1, 2, 3),
        None,
        List.empty
      )

    e.argNames should be (List("arg0", "arg1", "arg2"))
  }

  test("PropertyCheckFailedException's argNames method should return the passed argument names if supplied to the constructor") {

    val e =
      new GeneratorDrivenPropertyCheckFailedException(
        sde => "msg",
        None,
        sde => 7,
        None,
        "msg",
        List(1, 2, 3),
        Some(List("a", "b", "c")),
        List.empty
      )

    e.argNames should be (List("a", "b", "c"))
  }
}
