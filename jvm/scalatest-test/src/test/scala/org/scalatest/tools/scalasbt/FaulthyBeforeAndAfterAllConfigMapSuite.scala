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
package org.scalatest.tools.scalasbt

import org.scalatest._
import org.scalatest.funsuite.AnyFunSuite

class FaulthyBeforeAndAfterAllConfigMapSuite extends AnyFunSuite with BeforeAndAfterAllConfigMap {

  test("test 1") {}

  override protected def afterAll(configMap: ConfigMap): Unit = {
    throw new RuntimeException("oops!")
  }

}