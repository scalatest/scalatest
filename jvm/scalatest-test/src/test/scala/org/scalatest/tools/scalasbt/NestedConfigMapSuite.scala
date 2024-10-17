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
import org.scalatest.wordspec.AnyWordSpec

trait NestedConfigMapSuiteConfigMap extends SuiteMixin { this: Suite =>

  abstract override def run(testName: Option[String], args: Args): Status = {
    val newConfigMap = args.configMap + ("app" -> "an app instance")
    val newArgs = args.copy(configMap = newConfigMap)
    super.run(testName, newArgs)
  }
}

class NestedConfigMapSuite extends AnyWordSpec with NestedConfigMapSuiteConfigMap {

  class NestedSuite extends AnyWordSpec {

    var configMap: ConfigMap = _

    override def withFixture(test: NoArgTest): Outcome = {
      configMap = test.configMap
      super.withFixture(test)
    }

    "ConfiguredApp trait" should {

      "do something" in {
        assert(configMap.getOptional[String]("app") == Some("an app instance"))
      }

    }
  }

  override def nestedSuites = Vector(new NestedSuite)

}