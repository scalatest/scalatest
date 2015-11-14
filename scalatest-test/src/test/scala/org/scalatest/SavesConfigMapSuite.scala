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

import SavesConfigMapSuite.theConfigMap
import java.util.UUID

@WrapWith(classOf[ConfigMapWrapperSuite])
class SavesConfigMapSuite(configMap: ConfigMap) extends FunSuite {
  theConfigMap = Some(configMap)
  test("one test") {}
  test("two test") {}
  test("red test") {}
  test("blue test", org.scalatest.mytags.FastAsLight) {}
  ignore("ignore me") {}
  class NSuite extends Suite {
    override val suiteId = getClass.getName + "-" + UUID.randomUUID.toString
  }
  override def nestedSuites: Vector[Suite] = Vector(new NSuite, new NSuite, new NSuite)
}

object SavesConfigMapSuite {
  private var theConfigMap: Option[Map[String, Any]] = None
  def savedConfigMap: Option[Map[String,Any]] = theConfigMap
  def resetConfigMap() { theConfigMap = None }
}
