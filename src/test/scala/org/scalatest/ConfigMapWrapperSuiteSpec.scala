/*
 * Copyright 2001-2008 Artima, Inc.
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

class ConfigMapWrapperSuiteSpec extends FunSuite with SharedHelpers with SeveredStackTraces {

  // Need a test that ensures the passed config map gets in there.
  test("configMap should get passed into the wrapped Suite") {
    SavesConfigMapSuite.resetConfigMap()
    val wrapped = new ConfigMapWrapperSuite(classOf[SavesConfigMapSuite])
    val configMap = ConfigMap("salt" -> "pepper", "eggs" -> "bacon")
    wrapped.run(None, Args(SilentReporter, Stopper.default, Filter(), configMap, None, new Tracker, Set.empty))
    assert(SavesConfigMapSuite.savedConfigMap === Some(configMap))
  }
  
  test("configMap's expected test count should return the underlying's value") {
    val clazz = getClass.getClassLoader.loadClass("org.scalatest.SavesConfigMapSuite").asInstanceOf[Class[_ <: Suite]]
    val suite = new ConfigMapWrapperSuite(clazz)
    assert(suite.expectedTestCount(Filter(None, Set())) === 4)
    assert(suite.expectedTestCount(Filter(None, Set("org.scalatest.FastAsLight"))) === 3)
  }

  test("configMap's testNames should return the underlying's value") {
    val clazz = getClass.getClassLoader.loadClass("org.scalatest.SavesConfigMapSuite").asInstanceOf[Class[_ <: Suite]]
    val suite = new ConfigMapWrapperSuite(clazz)
    assert(suite.testNames.toList === List("one test", "two test", "red test", "blue test", "ignore me"))
  }

  test("configMap's nestedSuites should return the underlying's value") {
    val clazz = getClass.getClassLoader.loadClass("org.scalatest.SavesConfigMapSuite").asInstanceOf[Class[_ <: Suite]]
    val suite = new ConfigMapWrapperSuite(clazz)
    assert(suite.nestedSuites.size === 3)
  }

  test("configMap's tags method should return the underlying's value") {
    val clazz = getClass.getClassLoader.loadClass("org.scalatest.SavesConfigMapSuite").asInstanceOf[Class[_ <: Suite]]
    val suite = new ConfigMapWrapperSuite(clazz)
    assert(suite.tags === Map("blue test" -> Set("org.scalatest.FastAsLight"), "ignore me" -> Set("org.scalatest.Ignore")))
  }
}
