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
package org.scalatest.tools

import org.scalatest._
import org.scalatest.events.Event
import org.scalatest.events.Ordinal
import org.scalatest.events.AlertProvided
import scala.collection.mutable
import java.util.regex.Pattern
import java.net.URL
import java.io.File
import org.scalatest.tools.Runner.deglobSuiteParams
import org.scalatest.funspec.AnyFunSpec

class RunnerSpec extends AnyFunSpec with PrivateMethodTester {

  it("deglobSuiteParams should work correctly") {
    val suiteParam =
      SuiteParam("", Array.empty[String], Array.empty[String],
        Array.empty[NestedSuiteParam])

    val classNames =
      List(
        "foo.FooSpec",
        "foo.tools.FooToolsSpec",
        "foo.tools.FooToolsSuite",
        "foo.events.EventsFooSuite")

    def runDeglob(globs: List[String], expecteds: List[String]): Unit = {
      val globSuiteParams =
        globs.map(glob => suiteParam.copy(className = glob))

      val result = deglobSuiteParams(globSuiteParams, classNames.toSet)

      assert(result.map(_.className).toSet === expecteds.toSet)
    }

    runDeglob(List("org.*"), Nil)
    runDeglob(List("f?.*"),  Nil)
    runDeglob(List("f??.*"), classNames)
    runDeglob(List("foo.tools.*"),
      List("foo.tools.FooToolsSpec",
        "foo.tools.FooToolsSuite"))
    runDeglob(List("*.Foo*"),
      List("foo.FooSpec",
        "foo.tools.FooToolsSpec",
        "foo.tools.FooToolsSuite"))
    runDeglob(List("*.Foo*ls*"),
      List("foo.tools.FooToolsSpec",
        "foo.tools.FooToolsSuite"))
    runDeglob(List("*FooS[u]*"),
      List("foo.events.EventsFooSuite"))
    runDeglob(List("*FooS[up]*"),
      List("foo.FooSpec",
        "foo.events.EventsFooSuite"))
  }

  it("readMemoryFiles should issue alert if a Memento isn't runnable") {
    val events = mutable.Set.empty[Event]

    var tracker = new Tracker(new Ordinal(12))

    val reporter = new Reporter {
      def apply(event: Event): Unit = {
        events += event
      }
    }

    Runner.readMemoryFiles(
      List("jvm/scalatest-test/src/test/scala/org/scalatest/tools/memoryfile.eg"),
      reporter, tracker)

    assert(1 === events.filter(_.isInstanceOf[AlertProvided]).size)
  }

  it("should load JUnit wrapper suite class correctly") {
    val clazz = Runner.loadJUnitWrapperClass(Runner.getClass().getClassLoader())
    assert(clazz.getName == "org.scalatestplus.junit.JUnitWrapperSuite")
  }

  it("should load TestNG wrapper suite class correctly") {
    val clazz = Runner.loadTestNGWrapperClass(Runner.getClass().getClassLoader())
    assert(clazz.getName == "org.scalatestplus.testng.TestNGWrapperSuite")
  }

}
