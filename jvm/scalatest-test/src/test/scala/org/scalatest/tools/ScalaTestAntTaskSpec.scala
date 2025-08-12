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
import java.util.regex.Pattern
import java.net.URL
import java.io.File
import org.scalatest.funspec.AnyFunSpec

class ScalaTestAntTaskSpec extends AnyFunSpec {

  it("command line args should get generated for <test> elements") {
    val task = new ScalaTestAntTask

    def addTestName(name: String): Unit = {
      val elem = new TestElement
      elem.setName(name)
      task.addConfiguredTest(elem)
    }

    def addTestSubstring(substring: String): Unit = {
      val elem = new TestElement
      elem.setSubstring(substring)
      task.addConfiguredTest(elem)
    }

    addTestName("this is a test")
    addTestName("this is another test")
    addTestSubstring("this is a substring")

    val argsList = task.buildArgsList

    assert(argsList containsSlice List("-z", "this is a substring"))
    assert(argsList containsSlice List("-t", "this is a test"))
    assert(argsList containsSlice List("-t", "this is another test"))
  }

  it("command line arg should get generated for testsfile attribute") {
    val task = new ScalaTestAntTask

    task.setTestsfile("target/testsfile.dat")

    val argsList = task.buildArgsList

    assert(argsList containsSlice List("-A", "target/testsfile.dat"))
  }

  it("command line args should get generated for testsfile elements") {
    val task = new ScalaTestAntTask

    val elem = new TestsfileElement
    elem.setFilename("target/testsfile.dat")
    task.addConfiguredTestsfile(elem)

    val elem2 = new TestsfileElement
    elem2.setFilename("target/testsfile2.dat")
    task.addConfiguredTestsfile(elem2)

    val argsList = task.buildArgsList

    assert(argsList containsSlice List("-A", "target/testsfile.dat"))
    assert(argsList containsSlice List("-A", "target/testsfile2.dat"))
  }

  it("command line arg should get generated for memory reporter") {
    val task = new ScalaTestAntTask

    val elem = new ReporterElement
    elem.setType("memory")
    elem.setFilename("target/memory.out")
    task.addConfiguredReporter(elem)

    val argsList = task.buildArgsList

    assert(argsList containsSlice List("-M", "target/memory.out"))
  }
}
