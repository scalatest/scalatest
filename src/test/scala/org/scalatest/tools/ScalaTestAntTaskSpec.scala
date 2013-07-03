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
package org.scalatest.tools

import org.scalatest._
import java.util.regex.Pattern
import java.net.URL
import java.io.File

class ScalaTestAntTaskSpec extends Spec {

  def `command line args should get generated for <test> elements` {
    val task = new ScalaTestAntTask

    def addTestName(name: String) {
      val elem = new TestElement
      elem.setName(name)
      task.addConfiguredTest(elem)
    }

    def addTestGlob(glob: String) {
      val elem = new TestElement
      elem.setGlob(glob)
      task.addConfiguredTest(elem)
    }

    addTestName("this is a test")
    addTestName("this is another test")
    addTestGlob("this is a glob")

    val argsList = task.buildArgsList

    assert(argsList containsSlice List("-z", "this is a glob"))
    assert(argsList containsSlice List("-t", "this is a test"))
    assert(argsList containsSlice List("-t", "this is another test"))
  }
}
