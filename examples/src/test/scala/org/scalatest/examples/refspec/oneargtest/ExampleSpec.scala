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
package org.scalatest.examples.refspec.oneargtest

import org.scalatest.fixture
import java.io._

class ExampleSpec extends fixture.Spec {

  case class FixtureParam(file: File, writer: FileWriter)

  def withFixture(test: OneArgTest) = {

    // create the fixture
    val file = File.createTempFile("hello", "world")
    val writer = new FileWriter(file)
    val theFixture = FixtureParam(file, writer)

    try {
      writer.write("ScalaTest is ") // set up the fixture
      withFixture(test.toNoArgTest(theFixture)) // "loan" the fixture to the test
    }
    finally writer.close() // clean up the fixture
  }

  object `Testing ` {
    def `should be easy` { f: FixtureParam =>
      f.writer.write("easy!")
      f.writer.flush()
      assert(f.file.length === 18)
    }
 
    def `should be fun` { f: FixtureParam =>
      f.writer.write("fun!")
      f.writer.flush()
      assert(f.file.length === 17)
    }
  } 
}
