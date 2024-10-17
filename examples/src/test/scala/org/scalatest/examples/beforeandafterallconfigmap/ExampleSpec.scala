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
import org.scalatest._
import java.io._

trait TempFileExistsSpec extends fixture.FlatSpecLike {

  type FixtureParam = File
  override def withFixture(test: OneArgTest) = {
    val fileName = test.configMap("tempFileName").asInstanceOf[String]
    val file = new File(fileName)
    withFixture(test.toNoArgTest(file)) // loan the fixture to the test
  }

  "The temp file" should ("exist in " + suiteName) in { file =>
    assert(file.exists)
  }
}

class OneSpec extends TempFileExistsSpec
class TwoSpec extends TempFileExistsSpec
class RedSpec extends TempFileExistsSpec
class BlueSpec extends TempFileExistsSpec

class ExampleSpec extends Suites(
  new OneSpec,
  new TwoSpec,
  new RedSpec,
  new BlueSpec
) with TempFileExistsSpec with BeforeAndAfterAllConfigMap {

  private val tempFileName = "tempFileName"

  // Set up the temp file needed by the test, taking
  // a file name from the configMap
  override def beforeAll(configMap: ConfigMap) {

    require(
      configMap.isDefinedAt(tempFileName),
      "must place a temp file name in the configMap under the key: " + tempFileName
    )

    val fileName = configMap(tempFileName).asInstanceOf[String]

    val writer = new FileWriter(fileName)
    try writer.write("Hello, suite of tests!")
    finally writer.close()
  }

  // Delete the temp file
  override def afterAll(configMap: ConfigMap) {
    // No need to require that configMap contains the key again because it won't get
    // here if it didn't contain the key in beforeAll
    val fileName = configMap("tempFileName").asInstanceOf[String]
    val file = new File(fileName)
    file.delete()
  }
}
