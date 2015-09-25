import collection.mutable.ListBuffer
import io.Source
import java.io.{File, FileWriter, BufferedWriter}

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

object GenMatchers {

  def translateShouldToMust(shouldLine: String): String = {
    shouldLine
      .replaceAll("Trait <a href=\"MustMatchers.html\"><code>MustMatchers</code></a> is an alternative to <code>Matchers</code>", "Trait <code>MustMatchers</code> is an alternative to <a href=\"Matchers.html\"><code>Matchers</code></a>")
      .replaceAll("MustMatchers", "I_NEED_TO_STAY_MUSTMATCHERS")
      .replaceAll("ShouldMatchers", "I_NEED_TO_STAY_SHOULDMATCHERS")
      .replaceAll("must", "I_NEED_TO_STAY_SMALL_MUST")
      .replaceAll("Must", "I_NEED_TO_STAY_BIG_MUST")
      .replaceAll("<!-- PRESERVE --><code>should", "<code>I_NEED_TO_STAY_SMALL_SHOULD")
      .replaceAll("<!-- PRESERVE -->should", " I_NEED_TO_STAY_SMALL_SHOULD") // Why is there a space in front?
      .replaceAll("should", "must")
      .replaceAll("Should", "Must")
      .replaceAll("trait Matchers", "trait MustMatchers")
      .replaceAll("object Matchers extends Matchers", "object MustMatchers extends MustMatchers")
      .replaceAll("I_NEED_TO_STAY_SMALL_SHOULD", "should")
      .replaceAll("I_NEED_TO_STAY_BIG_MUST", "Must")
      .replaceAll("I_NEED_TO_STAY_SMALL_MUST", "must")
      .replaceAll("I_NEED_TO_STAY_SHOULDMATCHERS", "ShouldMatchers")
      .replaceAll("I_NEED_TO_STAY_MUSTMATCHERS", "MustMatchers")
      .replaceAll("import Matchers._", "import MustMatchers._")
      .replaceAll("import org.scalatest.Matchers._", "import org.scalatest.MustMatchers._")
      .replaceAll("Matchers.scala", "MustMatchers.scala")
  }

  def translateShouldToWill(shouldLine: String): String = {
    shouldLine
      .replaceAll("Trait <a href=\"WillMatchers.html\"><code>WillMatchers</code></a> is an alternative to <code>Matchers</code>", "Trait <code>WillMatchers</code> is an alternative to <a href=\"Matchers.html\"><code>Matchers</code></a>")
      .replaceAll("WillMatchers", "I_NEED_TO_STAY_WILLMATCHERS")
      .replaceAll("ShouldMatchers", "I_NEED_TO_STAY_SHOULDMATCHERS")
      .replaceAll("Assertions", "I_NEED_TO_STAY_ASSERTIONS")
      .replaceAll("will", "I_NEED_TO_STAY_SMALL_WILL")
      .replaceAll("Will", "I_NEED_TO_STAY_BIG_WILL")
      .replaceAll("<!-- PRESERVE --><code>should", "<code>I_NEED_TO_STAY_SMALL_SHOULD")
      .replaceAll("<!-- PRESERVE -->should", " I_NEED_TO_STAY_SMALL_SHOULD") // Why is there a space in front?
      .replaceAll("should", "will")
      .replaceAll("Should", "Will")
      .replaceAll("trait Matchers", "trait WillMatchers")
      .replaceAll("object Matchers extends Matchers", "object WillMatchers extends WillMatchers")
      .replaceAll("import MatchersHelper.indicateSuccess", "import WillMatchersHelper.indicateSuccess")
      .replaceAll("import MatchersHelper.indicateFailure", "import WillMatchersHelper.indicateFailure")
      .replaceAll("extends Assertions with", "extends")
      .replaceAll("Assertion", "Fact")
      .replaceAll("I_NEED_TO_STAY_SMALL_SHOULD", "should")
      .replaceAll("I_NEED_TO_STAY_BIG_WILL", "Will")
      .replaceAll("I_NEED_TO_STAY_SMALL_WILL", "will")
      .replaceAll("I_NEED_TO_STAY_SHOULDMATCHERS", "ShouldMatchers")
      .replaceAll("I_NEED_TO_STAY_WILLMATCHERS", "WillMatchers")
      .replaceAll("I_NEED_TO_STAY_ASSERTIONS", "Assertions")
      .replaceAll("import Matchers._", "import WillMatchers._")
      .replaceAll("import org.scalatest.Matchers._", "import org.scalatest.WillMatchers._")
      .replaceAll("Matchers.scala", "WillMatchers.scala")
  }

  def genMainImpl(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Unit = {
    targetDir.mkdirs()
    val matchersDir = new File(targetDir, "matchers")
    matchersDir.mkdirs()
    val junitDir = new File(targetDir, "junit")
    junitDir.mkdirs()

    val mustMatchersFile = new File(targetDir, "MustMatchers.scala")
    val mustMatchersWriter = new BufferedWriter(new FileWriter(mustMatchersFile))
    try {
      val lines = Source.fromFile(new File("scalatest/src/main/scala/org/scalatest/Matchers.scala")).getLines.toList
      var skipMode = false
      for (line <- lines) {
        val mustLine: String =
          if (scalaJS) {
            if (line.trim == "// SKIP-SCALATESTJS-START") {
              skipMode = true
              ""
            }
            else if (line.trim == "// SKIP-SCALATESTJS-END") {
              skipMode = false
              ""
            }
            else if (!skipMode) {
              if (line.trim.startsWith("//SCALATESTJS-ONLY "))
                translateShouldToMust(line.substring(line.indexOf("//SCALATESTJS-ONLY ") + 19))
              else
                translateShouldToMust(line)
            }
            else
              ""
          }
          else
            translateShouldToMust(line)

        mustMatchersWriter.write(mustLine)
        mustMatchersWriter.newLine()
      }
    }
    finally {
      mustMatchersWriter.flush()
      mustMatchersWriter.close()
      println("Generated " + mustMatchersFile.getAbsolutePath)
    }

    /*val willMatchersFile = new File(targetDir, "WillMatchers.scala")
    val willMatchersWriter = new BufferedWriter(new FileWriter(willMatchersFile))
    try {
      val lines = Source.fromFile(new File("scalatest/src/main/scala/org/scalatest/Matchers.scala")).getLines.toList
      var skipMode = false
      for (line <- lines) {
        val willLine: String =
          if (scalaJS) {
            if (line.trim == "// SKIP-SCALATESTJS-START") {
              skipMode = true
              ""
            }
            else if (line.trim == "// SKIP-SCALATESTJS-END") {
              skipMode = false
              ""
            }
            else if (!skipMode) {
              if (line.trim.startsWith("//SCALATESTJS-ONLY "))
                translateShouldToWill(line.substring(line.indexOf("//SCALATESTJS-ONLY ") + 19))
              else
                translateShouldToWill(line)
            }
            else
              ""
          }
          else
            translateShouldToWill(line)

        willMatchersWriter.write(willLine)
        willMatchersWriter.newLine()
      }
    }
    finally {
      willMatchersWriter.flush()
      willMatchersWriter.close()
      println("Generated " + willMatchersFile.getAbsolutePath)
    }*/
  }

  def genMain(targetDir: File, version: String, scalaVersion: String) {
    genMainImpl(targetDir, version, scalaVersion, false)
  }

  def genMainForScalaJS(targetDir: File, version: String, scalaVersion: String) {
    genMainImpl(targetDir, version, scalaVersion, true)
  }
}
