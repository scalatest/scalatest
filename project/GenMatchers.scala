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

    val deprecatedMustMatchers = """
                                   | package org.scalatest.matchers
                                   |
                                   | @deprecated("Please use org.scalatest.MustMatchers instead.")
                                   | trait MustMatchers extends org.scalatest.MustMatchers
                                   |
                                   | @deprecated("Please use org.scalatest.MustMatchers instead.")
                                   | object MustMatchers extends org.scalatest.MustMatchers
                                 """.stripMargin

    val deprecatedMustMatchersFile = new File(matchersDir, "MustMatchers.scala")
    val deprecatedMustMatchersWriter = new BufferedWriter(new FileWriter(deprecatedMustMatchersFile))
    try {
      deprecatedMustMatchersWriter.write(deprecatedMustMatchers)
    }
    finally {
      deprecatedMustMatchersWriter.flush()
      deprecatedMustMatchersWriter.close()
      println("Generated " + deprecatedMustMatchersFile.getAbsolutePath)
    }

    /*
        val matchersPackageObject = """
          | package org.scalatest
          |
          | package object matchers {
          |
          |   /**
          |    * Convenience type alias allowing <code>MustMatchers</code> to be used in <code>matchers</code> without qualification or another import
          |    * after a wildcard import of <code>org.scalatest</code>.
          |    */
          |
          |   /**
          |    * <p>
          |    * <strong>This class has been moved to the <code>org.scalatest</code> package. The deprecated type alias that has been left in its place will
          |    * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.matchers.MustMatchers</code> to <code>org.scalatest.MustMatchers</code>.</strong>
          |    * </p>
          |    */
          |   @deprecated("Please use org.scalatest.MustMatchers instead.")
          |   type MustMatchers = org.scalatest.MustMatchers
          | }
        """.stripMargin

        val matchersPackageObjectFile = new File(matchersDir, "package.scala")
        val matchersPackageObjectWriter = new BufferedWriter(new FileWriter(matchersPackageObjectFile))
        try {
          matchersPackageObjectWriter.write(matchersPackageObject)
        }
        finally {
          matchersPackageObjectWriter.flush()
          matchersPackageObjectWriter.close()
          println("Generated " + matchersPackageObjectFile.getAbsolutePath)
        }
    */
  }

  def genMain(targetDir: File, version: String, scalaVersion: String) {
    genMainImpl(targetDir, version, scalaVersion, false)
  }

  def genMainForScalaJS(targetDir: File, version: String, scalaVersion: String) {
    genMainImpl(targetDir, version, scalaVersion, true)
  }
}
