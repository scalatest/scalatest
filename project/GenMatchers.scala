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
      .replaceAll("MustMatchers", "I_NEED_TO_STAY_MUSTMATCHERS")
      .replaceAll("ShouldMatchers", "I_NEED_TO_STAY_SHOULDMATCHERS")
      .replaceAll("must", "I_NEED_TO_STAY_SMALL_MUST")
      .replaceAll("Must", "I_NEED_TO_STAY_BIG_MUST")
      .replaceAll("<!-- PRESERVE -->should", " I_NEED_TO_STAY_SMALL_SHOULD")
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

  def genMain(targetDir: File, scalaVersion: String) {
    targetDir.mkdirs()
    val matchersDir = new File(targetDir, "matchers")
    matchersDir.mkdirs()
    val junitDir = new File(targetDir, "junit")
    junitDir.mkdirs()

    val mustMatchersFile = new File(targetDir, "MustMatchers.scala")
    val mustMatchersWriter = new BufferedWriter(new FileWriter(mustMatchersFile))
    try {
      val lines = Source.fromFile(new File("src/main/scala/org/scalatest/Matchers.scala")).getLines.toList
      for (line <- lines) {
        val mustLine = translateShouldToMust(line)
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

   def translateShouldToMustInTests(shouldLine: String): String = {
    val temp1 = shouldLine.replaceAll("<code>must</code>", "<code>I_WAS_must_ORIGINALLY</code>")
    val temp2 = temp1.replaceAll("<!-- PRESERVE -->should", " I_MUST_STAY_SHOULD")
    val temp3 = temp2.replaceAll(
      "<a href=\"MustMatchers.html\"><code>MustMatchers</code></a>",
      "<a href=\"I_WAS_Must_ORIGINALLYMatchers.html\"><code>I_WAS_Must_ORIGINALLYMatchers</code></a>"
    )
    val temp4 = temp3.replaceAll("should", "must")
    val temp5 = temp4.replaceAll("Should", "Must")
    val temp6 = temp5.replaceAll("trait Matchers", "trait MustMatchers")
    val temp7 = temp6.replaceAll("object Matchers extends Matchers", "object MustMatchers extends MustMatchers")
    val temp8 = temp7.replaceAll("I_WAS_must_ORIGINALLY", "should")
    val temp9 = temp8.replaceAll(" I_MUST_STAY_SHOULD", "should")
    val temp10 = temp9.replaceAll("import Matchers._", "import MustMatchers._")
    val temp11 = temp10.replaceAll("with Matchers", "with MustMatchers")
    val temp12 = temp11.replaceAll("Matchers.scala", "MustMatchers.scala")
    temp12.replaceAll("I_WAS_Must_ORIGINALLY", "Should")
  }

  def genTest(targetBaseDir: File, scalaVersion: String) {
    val sourceBaseDir = new File("src/test/scala/org/scalatest")
    val matchersDir = new File(targetBaseDir, "matchers")
    matchersDir.mkdirs()

    def transformFile(shouldFile: File, mustFile: File) {
      val writer = new BufferedWriter(new FileWriter(mustFile))
      try {
        val shouldLines = Source.fromFile(shouldFile).getLines().toList // for 2.8
        for (shouldLine <- shouldLines) {
          val mustLine = translateShouldToMustInTests(shouldLine)
          writer.write(mustLine.toString)
          writer.newLine() // add for 2.8
        }
      }
      finally {
        writer.close()
        println("Generated " + mustFile.getAbsolutePath)
      }
    }

    def isShouldFile(file: File): Boolean =
      file.isFile && (file.getName.startsWith("Should") || file.getName.startsWith("ListShould") || file.getName.startsWith("EveryShould") || file.getName.startsWith("OptionShould"))

    // For those under org.scalatest
    for (shouldFile <- sourceBaseDir.listFiles) {
      if (isShouldFile(shouldFile)) {
        val shouldFileName = shouldFile.getName

        val mustFileName = shouldFileName.replace("Should", "Must")

        val mustFile = new File(targetBaseDir, mustFileName)
        transformFile(new File(sourceBaseDir, shouldFileName), mustFile)
      }
    }

    // For those under org.scalatest.matchers
    val matchersSourceDir = new File(sourceBaseDir, "matchers")
    for (shouldFile <- matchersSourceDir.listFiles) {
      if (isShouldFile(shouldFile)) {
        val shouldFileName = shouldFile.getName

        val mustFileName = shouldFileName.replace("Should", "Must")

        val mustMatchersFile = new File(matchersDir, mustFileName)
        transformFile(new File(matchersSourceDir, shouldFileName), mustMatchersFile)
      }
    }
  }

  def main(args: Array[String]) {
    val targetDir = args(0)
    val scalaVersion = args(1)
    genMain(new File(targetDir + "/main/scala/org/scalatest/"), scalaVersion)
    genTest(new File("gentests/" + targetDir + "/test/scala/org/scalatest/"), scalaVersion)
  }
}
