import collection.mutable.ListBuffer
import scala.io.Source
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

  val generatorSource = new File("GenMatchers.scala")

  def translateShouldToMust(shouldLine: String): String = {
    shouldLine
      .replaceAll("Trait <a href=\"../must/Matchers.html\"><code>must.Matchers</code></a> is an alternative to <!-- PRESERVE --><code>should.Matchers</code>", "Trait <code>must.Matchers</code> is an alternative to <!-- PRESERVE --><a href=\"../should/Matchers.html\"><!-- PRESERVE --><code>should.Matchers</code></a>")
      .replaceAll("MustMatchers", "I_NEED_TO_STAY_MUSTMATCHERS")
      .replaceAll("ShouldMatchers", "I_NEED_TO_STAY_SHOULDMATCHERS")
      .replaceAll("must", "I_NEED_TO_STAY_SMALL_MUST")
      .replaceAll("Must", "I_NEED_TO_STAY_BIG_MUST")
      .replaceAll("<!-- PRESERVE --><code>should", "<code>I_NEED_TO_STAY_SMALL_SHOULD")
      .replaceAll("<!-- PRESERVE -->should", " I_NEED_TO_STAY_SMALL_SHOULD") // Why is there a space in front?
      .replaceAll("<!-- PRESERVE --><a href=\"../should/Matchers.html\">", " I_NEED_IN_LINK_TO_STAY_SMALL_SHOULD") // Why is there a space in front?
      .replaceAll("should", "must")
      .replaceAll("Should", "Must")
      .replaceAll("I_NEED_TO_STAY_SMALL_SHOULD", "should")
      .replaceAll("I_NEED_IN_LINK_TO_STAY_SMALL_SHOULD", "<a href=\"../should/Matchers.html\">")
      .replaceAll("I_NEED_TO_STAY_BIG_MUST", "Must")
      .replaceAll("I_NEED_TO_STAY_SMALL_MUST", "must")
      .replaceAll("I_NEED_TO_STAY_SHOULDMATCHERS", "ShouldMatchers")
      .replaceAll("I_NEED_TO_STAY_MUSTMATCHERS", "MustMatchers")
      .replaceAll("import matchers.should.Matchers._", "import matchers.must.Matchers._")
      .replaceAll("import org.scalatest.matchers.should.Matchers._", "import org.scalatest.matchers.must.Matchers._")
  }

  def translateShouldToWill(shouldLine: String): String = {
    shouldLine
      .replaceAll("Trait <a href=\"WillMatchers.html\"><code>WillMatchers</code></a> is an alternative to <code>Matchers</code>", "Trait <code>WillMatchers</code> is an alternative to <a href=\"Matchers.html\"><code>Matchers</code></a>")
      .replaceAll("WillMatchers", "I_NEED_TO_STAY_WILLMATCHERS")
      .replaceAll("ShouldMatchers", "I_NEED_TO_STAY_SHOULDMATCHERS")
      .replaceAll("trait Matchers extends Assertions", "trait WillMatchers extends Expectations")
      .replaceAll("Assertions", "I_NEED_TO_STAY_ASSERTIONS")
      .replaceAll("will", "I_NEED_TO_STAY_SMALL_WILL")
      .replaceAll("Will", "I_NEED_TO_STAY_BIG_WILL")
      .replaceAll("<!-- PRESERVE --><code>should", "<code>I_NEED_TO_STAY_SMALL_SHOULD")
      .replaceAll("<!-- PRESERVE -->should", " I_NEED_TO_STAY_SMALL_SHOULD") // Why is there a space in front?
      .replaceAll("assertATypeShouldBeTrueImpl", "expectATypeWillBeTrueImpl")
      .replaceAll("assertAnTypeShouldBeTrueImpl", "expectAnTypeWillBeTrueImpl")
      .replaceAll("should", "will")
      .replaceAll("Should", "Will")
      .replaceAll("InspectorsHelper", "FactInspectorsHelper")
      .replaceAll("object Matchers extends Matchers", "object WillMatchers extends WillMatchers")
      .replaceAll("import MatchersHelper.indicateSuccess", "import WillMatchersHelper.indicateSuccess")
      .replaceAll("import MatchersHelper.indicateFailure", "import WillMatchersHelper.indicateFailure")
      .replaceAll("import org.scalatest.MatchersHelper.indicateSuccess", "import org.scalatest.WillMatchersHelper.indicateSuccess")
      .replaceAll("import org.scalatest.MatchersHelper.indicateFailure", "import org.scalatest.WillMatchersHelper.indicateFailure")
      .replaceAll("import org.scalatest.MatchersHelper.checkNoException", "import org.scalatest.WillMatchersHelper.checkNoException")
      .replaceAll("Assertion", "Fact")
      .replaceAll("I_NEED_TO_STAY_SMALL_SHOULD", "should")
      .replaceAll("I_NEED_TO_STAY_BIG_WILL", "Will")
      .replaceAll("I_NEED_TO_STAY_SMALL_WILL", "will")
      .replaceAll("I_NEED_TO_STAY_SHOULDMATCHERS", "ShouldMatchers")
      .replaceAll("I_NEED_TO_STAY_WILLMATCHERS", "WillMatchers")
      .replaceAll("I_NEED_TO_STAY_ASSERTIONS", "Assertions")
      .replaceAll("import matchers.should.Matchers._", "import matchers.will.WillMatchers._")
      .replaceAll("import org.scalatest.matchers.should.Matchers._", "import org.scalatest.matchers.will.WillMatchers._")
      .replaceAll("Matchers.scala", "WillMatchers.scala")
      .replaceAll("NoExceptionWord", "FactExceptionWord")
      .replaceAll("ResultOfATypeInvocation", "FactResultOfATypeInvocation")
      .replaceAll("ResultOfAnTypeInvocation", "FactResultOfAnTypeInvocation")
      .replaceAll("ResultOfBeWordForAType", "FactResultOfBeWordForAType")
      .replaceAll("ResultOfBeWordForAnType", "FactResultOfBeWordForAnType")
      .replaceAll("ResultOfBeWordForNoException", "FactResultOfBeWordForNoException")
      .replaceAll("ResultOfContainWord", "FactResultOfContainWord")
      .replaceAll("ResultOfNotWordForAny", "FactResultOfNotWordForAny")
      .replaceAll("MatcherWords", "FactMatcherWords")
  }

  def translateFile(targetDir: File, fileName: String, sourceFileName: String, scalaVersion: String, translateFun: String => String): File = {
    val outputFile = new File(targetDir, fileName)
    if (!outputFile.exists || generatorSource.lastModified > outputFile.lastModified) {
      val outputWriter = new BufferedWriter(new FileWriter(outputFile))
      try {
        val lines = Source.fromFile(new File(sourceFileName)).getLines.toList
        var skipMode = false
        for (line <- lines) {
          val mustLine: String = translateFun(line)

          outputWriter.write(mustLine)
          outputWriter.newLine()
        }
      }
      finally {
        outputWriter.flush()
        outputWriter.close()
        println("Generated " + outputFile.getAbsolutePath)
      }
    }
    outputFile
  }

  def translateFileJS(targetDir: File, fileName: String, sourceFileName: String, scalaVersion: String, translateFun: String => String): File = {
    val outputFile = new File(targetDir, fileName)
    if (!outputFile.exists || generatorSource.lastModified > outputFile.lastModified) {
      val outputWriter = new BufferedWriter(new FileWriter(outputFile))
      try {
        val lines = Source.fromFile(new File(sourceFileName)).getLines.toList
        var skipMode = false
        for (line <- lines) {
          val mustLine: String =
            if (line.trim == "// SKIP-SCALATESTJS,NATIVE-START") {
              skipMode = true
              ""
            }
            else if (line.trim == "// SKIP-SCALATESTJS,NATIVE-END") {
              skipMode = false
              ""
            }
            else if (!skipMode) {
              if (line.trim.startsWith("//SCALATESTJS,NATIVE-ONLY "))
                translateFun(line.substring(line.indexOf("//SCALATESTJS,NATIVE-ONLY ") + 26))
              else
                translateFun(line)
            }
            else
              ""

          outputWriter.write(mustLine)
          outputWriter.newLine()
        }
      }
      finally {
        outputWriter.flush()
        outputWriter.close()
        println("Generated " + outputFile.getAbsolutePath)
      }
    }
    outputFile
  }

  def translateFileDotty(targetDir: File, fileName: String, sourceFileName: String, scalaVersion: String, translateFun: String => String): File = {
    val outputFile = new File(targetDir, fileName)
    if (!outputFile.exists || generatorSource.lastModified > outputFile.lastModified) {
      val outputWriter = new BufferedWriter(new FileWriter(outputFile))
      try {
        val lines = Source.fromFile(new File(sourceFileName)).getLines.toList
        var skipMode = false
        for (line <- lines) {
          val mustLine: String =
            if (line.trim == "// SKIP-DOTTY-START") {
              skipMode = true
              ""
            }
            else if (line.trim == "// SKIP-DOTTY-END") {
              skipMode = false
              ""
            }
            else if (!skipMode) {
              if (line.trim.startsWith("//DOTTY-ONLY "))
                translateFun(line.substring(line.indexOf("//DOTTY-ONLY ") + 13))
              else
                translateFun(line)
            }
            else
              ""

          outputWriter.write(mustLine)
          outputWriter.newLine()
        }
      }
      finally {
        outputWriter.flush()
        outputWriter.close()
        println("Generated " + outputFile.getAbsolutePath)
      }
    }
    outputFile
  }

  def translateFileDottyJS(targetDir: File, fileName: String, sourceFileName: String, scalaVersion: String, translateFun: String => String): File = {
    val outputFile = new File(targetDir, fileName)
    if (!outputFile.exists || generatorSource.lastModified > outputFile.lastModified) {
      val outputWriter = new BufferedWriter(new FileWriter(outputFile))
      try {
        val lines = Source.fromFile(new File(sourceFileName)).getLines.toList
        var skipMode = false
        for (line <- lines) {
          val mustLine: String =
            if (line.trim == "// SKIP-DOTTY-START") {
              skipMode = true
              ""
            }
            else if (line.trim == "// SKIP-DOTTY-END") {
              skipMode = false
              ""
            }
            else if (line.trim == "// SKIP-SCALATESTJS,NATIVE-START") {
              skipMode = true
              ""
            }
            else if (line.trim == "// SKIP-SCALATESTJS,NATIVE-END") {
              skipMode = false
              ""
            }
            else if (!skipMode) {
              if (line.trim.startsWith("//DOTTY-ONLY "))
                translateFun(line.substring(line.indexOf("//DOTTY-ONLY ") + 13))
              else if (line.trim.startsWith("//SCALATESTJS,NATIVE-ONLY "))
                translateFun(line.substring(line.indexOf("//SCALATESTJS,NATIVE-ONLY ") + 26))
              else
                translateFun(line)
            }
            else
              ""

          outputWriter.write(mustLine)
          outputWriter.newLine()
        }
      }
      finally {
        outputWriter.flush()
        outputWriter.close()
        println("Generated " + outputFile.getAbsolutePath)
      }
    }
    outputFile
  }

  def genMain(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    targetDir.mkdirs()
    val matchersDir = new File(targetDir, "matchers")
    matchersDir.mkdirs()
    val shouldDir = new File(matchersDir, "should")
    shouldDir.mkdirs()
    val mustDir = new File(matchersDir, "must")
    mustDir.mkdirs()
    Seq(
      translateFile(mustDir, "Matchers.scala", "jvm/shouldmatchers/src/main/scala/org/scalatest/matchers/should/Matchers.scala", scalaVersion, translateShouldToMust)
      /*translateFile(targetDir, "WillMatchers.scala", "scalatest/src/main/scala/org/scalatest/Matchers.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactNoExceptionWord.scala", "scalatest/src/main/scala/org/scalatest/words/NoExceptionWord.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfATypeInvocation.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfATypeInvocation.scala", scalaVersion, scalaJS,
        (line: String) => translateShouldToWill(line.replaceAll("PleaseUseNoExceptionShouldSyntaxInstead", "STAY_AS_PLEASEUSNOTEXCEPTIONSHOULDSYNTAXINSTEAD"))
          .replaceAll("STAY_AS_PLEASEUSNOTEXCEPTIONSHOULDSYNTAXINSTEAD", "PleaseUseNoExceptionShouldSyntaxInstead")
      )
      translateFile(targetDir, "FactResultOfAnTypeInvocation.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfAnTypeInvocation.scala", scalaVersion, scalaJS,
        (line: String) => translateShouldToWill(line.replaceAll("PleaseUseNoExceptionShouldSyntaxInstead", "STAY_AS_PLEASEUSNOTEXCEPTIONSHOULDSYNTAXINSTEAD"))
                          .replaceAll("STAY_AS_PLEASEUSNOTEXCEPTIONSHOULDSYNTAXINSTEAD", "PleaseUseNoExceptionShouldSyntaxInstead")
      )
      translateFile(targetDir, "FactResultOfBeWordForAType.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfBeWordForAType.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfBeWordForAnType.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfBeWordForAnType.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfBeWordForNoException.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfBeWordForNoException.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfContainWord.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfContainWord.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfNotWordForAny.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfNotWordForAny.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactMatcherWords.scala", "scalatest/src/main/scala/org/scalatest/words/MatcherWords.scala", scalaVersion, scalaJS, translateShouldToWill)*/
    )
  }

  def genMainForScalaJS(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    targetDir.mkdirs()
    val matchersDir = new File(targetDir, "matchers")
    matchersDir.mkdirs()
    val shouldDir = new File(matchersDir, "should")
    shouldDir.mkdirs()
    val mustDir = new File(matchersDir, "must")
    mustDir.mkdirs()
    Seq(
      translateFileJS(mustDir, "Matchers.scala", "jvm/shouldmatchers/src/main/scala/org/scalatest/matchers/should/Matchers.scala", scalaVersion, translateShouldToMust)
      /*translateFile(targetDir, "WillMatchers.scala", "scalatest/src/main/scala/org/scalatest/Matchers.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactNoExceptionWord.scala", "scalatest/src/main/scala/org/scalatest/words/NoExceptionWord.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfATypeInvocation.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfATypeInvocation.scala", scalaVersion, scalaJS,
        (line: String) => translateShouldToWill(line.replaceAll("PleaseUseNoExceptionShouldSyntaxInstead", "STAY_AS_PLEASEUSNOTEXCEPTIONSHOULDSYNTAXINSTEAD"))
          .replaceAll("STAY_AS_PLEASEUSNOTEXCEPTIONSHOULDSYNTAXINSTEAD", "PleaseUseNoExceptionShouldSyntaxInstead")
      )
      translateFile(targetDir, "FactResultOfAnTypeInvocation.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfAnTypeInvocation.scala", scalaVersion, scalaJS,
        (line: String) => translateShouldToWill(line.replaceAll("PleaseUseNoExceptionShouldSyntaxInstead", "STAY_AS_PLEASEUSNOTEXCEPTIONSHOULDSYNTAXINSTEAD"))
                          .replaceAll("STAY_AS_PLEASEUSNOTEXCEPTIONSHOULDSYNTAXINSTEAD", "PleaseUseNoExceptionShouldSyntaxInstead")
      )
      translateFile(targetDir, "FactResultOfBeWordForAType.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfBeWordForAType.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfBeWordForAnType.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfBeWordForAnType.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfBeWordForNoException.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfBeWordForNoException.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfContainWord.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfContainWord.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfNotWordForAny.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfNotWordForAny.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactMatcherWords.scala", "scalatest/src/main/scala/org/scalatest/words/MatcherWords.scala", scalaVersion, scalaJS, translateShouldToWill)*/
    )
  }

  def genMainForDotty(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    targetDir.mkdirs()
    val matchersDir = new File(targetDir, "matchers")
    matchersDir.mkdirs()
    val shouldDir = new File(matchersDir, "should")
    shouldDir.mkdirs()
    val mustDir = new File(matchersDir, "must")
    mustDir.mkdirs()
    Seq(
      translateFileDotty(mustDir, "Matchers.scala", "jvm/shouldmatchers/src/main/scala/org/scalatest/matchers/should/Matchers.scala", scalaVersion, translateShouldToMust)
      /*translateFile(targetDir, "WillMatchers.scala", "scalatest/src/main/scala/org/scalatest/Matchers.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactNoExceptionWord.scala", "scalatest/src/main/scala/org/scalatest/words/NoExceptionWord.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfATypeInvocation.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfATypeInvocation.scala", scalaVersion, scalaJS,
        (line: String) => translateShouldToWill(line.replaceAll("PleaseUseNoExceptionShouldSyntaxInstead", "STAY_AS_PLEASEUSNOTEXCEPTIONSHOULDSYNTAXINSTEAD"))
          .replaceAll("STAY_AS_PLEASEUSNOTEXCEPTIONSHOULDSYNTAXINSTEAD", "PleaseUseNoExceptionShouldSyntaxInstead")
      )
      translateFile(targetDir, "FactResultOfAnTypeInvocation.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfAnTypeInvocation.scala", scalaVersion, scalaJS,
        (line: String) => translateShouldToWill(line.replaceAll("PleaseUseNoExceptionShouldSyntaxInstead", "STAY_AS_PLEASEUSNOTEXCEPTIONSHOULDSYNTAXINSTEAD"))
                          .replaceAll("STAY_AS_PLEASEUSNOTEXCEPTIONSHOULDSYNTAXINSTEAD", "PleaseUseNoExceptionShouldSyntaxInstead")
      )
      translateFile(targetDir, "FactResultOfBeWordForAType.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfBeWordForAType.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfBeWordForAnType.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfBeWordForAnType.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfBeWordForNoException.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfBeWordForNoException.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfContainWord.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfContainWord.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfNotWordForAny.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfNotWordForAny.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactMatcherWords.scala", "scalatest/src/main/scala/org/scalatest/words/MatcherWords.scala", scalaVersion, scalaJS, translateShouldToWill)*/
    )
  }

  def genMainForDottyJS(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    targetDir.mkdirs()
    val matchersDir = new File(targetDir, "matchers")
    matchersDir.mkdirs()
    val shouldDir = new File(matchersDir, "should")
    shouldDir.mkdirs()
    val mustDir = new File(matchersDir, "must")
    mustDir.mkdirs()
    Seq(
      translateFileDottyJS(mustDir, "Matchers.scala", "jvm/shouldmatchers/src/main/scala/org/scalatest/matchers/should/Matchers.scala", scalaVersion, translateShouldToMust)
      /*translateFile(targetDir, "WillMatchers.scala", "scalatest/src/main/scala/org/scalatest/Matchers.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactNoExceptionWord.scala", "scalatest/src/main/scala/org/scalatest/words/NoExceptionWord.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfATypeInvocation.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfATypeInvocation.scala", scalaVersion, scalaJS,
        (line: String) => translateShouldToWill(line.replaceAll("PleaseUseNoExceptionShouldSyntaxInstead", "STAY_AS_PLEASEUSNOTEXCEPTIONSHOULDSYNTAXINSTEAD"))
          .replaceAll("STAY_AS_PLEASEUSNOTEXCEPTIONSHOULDSYNTAXINSTEAD", "PleaseUseNoExceptionShouldSyntaxInstead")
      )
      translateFile(targetDir, "FactResultOfAnTypeInvocation.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfAnTypeInvocation.scala", scalaVersion, scalaJS,
        (line: String) => translateShouldToWill(line.replaceAll("PleaseUseNoExceptionShouldSyntaxInstead", "STAY_AS_PLEASEUSNOTEXCEPTIONSHOULDSYNTAXINSTEAD"))
                          .replaceAll("STAY_AS_PLEASEUSNOTEXCEPTIONSHOULDSYNTAXINSTEAD", "PleaseUseNoExceptionShouldSyntaxInstead")
      )
      translateFile(targetDir, "FactResultOfBeWordForAType.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfBeWordForAType.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfBeWordForAnType.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfBeWordForAnType.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfBeWordForNoException.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfBeWordForNoException.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfContainWord.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfContainWord.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactResultOfNotWordForAny.scala", "scalatest/src/main/scala/org/scalatest/words/ResultOfNotWordForAny.scala", scalaVersion, scalaJS, translateShouldToWill)
      translateFile(targetDir, "FactMatcherWords.scala", "scalatest/src/main/scala/org/scalatest/words/MatcherWords.scala", scalaVersion, scalaJS, translateShouldToWill)*/
    )
  }
}
