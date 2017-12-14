/*
* Copyright 2001-2015 Artima, Inc.
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

import io.Source
import java.io.{File, FileWriter, BufferedWriter}

object GenCommonTestNative {

  private def uncommentJsExport(line: String): String =
    if (line.trim.startsWith("//SCALATESTJS,NATIVE-ONLY "))
      line.substring(line.indexOf("//SCALATESTJS,NATIVE-ONLY ") + 26)
    else if (line.trim.startsWith("//SCALATESTNATIVE-ONLY "))
      line.substring(line.indexOf("//SCALATESTNATIVE-ONLY ") + 23)
    else
      line

  private def transformLine(line: String): String =
    uncommentJsExport(line)


  private def copyFile(sourceFile: File, destFile: File): File = {
    val destWriter = new BufferedWriter(new FileWriter(destFile))
    try {
      val lines = Source.fromFile(sourceFile).getLines.toList
      var skipMode = false
      for (line <- lines) {
        if (line.trim == "// SKIP-SCALATESTJS,NATIVE-START" || line.trim == "// SKIP-SCALATESTNATIVE-START")
          skipMode = true
        else if (line.trim == "// SKIP-SCALATESTJS,NATIVE-END" || line.trim == "// SKIP-SCALATESTNATIVE-END")
          skipMode = false
        else if (!skipMode) {
          destWriter.write(transformLine(line))
          destWriter.newLine()
        }
      }
      destFile
    }
    finally {
      destWriter.flush()
      destWriter.close()
      println("Copied " + destFile.getAbsolutePath)
    }
  }

  def copyDir(sourceDirName: String, packageDirName: String, files: List[String], targetDir: File): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    files.map { sourceFileName =>
      val sourceFile = new File(sourceDir, sourceFileName)
      val destFile = new File(packageDir, sourceFile.getName)
      copyFile(sourceFile, destFile)
    }
  }

  def genMain(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyDir("common-test/src/main/scala/org/scalatest", "org/scalatest",
      List(
        "SharedHelpers.scala",
        "mytags.scala",
        "StubReporter.scala",
        "LineNumberMacro.scala",
        "LineNumberHelper.scala",
        "ReturnsNormallyThrowsAssertion.scala",
        "BookPropertyMatchers.scala",
        "EmptyMocks.scala",
        "FileMocks.scala",
        "StringFixture.scala",
        "CompatParColls.scala"
      ), targetDir) ++
    copyDir("common-test/src/main/scala/org/scalatest/path", "org/scalatest/path",
      List("ExampleLikeSpecs.scala"), targetDir) ++
    copyDir("common-test/src/main/scala/scala/util/parsing/combinator", "scala/util/parsing/combinator",
      List(
        "ImplicitConversions.scala",
        "JavaTokenParsers.scala",
        "PackratParsers.scala",
        "Parsers.scala",
        "RegexParsers.scala",
        "SubSequence.scala"
      ), targetDir) ++
    copyDir("common-test/src/main/scala/scala/util/parsing/combinator/lexical", "scala/util/parsing/combinator/lexical",
      List(
        "Lexical.scala",
        "Scanners.scala",
        "StdLexical.scala"
      ), targetDir) ++
    copyDir("common-test/src/main/scala/scala/util/parsing/combinator/syntactical", "scala/util/parsing/combinator/syntactical",
      List(
        "StandardTokenParsers.scala",
        "StdTokenParsers.scala",
        "TokenParsers.scala"
      ), targetDir) ++
    copyDir("common-test/src/main/scala/scala/util/parsing/combinator/token", "scala/util/parsing/combinator/token",
      List(
        "StdTokens.scala",
        "Tokens.scala"
      ), targetDir) ++
    copyDir("common-test/src/main/scala/scala/util/parsing/input", "scala/util/parsing/input",
      List(
        "CharArrayReader.scala",
        "CharSequenceReader.scala",
        "NoPosition.scala",
        "OffsetPosition.scala",
        "PagedSeqReader.scala",
        "Position.scala",
        "Positional.scala",
        "Reader.scala",
        "StreamReader.scala",
        "PositionCache.scala"
      ), targetDir)
  }

}
