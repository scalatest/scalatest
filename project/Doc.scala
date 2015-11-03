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

import sbt.Keys._
import sbt._
import java.io.PrintWriter

import scala.io.Source

trait Doc {

  def releaseVersion: String

  def scalacticMacro: Project

  val githubTag = "release-3.0.0-M11-for-scala-2.11-and-2.10" // for scaladoc source urls

  val scalatestDocSourceUrl =
    "https://github.com/scalatest/scalatest/tree/"+ githubTag +
      "/scalatest/€{FILE_PATH}.scala"

  val scalacticDocSourceUrl =
    "https://github.com/scalatest/scalatest/tree/"+ githubTag +
      "/scalactic/€{FILE_PATH}.scala"

  lazy val scalatestDocSettings = Seq(
    docsrcDirSetting,
    scalatestDocScalacOptionsSetting
  )

  lazy val scalacticDocSettings = Seq(
    docsrcDirSetting,
    scalacticDocScalacOptionsSetting
  )

  //
  // Prepares source files for running scaladoc.
  //
  def genDocSources(srcFiles: Seq[File],
                    srcDirs: Seq[File],
                    docsrcDir: File): Seq[File] =
  {
    val scalaFiles =
      for {
        srcFile <- srcFiles
        if srcFile.name.endsWith(".scala")
      } yield {
        val srcPath = srcFile.getPath
        val maybeSourceFile = srcDirs.flatMap(srcFile.relativeTo).headOption
        maybeSourceFile match {
          case Some(docsrcFile) => copyDocFile(srcFile, new File(docsrcDir.asFile, docsrcFile.getPath))
          case None             =>
            throw new RuntimeException("unexpected source path ["+ srcPath +"] not relative to " + srcDirs.mkString("[", ", ", "]"))
        }
      }

    val javaSources = srcFiles.filter(_.name.endsWith(".java")).toSet
    val javaTagFiles = JavaTagDocumenter.docJavaTags(javaSources)

    scalaFiles ++ javaTagFiles
  }

  //
  // Copies a file, doing a little filtering along the way to make
  // destination file suitable for use in generating scaladocs.
  //
  // Returns destination file.
  //
  private def copyDocFile(srcFile: File, destFile: File): File = {
    if (!destFile.exists || (destFile.lastModified < srcFile.lastModified)) {
      IO.createDirectory(file(destFile.getParent))

      val writer = new PrintWriter(destFile)

      try {
        for (line <- Source.fromFile(srcFile).getLines)
          writer.println(line.replaceFirst("@Finders(.*)", ""))
      }
      finally { writer.close }
    }
    destFile
  }

  //
  // Adds customization to scaladocs.
  //
  // Appends additional css to template.css file and copies
  // additional gifs into lib directory.
  //
  // Note: found that adding new gifs into lib directory causes
  // doc task to rebuild scaladocs from scratch each time.
  // Without that it only rebuilds if needed.
  //
  def docTask(docDir: File, srcDir: File, projectName: String): File = {
    val docLibDir = docDir / "lib"
    val htmlSrcDir = srcDir / "html"
    val cssFile = docLibDir / "template.css"
    val addlCssFile = htmlSrcDir / "addl.css"

    val css = Source.fromFile(cssFile).mkString
    val addlCss = Source.fromFile(addlCssFile).mkString

    if (!css.contains("pre.stHighlighted")) {
      val writer = new PrintWriter(cssFile)

      try {
        writer.println(css)
        writer.println(addlCss)
      }
      finally { writer.close }
    }

    if (projectName.contains("scalatest")) {
      (htmlSrcDir * "*.gif").get.foreach { gif =>
        IO.copyFile(gif, docLibDir / gif.name)
      }
    }
    docDir
  }

  lazy val projectTitle =
    settingKey[String]("Name of project to display in doc titles")

  lazy val docsrcDir =
    settingKey[File](
      "Directory to hold processed source files for generating scaladocs")

  val docsrcDirSetting =
    docsrcDir := target.value / "docsrc"

  val scalacticDocSourcesSetting =
    sources in (Compile, doc) :=
      genDocSources((sources in Compile).value ++ (sources in scalacticMacro in Compile).value,
        Seq((sourceManaged in Compile).value,
          baseDirectory.value,
          file(".").getCanonicalFile),
        docsrcDir.value)

  val scalatestDocSourcesSetting =
    sources in (Compile, doc) :=
      genDocSources((sources in Compile).value,
        Seq((sourceManaged in Compile).value,
          baseDirectory.value,
          file(".").getCanonicalFile),
        docsrcDir.value)

  val scalatestDocScalacOptionsSetting =
    scalacOptions in (Compile, doc) ++=
      Seq[String](
        "-Ymacro-no-expand", // avoids need to separate out macros in docsrc dir
        "-sourcepath", docsrcDir.value.getAbsolutePath,
        "-doc-title", projectTitle.value +" "+ releaseVersion,
        "-doc-source-url", scalatestDocSourceUrl)

  val scalacticDocScalacOptionsSetting =
    scalacOptions in (Compile, doc) ++=
      Seq[String](
        "-Ymacro-no-expand", // avoids need to separate out macros in docsrc dir
        "-sourcepath", docsrcDir.value.getAbsolutePath,
        "-doc-title", projectTitle.value +" "+ releaseVersion,
        "-doc-source-url", scalacticDocSourceUrl)

  val docTaskSetting =
    doc in Compile := docTask((doc in Compile).value,
      (sourceDirectory in Compile).value,
      name.value)

  val scalatestJSDocTaskSetting =
    doc in Compile := docTask((doc in Compile).value,
      (sourceManaged in Compile).value,
      name.value)
}