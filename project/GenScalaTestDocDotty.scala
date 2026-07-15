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

object GenScalaTestDocDotty {

  def genScala(targetDir: java.io.File, version: String, scalaVersion: String): Seq[java.io.File] =
    GenScalaTestDoc.copyDir("dotty/core/src/main/scala/org/scalatest", "scala/org/scalatest", targetDir, ".scala") ++
    GenScalaTestDoc.copyDir("dotty/diagrams/src/main/scala/org/scalatest/diagrams", "scala/org/scalatest/diagrams", targetDir, ".scala") ++
    GenScalaTestDoc.copyDir("dotty/expectations/src/main/scala/org/scalatest/expectations", "scala/org/scalatest/expectations", targetDir, ".scala") ++
    GenScalaTestDoc.copyDir("dotty/matchers-core/src/main/scala/org/scalatest/matchers", "scala/org/scalatest/matchers", targetDir, ".scala") ++
    GenScalaTestDoc.copyDir("dotty/shouldmatchers/src/main/scala/org/scalatest/matchers/should", "scala/org/scalatest/matchers/should", targetDir, ".scala") ++
    GenScalaTestDoc.copyDir("dotty/mustmatchers/src/main/scala/org/scalatest/matchers/must", "scala/org/scalatest/matchers/must", targetDir, ".scala")
}