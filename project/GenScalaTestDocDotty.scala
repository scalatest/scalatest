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
    GenScalaTestDotty.copyDir("dotty/core/src/main/scala/org/scalatest", "org/scalatest", targetDir, List.empty) ++
    GenScalaTestDotty.copyDir("dotty/core/src/main/scala/org/scalatest/enablers", "org/scalatest/enablers", targetDir, List.empty) ++
    GenScalaTestDotty.copyDir("dotty/diagrams/src/main/scala/org/scalatest/diagrams", "org/scalatest/diagrams", targetDir, List.empty) ++
    GenScalaTestDotty.copyDir("dotty/expectations/src/main/scala/org/scalatest/expectations", "org/scalatest/expectations", targetDir, List.empty) ++
    GenScalaTestDotty.copyDir("dotty/matchers-core/src/main/scala/org/scalatest/matchers", "org/scalatest/matchers", targetDir, List.empty) ++
    GenScalaTestDotty.copyDir("dotty/matchers-core/src/main/scala/org/scalatest/matchers/dsl", "org/scalatest/matchers/dsl", targetDir, List.empty) ++
    GenScalaTestDotty.copyDir("dotty/shouldmatchers/src/main/scala/org/scalatest/matchers/should", "org/scalatest/matchers/should", targetDir, List.empty) ++
    GenScalaTestDotty.copyDir("dotty/mustmatchers/src/main/scala/org/scalatest/matchers/must", "org/scalatest/matchers/must", targetDir, List.empty)
}