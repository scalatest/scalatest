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

object GenScalacticDocDotty {

  def genScala(targetDir: java.io.File, version: String, scalaVersion: String): Seq[java.io.File] =
    GenScalacticDotty.copyDir("jvm/scalactic/src/main/scala/org/scalactic", "org/scalactic", targetDir,
      List(
        "BooleanMacro.scala", // Re-implemented
        "Requirements.scala", // Re-implemented
        "Snapshots.scala"  // Re-implemented
      )
    ) ++
    GenScalacticDotty.copyDir("jvm/scalactic/src/main/scala/org/scalactic/exceptions", "org/scalactic/exceptions", targetDir, List.empty) ++
    GenScalacticDotty.copyDir("jvm/scalactic/src/main/scala/org/scalactic/source", "org/scalactic/source", targetDir,
      List(
        "Position.scala",  // Re-implemented
        "TypeInfo.scala"  // Re-implemented
      )) ++
    GenScalacticDotty.copyDir("jvm/scalactic/src/main/scala/org/scalactic/anyvals", "org/scalactic/anyvals", targetDir, List.empty) ++
    GenScalacticDotty.copyDir("dotty/scalactic/src/main/scala/org/scalactic", "org/scalactic", targetDir, List.empty) ++
    GenScalacticDotty.copyDir("dotty/scalactic/src/main/scala/org/scalactic/source", "org/scalactic/source", targetDir, List.empty) ++
    GenScalacticDotty.copyDir("dotty/scalactic/src/main/scala/org/scalactic/opaquetypes", "org/scalactic/opaquetypes", targetDir, List.empty)
}