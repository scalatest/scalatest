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

trait GenerateTests {

  def getJavaHome: Option[File]

  def buildScalaVersion: String

  def crossBuildLibraryDependencies(theScalaVersion: String): Seq[ModuleID]

  def scalatest: Project

  def scalacticMacro: Project

  def commonTest: Project

  def scalatestLibraryDependencies: Seq[ModuleID]

  def scalatestTestOptions: Seq[Tests.Argument]

  def genFiles(name: String, generatorSource: String)(gen: (File, String, String) => Unit)(basedir: File, outDir: File, theVersion: String, theScalaVersion: String): Seq[File]

  def gentestsLibraryDependencies =
    Seq(
      "org.mockito" % "mockito-all" % "1.9.0" % "optional",
      "junit" % "junit" % "4.10" % "optional",
      "org.testng" % "testng" % "6.8.7" % "optional",
      "org.jmock" % "jmock-legacy" % "2.5.1" % "optional",
      "org.pegdown" % "pegdown" % "1.4.2" % "optional"
    )

  def gentestsSharedSettings: Seq[Setting[_]] = Seq(
    javaHome := getJavaHome,
    scalaVersion := buildScalaVersion,
    scalacOptions ++= Seq("-feature"),
    resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public",
    libraryDependencies ++= crossBuildLibraryDependencies(scalaVersion.value),
    libraryDependencies ++= gentestsLibraryDependencies,
    testOptions in Test := Seq(Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/html"))
  )

  val genRegular1 = TaskKey[Unit]("genregular1", "Generate regular tests 1")
  val genRegularTask1 = genRegular1 <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenRegularTests1.genTest(new File(testTargetDir, "scala/genregular1"), theVersion, theScalaVersion)
  }

  val genRegular2 = TaskKey[Unit]("genregular2", "Generate regular tests 2")
  val genRegularTask2 = genRegular2 <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenRegularTests2.genTest(new File(testTargetDir, "scala/genregular2"), theVersion, theScalaVersion)
  }

  val genRegular3 = TaskKey[Unit]("genregular3", "Generate regular tests 3")
  val genRegularTask3 = genRegular3 <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenRegularTests3.genTest(new File(testTargetDir, "scala/genregular3"), theVersion, theScalaVersion)
  }

  val genRegular4 = TaskKey[Unit]("genregular4", "Generate regular tests 4")
  val genRegularTask4 = genRegular4 <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenRegularTests4.genTest(new File(testTargetDir, "scala/genregular4"), theVersion, theScalaVersion)
  }

  val genRegular5 = TaskKey[Unit]("genregular5", "Generate regular tests 5")
  val genRegularTask5 = genRegular5 <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenRegularTests5.genTest(new File(testTargetDir, "scala/genregular5"), theVersion, theScalaVersion)
  }

  val genGenTestsKey = TaskKey[Unit]("gengen", "Generate Property Checks Tests")
  val genGenTestsTask = genGenTestsKey <<= (sourceManaged in Compile, sourceManaged in Test, name, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, projName: String, theVersion: String, theScalaVersion: String) =>
    GenGen.genTest(new File(testTargetDir, "scala/gengen"), theVersion, theScalaVersion)
  }

  val genTablesTestsKey = TaskKey[Unit]("gentables", "Generate Tables Tests")
  val genTablesTestsTask = genTablesTestsKey <<= (sourceManaged in Compile, sourceManaged in Test, name, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, projName: String, theVersion: String, theScalaVersion: String) =>
    GenTable.genTest(new File(testTargetDir, "scala/gentables"), theVersion, theScalaVersion)
  }

  val genMustMatchersTests = TaskKey[Unit]("genmatchers", "Generate Must Matchers Tests")
  val genMustMatchersTestsTask = genMustMatchersTests <<= (sourceManaged in Compile, sourceManaged in Test, name, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, projName: String, theVersion: String, theScalaVersion: String) =>
    projName match {
      case "genMustMatchersTests1" =>
        GenMustMatchersTests1.genTest(new File(testTargetDir, "scala/genmatchers1"), theVersion, theScalaVersion)
      case "genMustMatchersTests2" =>
        GenMustMatchersTests2.genTest(new File(testTargetDir, "scala/genmatchers2"), theVersion, theScalaVersion)
      case "genMustMatchersTests3" =>
        GenMustMatchersTests3.genTest(new File(testTargetDir, "scala/genmatchers3"), theVersion, theScalaVersion)
      case "genMustMatchersTests4" =>
        GenMustMatchersTests4.genTest(new File(testTargetDir, "scala/genmatchers4"), theVersion, theScalaVersion)
    }
  }

  lazy val genRegularTests1 = Project("genRegularTests1", file("gentests/GenRegular1"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genRegularTask1,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genregular1", "GenRegular1.scala")(GenRegularTests1.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genRegularTests2 = Project("genRegularTests2", file("gentests/GenRegular2"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genRegularTask2,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genregular2", "GenRegular2.scala")(GenRegularTests2.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genRegularTests3 = Project("genRegularTests3", file("gentests/GenRegular3"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genRegularTask3,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genregular3", "GenRegular3.scala")(GenRegularTests3.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genRegularTests4 = Project("genRegularTests4", file("gentests/GenRegular4"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genRegularTask4,
      libraryDependencies ++= scalatestLibraryDependencies,
      testOptions in Test := scalatestTestOptions,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genregular4", "GenRegularTests1.scala")(GenRegularTests4.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genRegularTests5 = Project("genRegularTests5", file("gentests/GenRegular5"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genRegularTask5,
      libraryDependencies ++= scalatestLibraryDependencies,
      testOptions in Test := scalatestTestOptions,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genregular5", "GenRegularTests1.scala")(GenRegularTests5.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genGenTests = Project("genGenTests", file("gentests/GenGen"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genGenTestsTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("gengen", "GenGen.scala")(GenGen.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genTablesTests = Project("genTablesTests", file("gentests/GenTables"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genTablesTestsTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("gentables", "GenTable.scala")(GenTable.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genMustMatchersTests1 = Project("genMustMatchersTests1", file("gentests/MustMatchers1"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genMustMatchersTestsTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genmatchers1", "GenMustMatchersTests.scala")(GenMustMatchersTests1.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genMustMatchersTests2 = Project("genMustMatchersTests2", file("gentests/MustMatchers2"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genMustMatchersTestsTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genmatchers2", "GenMustMatchersTests.scala")(GenMustMatchersTests2.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genMustMatchersTests3 = Project("genMustMatchersTests3", file("gentests/MustMatchers3"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genMustMatchersTestsTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genmatchers3", "GenMustMatchersTests.scala")(GenMustMatchersTests3.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genMustMatchersTests4 = Project("genMustMatchersTests4", file("gentests/MustMatchers4"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genMustMatchersTestsTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genmatchers4", "GenMustMatchersTests.scala")(GenMustMatchersTests4.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genInspectorsTests = Project("genInspectorsTests", file("gentests/GenInspectors"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genInspectorsTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("geninspectors", "GenInspectors.scala")(GenInspectors.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genInspectorsShorthandsTests1 = Project("genInspectorsShorthandsTests1", file("gentests/GenInspectorsShorthands1"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genInspectorsShorthandsTask1,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("geninspectorsshorthands1", "GenInspectorsShorthands.scala")(GenInspectorsShorthands1.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genInspectorsShorthandsTests2 = Project("genInspectorsShorthandsTests2", file("gentests/GenInspectorsShorthands2"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genInspectorsShorthandsTask2,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("geninspectorsshorthands2", "GenInspectorsShorthands.scala")(GenInspectorsShorthands2.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genTheyTests = Project("genTheyTests", file("gentests/GenThey"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genTheyWordTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genthey", "GenTheyWord.scala")(GenTheyWord.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genContainTests1 = Project("genContainTests1", file("gentests/GenContain1"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genContainTask1,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("gencontain1", "GenContain1.scala")(GenContain1.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genContainTests2 = Project("genContainTests2", file("gentests/GenContain2"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genContainTask2,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("gencontain2", "GenContain2.scala")(GenContain2.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genSortedTests = Project("genSortedTests", file("gentests/GenSorted"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genSortedTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("gensorted", "GenSorted.scala")(GenSorted.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genLoneElementTests = Project("genLoneElementTests", file("gentests/GenLoneElement"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genLoneElementTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genloneelement", "GenLoneElement.scala")(GenLoneElement.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genEmptyTests = Project("genEmptyTests", file("gentests/GenEmpty"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genEmptyTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genempty", "GenEmpty.scala")(GenEmpty.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genSafeStyleTests = Project("genSafeStyleTests", file("gentests/GenSafeStyles"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genSafeStyleTestsTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("gensafestyletests", "GenSafeStyles.scala")(GenSafeStyles.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val gentests = Project("gentests", file("gentests"))
    .aggregate(genMustMatchersTests1, genMustMatchersTests2, genMustMatchersTests3, genMustMatchersTests4, genGenTests, genTablesTests, genInspectorsTests, genInspectorsShorthandsTests1,
      genInspectorsShorthandsTests2, genTheyTests, genContainTests1, genContainTests2, genSortedTests, genLoneElementTests, genEmptyTests, genSafeStyleTests)

  val genTheyWord = TaskKey[Unit]("genthey", "Generate They Word tests")
  val genTheyWordTask = genTheyWord <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenTheyWord.genTest(new File(testTargetDir, "scala/genthey"), theVersion, theScalaVersion)
  }

  val genInspectors = TaskKey[Unit]("geninspectors", "Generate Inspectors tests")
  val genInspectorsTask = genInspectors <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenInspectors.genTest(new File(testTargetDir, "scala/geninspectors"), theVersion, theScalaVersion)
  }

  val genInspectorsShorthands1 = TaskKey[Unit]("geninspectorsshorthands1", "Generate Inspectors Shorthands tests 1")
  val genInspectorsShorthandsTask1 = genInspectorsShorthands1 <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion) =>
    GenInspectorsShorthands1.genTest(new File(testTargetDir, "scala/geninspectorsshorthands1"), theVersion, theScalaVersion)
  }

  val genInspectorsShorthands2 = TaskKey[Unit]("geninspectorsshorthands2", "Generate Inspectors Shorthands tests 2")
  val genInspectorsShorthandsTask2 = genInspectorsShorthands2 <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion) =>
    GenInspectorsShorthands2.genTest(new File(testTargetDir, "scala/geninspectorsshorthands2"), theVersion, theScalaVersion)
  }

  val genContain1 = TaskKey[Unit]("gencontain1", "Generate contain matcher tests 1")
  val genContainTask1 = genContain1 <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenContain1.genTest(new File(testTargetDir, "scala/gencontain1"), theVersion, theScalaVersion)
  }

  val genContain2 = TaskKey[Unit]("gencontain2", "Generate contain matcher tests 2")
  val genContainTask2 = genContain2 <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenContain2.genTest(new File(testTargetDir, "scala/gencontain2"), theVersion, theScalaVersion)
  }

  val genSorted = TaskKey[Unit]("gensorted", "Generate sorted matcher tests")
  val genSortedTask = genSorted <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenSorted.genTest(new File(testTargetDir, "scala/gensorted"), theVersion, theScalaVersion)
  }

  val genLoneElement = TaskKey[Unit]("genloneelement", "Generate lone element matcher tests")
  val genLoneElementTask = genLoneElement <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenLoneElement.genTest(new File(testTargetDir, "scala/genloneelement"), theVersion, theScalaVersion)
  }

  val genEmpty = TaskKey[Unit]("genempty", "Generate empty matcher tests")
  val genEmptyTask = genEmpty <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenEmpty.genTest(new File(testTargetDir, "scala/genempty"), theVersion, theScalaVersion)
  }

  val genSafeStyleTestsTaskKey = TaskKey[Unit]("gensafestyletests", "Generate Safe Style tests")
  val genSafeStyleTestsTask = genSafeStyleTestsTaskKey <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenSafeStyles.genTest(new File(testTargetDir, "scala/gensafestyles"), theVersion, theScalaVersion)
  }

}