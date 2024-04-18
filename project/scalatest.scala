import sbt._
import Keys._
import java.net.{URL, URLClassLoader}
import java.io.PrintWriter
import scala.io.Source
import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.sbt.osgi.SbtOsgi
import com.typesafe.sbt.osgi.SbtOsgi.autoImport._
import com.jsuereth.sbtpgp.SbtPgp.autoImport._

//import sbtcrossproject.CrossPlugin.autoImport._
import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType, _}
import scalajscrossproject.ScalaJSCrossPlugin.autoImport._

import com.typesafe.tools.mima.plugin.MimaKeys.{mimaPreviousArtifacts, mimaCurrentClassfiles, mimaBinaryIssueFilters}
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

import xerial.sbt.Sonatype.autoImport.sonatypePublishToBundle

object ScalatestBuild extends BuildCommons with DottyBuild with NativeBuild with JsBuild {

  // To run gentests
  // rm -rf gentests
  // sbt genGenTests/test  (etc., look at specific failures on CI output)

  // To enable deprecation warnings on the fly
  // set scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")
  // To temporarily switch sbt to a different Scala version:
  // > ++ 2.10.5

  def envVar(name: String): Option[String] =
    try {
      Some(sys.env(name))
    }
    catch {
      case e: NoSuchElementException => None
    }

  def getNexusCredentials: Credentials =
    (envVar("SCALATEST_NEXUS_LOGIN"), envVar("SCALATEST_NEXUS_PASSWORD")) match {
      case (Some(login), Some(password)) => Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", login, password)
      case _ => Credentials(Path.userHome / ".ivy2" / ".credentials")
    }

  def getJavaHome(scalaMajorVersion: String): Option[File] = {
    val javaHome = new File(System.getProperty("java.home"))
    val javaHomeBin = new File(javaHome, "bin")
    val javac = new File(javaHomeBin, "javac")
    val javacExe = new File(javaHomeBin, "javac.exe")
    if (javac.exists || javacExe.exists)
      Some(file(javaHome.getAbsolutePath))
    else {
      val javaHomeParentBin = new File(javaHome.getParent, "bin")
      val parentJavac = new File(javaHomeParentBin, "javac")
      val parentJavacExe = new File(javaHomeParentBin, "javac.exe")
      if (parentJavac.exists || parentJavacExe.exists)
        Some(file(javaHome.getParentFile.getAbsolutePath))
      else
        println("WARNING: javac from java.home not found, javac on PATH will be used.  Try to use JDK instead of JRE to launch SBT to remove this warning.")
      None
    }
  }

  def commonSharedSettings: Seq[Setting[_]] = Seq(
    javaHome := getJavaHome(scalaBinaryVersion.value),
    version := releaseVersion,
    resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public",
    publishTo := sonatypePublishToBundle.value, 
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false },
    pomExtra := (
      <url>http://www.scalatest.org</url>
        <licenses>
          <license>
            <name>the Apache License, ASL Version 2.0</name>
            <url>http://www.apache.org/licenses/LICENSE-2.0</url>
            <distribution>repo</distribution>
          </license>
        </licenses>
        <scm>
          <url>https://github.com/scalatest/scalatest</url>
          <connection>scm:git:git@github.com:scalatest/scalatest.git</connection>
          <developerConnection>
            scm:git:git@github.com:scalatest/scalatest.git
          </developerConnection>
        </scm>
        <developers>
          <developer>
            <id>bvenners</id>
            <name>Bill Venners</name>
            <email>bill@artima.com</email>
          </developer>
          <developer>
            <id>gcberger</id>
            <name>George Berger</name>
            <email>george.berger@gmail.com</email>
          </developer>
          <developer>
            <id>cheeseng</id>
            <name>Chua Chee Seng</name>
            <email>cheeseng@amaseng.com</email>
          </developer>
        </developers>
      ),
    credentials += getNexusCredentials,
  )

  def sharedSettings: Seq[Setting[_]] = 
    commonSharedSettings ++ scalaVersionsSettings ++ Seq(
      libraryDependencies ++= {
        if (scalaVersion.value.startsWith("3."))
          Seq()
        else
          scalaLibraries(scalaVersion.value),
      }
    )

  lazy val scalatestDocSettings = Seq(
    docsrcDirSetting,
    scalatestDocScalacOptionsSetting
  )

  lazy val scalacticDocSettings = Seq(
    docsrcDirSetting,
    scalacticDocScalacOptionsSetting
  )

  def scalaXmlDependency(theScalaVersion: String): Seq[ModuleID] =
    CrossVersion.partialVersion(theScalaVersion) match {
      case Some((2, 10)) => Seq.empty
      case Some((2, 11)) => Seq(("org.scala-lang.modules" %% "scala-xml" % "1.3.0"))
      case Some((scalaEpoch, scalaMajor)) if (scalaEpoch == 2 && scalaMajor >= 12) || scalaEpoch == 3 =>
        Seq(("org.scala-lang.modules" %% "scala-xml" % "2.3.0"))
    }

  def scalaLibraries(theScalaVersion: String) =
    Seq(
      "org.scala-lang" % "scala-compiler" % theScalaVersion % "provided",
      "org.scala-lang" % "scala-reflect" % theScalaVersion // this is needed to compile macro
    )

  def scalatestLibraryDependencies =
    Seq(
      "org.scala-sbt" % "test-interface" % "1.0" % "optional",
      "com.google.inject" % "guice" % "4.0" % "optional",
      "org.apache.ant" % "ant" % "1.10.12" % "optional",
      "org.ow2.asm" % "asm-all" % "4.1" % "optional",
      flexmarkAll
    )

  def crossBuildTestLibraryDependencies = Def.setting {
    CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, scalaMajor)) if scalaMajor == 10 =>
        Seq.empty

      case _ => // for scala 2.11, 2.12, 2.13 and 3.x
        Seq(
          "org.scala-lang.modules" %%% "scala-parser-combinators" % "2.4.0"
        )
    }
  }

  val flexmarkAll = "com.vladsch.flexmark" % "flexmark-all" % flexmarkVersion % "optional"

  def scalatestTestLibraryDependencies(theScalaVersion: String) =
    Seq(
      "org.scalatestplus" %% "testng-6-7" % plusTestNGVersion % "test",
      "org.scalatestplus" %% "junit-4-13" % plusJUnitVersion % "test"
    )

  lazy val commonTest = Project("common-test", file("jvm/common-test"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Common test classes used by scalactic and scalatest",
      libraryDependencies ++= crossBuildTestLibraryDependencies.value,
      sourceGenerators in Compile += {
        Def.task{
          GenGen.genMain((sourceManaged in Compile).value / "org" / "scalatest" / "prop", version.value, scalaVersion.value) ++
          GenCompatibleClasses.genTest((sourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      scalacOptions in (Compile, doc) := List.empty
    ).dependsOn(scalacticMacro, LocalProject("scalatest"))

  lazy val scalacticMacro = project.in(file("jvm/scalactic-macro"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Scalactic Macro",
      organization := "org.scalactic",
      sourceGenerators in Compile += {
        Def.task{
          ScalacticGenResourcesJVM.genResources((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenAnyVals.genMain((sourceManaged in Compile).value / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value, false) ++
          GenEvery.genMain((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenColCompatHelper.genMain((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++ 
          GenMacroContext.genMain((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value)
        }.taskValue
      },
      // Disable publishing macros directly, included in scalactic main jar
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      scalacOptions in (Compile, doc) := List.empty
    )

  lazy val scalactic = project.in(file("jvm/scalactic"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalacticDocSettings: _*)
    .settings(
      projectTitle := "Scalactic",
      organization := "org.scalactic",
      initialCommands in console := "import org.scalactic._",
      sourceGenerators in Compile += {
        Def.task{
          GenVersions.genScalacticVersions((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          ScalacticGenResourcesJVM.genFailureMessages((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenArrayHelper.genMain((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value)
        }.taskValue
      },
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10") Seq.empty[String] else if (scalaVersion.value.startsWith("2.13")) Seq.empty else Seq("-Ypartial-unification")),
      // include the macro classes and resources in the main jar
      mappings in (Compile, packageBin) ++= mappings.in(scalacticMacro, Compile, packageBin).value,
      // include the macro sources in the main source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalacticMacro, Compile, packageSrc).value,
      sources in (Compile, doc) :=
        genDocSources((sources in Compile).value ++ (sources in scalacticMacro in Compile).value,
          Seq((sourceManaged in Compile).value,
            baseDirectory.value,
            file(".").getCanonicalFile),
          docsrcDir.value), 
      docTaskSetting,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar"), 
      mimaBinaryIssueFilters ++= 
        Seq(
          exclude[DirectMissingMethodProblem]("org.scalactic.AndBool.failureMessageArgs"), // Private class function
          exclude[DirectMissingMethodProblem]("org.scalactic.AndBool.this"), // Private class function
          exclude[DirectMissingMethodProblem]("org.scalactic.OrBool.negatedFailureMessageArgs"), // Private class function
          exclude[DirectMissingMethodProblem]("org.scalactic.OrBool.this") // Private class function
        )
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalactic",
        "org.scalactic.anyvals",
        "org.scalactic.exceptions",
        "org.scalactic.source"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "org.scalactic.*",
        "scala.util.parsing.*;version=\"$<range;[==,=+);$<replace;1.0.4;-;.>>\"",
        "scala.xml.*;version=\"$<range;[==,=+);$<replace;1.0.4;-;.>>\"",
        "scala.*;version=\"$<range;[==,=+);$<replace;"+scalaBinaryVersion.value+";-;.>>\"",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "Scalactic",
        "Bundle-Description" -> "Scalactic is an open-source library for Scala projects.",
        "Bundle-DocURL" -> "http://www.scalactic.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalacticMacro % "compile-internal, test-internal")  // avoid dependency in pom on non-existent scalactic-macro artifact, per discussion in http://grokbase.com/t/gg/simple-build-tool/133shekp07/sbt-avoid-dependence-in-a-macro-based-project

  lazy val scalacticTest = Project("scalactic-test", file("jvm/scalactic-test"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Scalactic Test",
      organization := "org.scalactic",
      testOptions in Test ++=
        Seq(Tests.Argument(TestFrameworks.ScalaTest,
          "-oDIF",
          "-W", "120", "60")),
      logBuffered in Test := false,
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      sourceGenerators in Test += Def.task {
        GenAnyVals.genTest((sourceManaged in Test).value / "scala" / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value)
      }.taskValue,
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification"))
    ).dependsOn(scalactic, scalatest % "test", commonTest % "test")

  def sharedTestSettings: Seq[Setting[_]] = 
    Seq(
      organization := "org.scalatest",
      libraryDependencies ++= scalatestLibraryDependencies,
      libraryDependencies ++= scalatestTestLibraryDependencies(scalaVersion.value),
      testOptions in Test := scalatestTestOptions,
      logBuffered in Test := false,
      baseDirectory in Test := file("./"),
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification"))
    )

  lazy val scalatestTest = Project("scalatest-test", file("jvm/scalatest-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettings: _*)
    .settings(
      projectTitle := "ScalaTest Test"
    ).dependsOn(commonTest % "test")
     .aggregate(
       scalatestDiagramsTest, 
       scalatestFeatureSpecTest, 
       scalatestFlatSpecTest, 
       scalatestFreeSpecTest, 
       scalatestFunSpecTest, 
       scalatestFunSuiteTest, 
       scalatestPropSpecTest, 
       scalatestWordSpecTest
     )

  lazy val scalatestDiagramsTest = project.in(file("jvm/diagrams-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettings: _*)
    .settings(
      projectTitle := "ScalaTest Diagrams Test"
    ).dependsOn(commonTest % "test")

  lazy val scalatestFeatureSpecTest = project.in(file("jvm/featurespec-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettings: _*)
    .settings(
      projectTitle := "ScalaTest FeatureSpec Test"
    ).dependsOn(commonTest % "test")

  lazy val scalatestFlatSpecTest = project.in(file("jvm/flatspec-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettings: _*)
    .settings(
      projectTitle := "ScalaTest FlatSpec Test"
    ).dependsOn(commonTest % "test")

  lazy val scalatestFreeSpecTest = project.in(file("jvm/freespec-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettings: _*)
    .settings(
      projectTitle := "ScalaTest FreeSpec Test"
    ).dependsOn(commonTest % "test")

  lazy val scalatestFunSpecTest = project.in(file("jvm/funspec-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettings: _*)
    .settings(
      projectTitle := "ScalaTest FunSpec Test"
    ).dependsOn(commonTest % "test")

  lazy val scalatestFunSuiteTest = project.in(file("jvm/funsuite-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettings: _*)
    .settings(
      projectTitle := "ScalaTest FunSuite Test"
    ).dependsOn(commonTest % "test")  

  lazy val scalatestPropSpecTest = project.in(file("jvm/propspec-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettings: _*)
    .settings(
      projectTitle := "ScalaTest PropSpec Test"
    ).dependsOn(commonTest % "test")

  lazy val scalatestWordSpecTest = project.in(file("jvm/wordspec-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettings: _*)
    .settings(
      projectTitle := "ScalaTest WordSpec Test"
    ).dependsOn(commonTest % "test")            

  lazy val scalatestApp = project.in(file("scalatest-app"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest App",
      name := "scalatest-app",
      organization := "org.scalatest",
      libraryDependencies ++= scalatestLibraryDependencies,
      // include the scalactic classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalactic, Compile, packageBin).value,
      // include the scalactic sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalactic, Compile, packageSrc).value,
      // include the scalatest classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatest, Compile, packageBin).value,
      // include the scalatest sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatest, Compile, packageSrc).value,
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      scalatestDocSettings,
      unmanagedResourceDirectories in Compile += baseDirectory.value / "scalatest" / "src" / "main" / "resources",
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "images",
        "org.scalatest",
        "org.scalatest.compatible",
        "org.scalatest.concurrent",
        "org.scalatest.diagrams",
        "org.scalatest.enablers",
        "org.scalatest.events",
        "org.scalatest.exceptions",
        "org.scalatest.expectations",
        "org.scalatest.fixture",
        "org.scalatest.funsuite",
        "org.scalatest.featurespec",
        "org.scalatest.funspec",
        "org.scalatest.freespec",
        "org.scalatest.flatspec",
        "org.scalatest.matchers",
        "org.scalatest.matchers.should",
        "org.scalatest.matchers.must",
        "org.scalatest.matchers.dsl",
        "org.scalatest.verbs",
        "org.scalatest.path",
        "org.scalatest.prop",
        "org.scalatest.propspec",
        "org.scalatest.refspec",
        "org.scalatest.tags",
        "org.scalatest.tagobjects",
        "org.scalatest.time",
        "org.scalatest.tools",
        "org.scalatest.verb",
        "org.scalatest.words",
        "org.scalatest.wordspec",
        "org.scalactic",
        "org.scalactic.anyvals",
        "org.scalactic.exceptions",
        "org.scalactic.source"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "org.scalactic.*",
        "scala.util.parsing.*;version=\"$<range;[==,=+);$<replace;1.0.4;-;.>>\"",
        "scala.xml.*;version=\"$<range;[==,=+);$<replace;1.0.4;-;.>>\"",
        "scala.*;version=\"$<range;[==,=+);$<replace;"+scalaBinaryVersion.value+";-;.>>\"",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc.",
        "Main-Class" -> "org.scalatest.tools.Runner"
      )
    ).dependsOn(
        scalacticMacro % "compile-internal, test-internal", 
        scalactic % "compile-internal", 
        scalatestCompatible % "compile-internal", 
        scalatestCore % "compile-internal", 
        scalatestFeatureSpec % "compile-internal", 
        scalatestFlatSpec % "compile-internal", 
        scalatestFreeSpec % "compile-internal", 
        scalatestFunSuite % "compile-internal", 
        scalatestFunSpec % "compile-internal", 
        scalatestPropSpec % "compile-internal", 
        scalatestWordSpec % "compile-internal", 
        scalatestDiagrams % "compile-internal", 
        scalatestMatchersCore % "compile-internal", 
        scalatestShouldMatchers % "compile-internal", 
        scalatestMustMatchers % "compile-internal")

  lazy val scalatestDoc = project.in(file("scalatest-doc"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Doc",
      name := "scalatest-doc",
      organization := "org.scalatest",
      libraryDependencies ++= scalatestLibraryDependencies,
      libraryDependencies ++= scalaXmlDependency(scalaVersion.value),
      javaSourceManaged := target.value / "java",
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          GenScalaTestDoc.genScala((sourceManaged in Compile).value, version.value, scalaVersion.value) ++ 
          GenScalaTestDoc.genJava((javaSourceManaged in Compile).value, version.value, scalaVersion.value) ++ 
          GenTable.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
          GenConfigMap.genMain((sourceManaged in Compile).value, version.value, scalaVersion.value) ++ 
          ScalaTestGenResourcesJVM.genResources((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
          ScalaTestGenResourcesJVM.genFailureMessages((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++ 
          GenVersions.genScalaTestVersions((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++ 
          GenCompatibleClasses.genMain((sourceManaged in Compile).value / "org" / "scalatest" / "tools", version.value, scalaVersion.value) ++ 
          GenFactories.genMain((sourceManaged in Compile).value / "org" / "scalatest" / "matchers" / "dsl", version.value, scalaVersion.value) ++ 
          GenMatchers.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      },
      sources in (Compile, doc) :=
        genDocSources((sources in Compile).value,
                       Seq((sourceManaged in Compile).value,
                           (scalaSource in Compile).value, 
                           (javaSource in Compile).value),
                       docsrcDir.value), 
      scalatestDocSettings,
      docTaskSetting,
      unmanagedResourceDirectories in Compile += baseDirectory.value / "scalatest" / "src" / "main" / "resources",
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).dependsOn(
      scalacticMacro, 
      scalactic
    )      

  lazy val rootProject = Project("root", file("."))
                         .aggregate(
                           scalacticMacro, 
                           scalactic, 
                           scalatest, 
                           scalatestCore, 
                           scalatestFeatureSpec, 
                           scalatestFlatSpec, 
                           scalatestFreeSpec, 
                           scalatestFunSuite, 
                           scalatestFunSpec, 
                           scalatestPropSpec, 
                           scalatestWordSpec, 
                           scalatestDiagrams, 
                           scalatestMatchersCore, 
                           scalatestShouldMatchers, 
                           scalatestMustMatchers, 
                           commonTest, 
                           scalacticTest, 
                           scalatestTest
                         )

  lazy val scalatestCompatible = project.in(file("jvm/compatible"))
    .enablePlugins(SbtOsgi)
    .settings(commonSharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Compatible",
      name := "scalatest-compatible",
      organization := "org.scalatest",
      javaSourceManaged := target.value / "java",
      autoScalaLibrary := false, 
      crossPaths := false, // disable using the Scala version in output paths and artifacts
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      scalatestDocSettings,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.compatible"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalactic.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest Compatible",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    )

  lazy val scalatestCore = project.in(file("jvm/core"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Core",
      name := "scalatest-core",
      organization := "org.scalatest",
      libraryDependencies ++= scalaXmlDependency(scalaVersion.value),
      libraryDependencies ++= scalatestLibraryDependencies,
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      sourceGenerators in Compile += {
       Def.task{
         GenTable.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
         GenConfigMap.genMain((sourceManaged in Compile).value, version.value, scalaVersion.value) ++ 
         ScalaTestGenResourcesJVM.genResources((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
         ScalaTestGenResourcesJVM.genFailureMessages((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++ 
         GenVersions.genScalaTestVersions((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++ 
         GenCompatibleClasses.genMain((sourceManaged in Compile).value / "org" / "scalatest" / "tools", version.value, scalaVersion.value)
       }.taskValue
      },
      scalatestDocSettings,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar"), 
      mimaBinaryIssueFilters ++= {
        Seq(
          exclude[DirectMissingMethodProblem]("org.scalatest.FailureMessages.cannotLoadDiscoveredSuite"), // Private class function
          exclude[DirectMissingMethodProblem]("org.scalatest.Resources.cannotLoadDiscoveredSuite") // Private class function
        )
      }
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest", 
        "org.scalatest.concurrent",  
        "org.scalatest.enablers",  
        "org.scalatest.exceptions",  
        "org.scalatest.events", 
        "org.scalatest.fixture",  
        "org.scalatest.prop", 
        "org.scalatest.tags", 
        "org.scalatest.tagobjects", 
        "org.scalatest.time", 
        "org.scalatest.tools",  
        "org.scalatest.verbs"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest Core",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc.",
        "Main-Class" -> "org.scalatest.tools.Runner"
      )
    ).dependsOn(scalatestCompatible, scalacticMacro % "compile-internal, test-internal", scalactic)  

  lazy val scalatestFeatureSpec = project.in(file("jvm/featurespec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest FeatureSpec",
      name := "scalatest-featurespec",
      organization := "org.scalatest",
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      scalatestDocSettings,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.featurespec"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest FeatureSpec",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCore, scalacticMacro % "compile-internal, test-internal")

  lazy val scalatestFlatSpec = project.in(file("jvm/flatspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest FlatSpec",
      name := "scalatest-flatspec",
      organization := "org.scalatest",
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      scalatestDocSettings,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.flatspec"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest FlatSpec",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCore, scalacticMacro % "compile-internal, test-internal")

  lazy val scalatestFreeSpec = project.in(file("jvm/freespec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest FreeSpec",
      name := "scalatest-freespec",
      organization := "org.scalatest",
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      scalatestDocSettings,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.freespec"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest FreeSpec",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCore, scalacticMacro % "compile-internal, test-internal")

  lazy val scalatestFunSuite = project.in(file("jvm/funsuite"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest FunSuite",
      name := "scalatest-funsuite",
      organization := "org.scalatest",
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      scalatestDocSettings,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.funsuite"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest FunSuite",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCore, scalacticMacro % "compile-internal, test-internal") 

  lazy val scalatestFunSpec = project.in(file("jvm/funspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest FunSpec",
      name := "scalatest-funspec",
      organization := "org.scalatest",
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      scalatestDocSettings,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.funspec"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest FunSpec",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCore, scalacticMacro % "compile-internal, test-internal")     

  lazy val scalatestPropSpec = project.in(file("jvm/propspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest PropSpec",
      name := "scalatest-propspec",
      organization := "org.scalatest",
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      scalatestDocSettings,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.propspec"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest PropSpec",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCore, scalacticMacro % "compile-internal, test-internal")

  lazy val scalatestRefSpec = project.in(file("jvm/refspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest RefSpec",
      name := "scalatest-refspec",
      organization := "org.scalatest",
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      scalatestDocSettings,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.refspec"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest RefSpec",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCore, scalacticMacro % "compile-internal, test-internal") 

  lazy val scalatestWordSpec = project.in(file("jvm/wordspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest WordSpec",
      name := "scalatest-wordspec",
      organization := "org.scalatest",
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      scalatestDocSettings,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.wordspec"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest WordSpec",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCore, scalacticMacro % "compile-internal, test-internal")

  lazy val scalatestDiagrams = project.in(file("jvm/diagrams"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Diagrams",
      name := "scalatest-diagrams",
      organization := "org.scalatest",
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      scalatestDocSettings,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.diagrams"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest Diagrams",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCore, scalacticMacro % "compile-internal, test-internal")

  lazy val scalatestMatchersCore = project.in(file("jvm/matchers-core"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Matchers Core",
      name := "scalatest-matchers-core",
      organization := "org.scalatest",
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      sourceGenerators in Compile += {
       Def.task{
         GenFactories.genMain((sourceManaged in Compile).value / "org" / "scalatest" / "matchers" / "dsl", version.value, scalaVersion.value)
       }.taskValue
      },
      scalatestDocSettings,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.matchers", 
        "org.scalatest.matchers.dsl"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest Matchers Core",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCore, scalacticMacro % "compile-internal, test-internal") 

  lazy val scalatestShouldMatchers = project.in(file("jvm/shouldmatchers"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Should Matchers",
      name := "scalatest-shouldmatchers",
      organization := "org.scalatest",
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      scalatestDocSettings,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.matchers.should"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest Should Matchers",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestMatchersCore, scalacticMacro % "compile-internal, test-internal")

  lazy val scalatestMustMatchers = project.in(file("jvm/scalatest-mustmatchers"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Must Matchers",
      name := "scalatest-mustmatchers",
      organization := "org.scalatest",
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      sourceGenerators in Compile += {
       Def.task{
         GenMatchers.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
       }.taskValue
      },
      scalatestDocSettings,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.matchers.must"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest Must Matchers",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestMatchersCore, scalacticMacro % "compile-internal, test-internal")     

  lazy val scalatestModules = (project in file("modules/jvm/modules-aggregation"))
    .settings(sharedSettings: _*)
    .settings(
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      scalacOptions in (Compile, doc) := List.empty
    ).aggregate(
      scalatestCore, 
      scalatestFeatureSpec, 
      scalatestFlatSpec, 
      scalatestFreeSpec, 
      scalatestFunSuite, 
      scalatestFunSpec, 
      scalatestPropSpec, 
      scalatestRefSpec, 
      scalatestWordSpec, 
      scalatestDiagrams, 
      scalatestMatchersCore, 
      scalatestShouldMatchers, 
      scalatestMustMatchers
    )

  lazy val scalatest = project.in(file("jvm/scalatest"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest",
      name := "scalatest",
      organization := "org.scalatest",
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      scalatestDocSettings,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.privatePackage := Seq.empty, 
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(
      scalacticMacro % "compile-internal, test-internal", 
      scalatestCore, 
      scalatestFeatureSpec, 
      scalatestFlatSpec, 
      scalatestFreeSpec, 
      scalatestFunSuite, 
      scalatestFunSpec, 
      scalatestPropSpec, 
      scalatestRefSpec, 
      scalatestWordSpec, 
      scalatestDiagrams, 
      scalatestMatchersCore, 
      scalatestShouldMatchers, 
      scalatestMustMatchers
    ).aggregate(
      scalatestCore, 
      scalatestFeatureSpec, 
      scalatestFlatSpec, 
      scalatestFreeSpec, 
      scalatestFunSuite, 
      scalatestFunSpec, 
      scalatestPropSpec, 
      scalatestWordSpec, 
      scalatestRefSpec, 
      scalatestDiagrams, 
      scalatestMatchersCore, 
      scalatestShouldMatchers, 
      scalatestMustMatchers
    )          

  def gentestsLibraryDependencies =
    Seq(
      flexmarkAll,
      "org.scalatestplus" %% "testng-6-7" % plusTestNGVersion % "test",
      "org.scalatestplus" %% "junit-4-13" % plusJUnitVersion % "test"
    )

  def gentestsSharedSettings: Seq[Setting[_]] = scalaVersionsSettings ++ Seq(
    javaHome := getJavaHome(scalaBinaryVersion.value),
    scalacOptions ++= Seq("-feature") ++ (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty else Seq("-Ypartial-unification")),
    resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public",
    libraryDependencies ++= gentestsLibraryDependencies,
    testOptions in Test := Seq(Tests.Argument(TestFrameworks.ScalaTest, "-h", "target/html"))
  )

  lazy val genRegularTests1 = project.in(file("gentests/GenRegular1"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genRegularTask1,
      sourceGenerators in Test += {
        Def.task{
          GenRegularTests1.genTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genRegularTests2 = project.in(file("gentests/GenRegular2"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genRegularTask2,
      sourceGenerators in Test += {
        Def.task{
          GenRegularTests2.genTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genRegularTests3 = project.in(file("gentests/GenRegular3"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genRegularTask3,
      sourceGenerators in Test += {
        Def.task{
          GenRegularTests3.genTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genRegularTests4 = project.in(file("gentests/GenRegular4"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genRegularTask4,
      libraryDependencies ++= scalatestLibraryDependencies,
      testOptions in Test := scalatestTestOptions,
      javaSourceManaged := target.value / "java",
      managedSourceDirectories in Test += javaSourceManaged.value,
      sourceGenerators in Test += {
        Def.task{
          GenRegularTests4.genJava((javaSourceManaged in Compile).value)
        }.taskValue
      },
      sourceGenerators in Test += {
        Def.task{
          GenRegularTests4.genTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genRegularTests5 = project.in(file("gentests/GenRegular5"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genRegularTask5,
      libraryDependencies ++= scalatestLibraryDependencies,
      libraryDependencies ++= gentestsLibraryDependencies,
      testOptions in Test := scalatestTestOptions,
      javaSourceManaged := target.value / "java",
      managedSourceDirectories in Test += javaSourceManaged.value,
      sourceGenerators in Test += {
        Def.task{
          GenRegularTests5.genJava((javaSourceManaged in Compile).value)
        }.taskValue
      },
      sourceGenerators in Test += {
        Def.task{
          GenRegularTests5.genTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genMustMatchersTests1 = project.in(file("gentests/MustMatchers1"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genMustMatchersTask,
      sourceGenerators in Test += {
        Def.task{
          GenMustMatchersTests1.genTest((sourceManaged in Test).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genMustMatchersTests2 = project.in(file("gentests/MustMatchers2"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genMustMatchersTask,
      sourceGenerators in Test += {
        Def.task{
          GenMustMatchersTests2.genTest((sourceManaged in Test).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genMustMatchersTests3 = project.in(file("gentests/MustMatchers3"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genMustMatchersTask,
      sourceGenerators in Test += {
        Def.task{
          GenMustMatchersTests3.genTest((sourceManaged in Test).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genMustMatchersTests4 = project.in(file("gentests/MustMatchers4"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genMustMatchersTask,
      sourceGenerators in Test += {
        Def.task{
          GenMustMatchersTests4.genTest((sourceManaged in Test).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genGenTests = project.in(file("gentests/GenGen"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genGenTask,
      sourceGenerators in Test += {
        Def.task{
          GenGen.genTest((sourceManaged in Test).value / "org" / "scalatest" / "prop", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genTablesTests = project.in(file("gentests/GenTables"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genTablesTask,
      sourceGenerators in Test += {
        Def.task{
          GenTable.genTest((sourceManaged in Test).value / "org" / "scalatest" / "prop", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genInspectorsTests = project.in(file("gentests/GenInspectors"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genInspectorsTask,
      sourceGenerators in Test += {
        Def.task{
          GenInspectors.genTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genInspectorsShorthandsTests1 = project.in(file("gentests/GenInspectorsShorthands1"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genInspectorsShorthandsTask1,
      sourceGenerators in Test += {
        Def.task{
          GenInspectorsShorthands1.genTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genInspectorsShorthandsTests2 = project.in(file("gentests/GenInspectorsShorthands2"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genInspectorsShorthandsTask2,
      sourceGenerators in Test += {
        Def.task{
          GenInspectorsShorthands2.genTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genTheyTests = project.in(file("gentests/GenThey"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genTheyWordTask,
      sourceGenerators in Test += {
        Def.task{
          GenTheyWord.genTest((sourceManaged in Test).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genContainTests1 = project.in(file("gentests/GenContain1"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genContainTask1,
      sourceGenerators in Test += {
        Def.task{
          GenContain1.genTest((sourceManaged in Test).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genContainTests2 = project.in(file("gentests/GenContain2"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genContainTask2,
      sourceGenerators in Test += {
        Def.task{
          GenContain2.genTest((sourceManaged in Test).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genSortedTests = project.in(file("gentests/GenSorted"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genSortedTask,
      sourceGenerators in Test += {
        Def.task{
          GenSorted.genTest((sourceManaged in Test).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genLoneElementTests = project.in(file("gentests/GenLoneElement"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genLoneElementTask,
      sourceGenerators in Test += {
        Def.task{
          GenLoneElement.genTest((sourceManaged in Test).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genEmptyTests = project.in(file("gentests/GenEmpty"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genEmptyTask,
      sourceGenerators in Test += {
        Def.task{
          GenEmpty.genTest((sourceManaged in Test).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  /*lazy val genSafeStyleTests = project.in(file("gentests/GenSafeStyles"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genSafeStyleTestsTask,
      sourceGenerators in Test += {
        Def.task{
          GenSafeStyles.genTest((sourceManaged in Test).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")*/

  lazy val gentests = project.in(file("gentests"))
    .aggregate(genMustMatchersTests1, genMustMatchersTests2, genMustMatchersTests3, genMustMatchersTests4, genGenTests, genTablesTests, genInspectorsTests, genInspectorsShorthandsTests1,
               genInspectorsShorthandsTests2, genTheyTests, genContainTests1, genContainTests2, genSortedTests, genLoneElementTests, genEmptyTests/*, genSafeStyleTests*/)

  lazy val examples = project.in(file("examples"))
    .settings(
      scalaVersionsSettings
    ).dependsOn(scalacticMacro, scalactic, scalatest)

  def genFiles(name: String, generatorSource: String)(gen: (File, String, String) => Unit)(basedir: File, outDir: File, theVersion: String, theScalaVersion: String): Seq[File] = {
    val tdir = outDir / "scala" / name
    val genSource = basedir / "project" / generatorSource

    def results = (tdir ** "*.scala").get
    if (results.isEmpty || results.exists(_.lastModified < genSource.lastModified)) {
      tdir.mkdirs()
      gen(tdir, theVersion, theScalaVersion)
    }
    results
  }

  def genJavaFiles(name: String, generatorSource: String)(gen: (File, String, String) => Unit)(basedir: File, outDir: File, theVersion: String, theScalaVersion: String): Seq[File] = {
    val tdir = outDir / "java" / name
    val genSource = basedir / "project" / generatorSource

    def results = (tdir ** "*.java").get
    if (results.isEmpty || results.exists(_.lastModified < genSource.lastModified)) {
      tdir.mkdirs()
      gen(tdir, theVersion, theScalaVersion)
    }
    results
  }

  val genRegular1 = TaskKey[Unit]("genregular1", "Generate regular tests 1")
  val genRegularTask1 = genRegular1 := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenRegularTests1.genTest(new File(testTargetDir, "scala/genregular1"), theVersion, theScalaVersion)
  }

  val genRegular2 = TaskKey[Unit]("genregular2", "Generate regular tests 2")
  val genRegularTask2 = genRegular2 := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenRegularTests2.genTest(new File(testTargetDir, "scala/genregular2"), theVersion, theScalaVersion)
  }

  val genRegular3 = TaskKey[Unit]("genregular3", "Generate regular tests 3")
  val genRegularTask3 = genRegular3 := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenRegularTests3.genTest(new File(testTargetDir, "scala/genregular3"), theVersion, theScalaVersion)
  }

  val genRegular4 = TaskKey[Unit]("genregular4", "Generate regular tests 4")
  val genRegularTask4 = genRegular4 := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenRegularTests4.genTest(new File(testTargetDir, "scala/genregular4"), theVersion, theScalaVersion)
  }

  val genRegular5 = TaskKey[Unit]("genregular5", "Generate regular tests 5")
  val genRegularTask5 = genRegular5 := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenRegularTests5.genTest(new File(testTargetDir, "scala/genregular5"), theVersion, theScalaVersion)
  }

  val genMustMatchers = TaskKey[Unit]("genmatchers", "Generate Must Matchers")
  val genMustMatchersTask = genMustMatchers := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val projName = name.value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    projName match {
      case "scalatest" =>
        GenMatchers.genMain(new File(mainTargetDir, "scala/genmatchers"), theVersion, theScalaVersion)
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
  val genGen = TaskKey[Unit]("gengen", "Generate Property Checks")
  val genGenTask = genGen := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val projName = name.value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    projName match {
      case "scalatest" =>
        GenGen.genMain(new File(mainTargetDir, "scala/gengen"), theVersion, theScalaVersion)
      case "gentests" =>
        GenGen.genTest(new File(testTargetDir, "scala/gengen"), theVersion, theScalaVersion)
    }
  }

  val genTables = TaskKey[Unit]("gentables", "Generate Tables")
  val genTablesTask = genTables := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val projName = name.value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    projName match {
      case "scalatest" =>
        GenTable.genMain(new File(mainTargetDir, "scala/gentables"), theVersion, theScalaVersion)
      case "gentests" =>
        GenTable.genTest(new File(testTargetDir, "scala/gentables"), theVersion, theScalaVersion)
    }
  }

  val genTheyWord = TaskKey[Unit]("genthey", "Generate They Word tests")
  val genTheyWordTask = genTheyWord := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenTheyWord.genTest(new File(testTargetDir, "scala/genthey"), theVersion, theScalaVersion)
  }

  val genInspectors = TaskKey[Unit]("geninspectors", "Generate Inspectors tests")
  val genInspectorsTask = genInspectors := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenInspectors.genTest(new File(testTargetDir, "scala/geninspectors"), theVersion, theScalaVersion)
  }

  val genInspectorsShorthands1 = TaskKey[Unit]("geninspectorsshorthands1", "Generate Inspectors Shorthands tests 1")
  val genInspectorsShorthandsTask1 = genInspectorsShorthands1 := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenInspectorsShorthands1.genTest(new File(testTargetDir, "scala/geninspectorsshorthands1"), theVersion, theScalaVersion)
  }

  val genInspectorsShorthands2 = TaskKey[Unit]("geninspectorsshorthands2", "Generate Inspectors Shorthands tests 2")
  val genInspectorsShorthandsTask2 = genInspectorsShorthands2 := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenInspectorsShorthands2.genTest(new File(testTargetDir, "scala/geninspectorsshorthands2"), theVersion, theScalaVersion)
  }

  val genFactories = TaskKey[Unit]("genfactories", "Generate Matcher Factories")
  val genFactoriesTask = genFactories := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenFactories.genMain(new File(mainTargetDir, "scala/genfactories"), theVersion, theScalaVersion)
  }

  val genCompatibleClasses = TaskKey[Unit]("gencompcls", "Generate Compatible Classes for Java 6 & 7")
  val genCompatibleClassesTask = genCompatibleClasses := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenCompatibleClasses.genMain(new File(mainTargetDir, "scala/gencompclass"), theVersion, theScalaVersion)
  }

  val genVersions = TaskKey[Unit]("genversions", "Generate Versions object")
  val genVersionsTask = genVersions := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenVersions.genScalaTestVersions(new File(mainTargetDir, "scala/gencompclass"), theVersion, theScalaVersion)
  }

  val genContain1 = TaskKey[Unit]("gencontain1", "Generate contain matcher tests 1")
  val genContainTask1 = genContain1 := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenContain1.genTest(new File(testTargetDir, "scala/gencontain1"), theVersion, theScalaVersion)
  }

  val genContain2 = TaskKey[Unit]("gencontain2", "Generate contain matcher tests 2")
  val genContainTask2 = genContain2 := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenContain2.genTest(new File(testTargetDir, "scala/gencontain2"), theVersion, theScalaVersion)
  }

  val genSorted = TaskKey[Unit]("gensorted", "Generate sorted matcher tests")
  val genSortedTask = genSorted := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenSorted.genTest(new File(testTargetDir, "scala/gensorted"), theVersion, theScalaVersion)
  }

  val genLoneElement = TaskKey[Unit]("genloneelement", "Generate lone element matcher tests")
  val genLoneElementTask = genLoneElement := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenLoneElement.genTest(new File(testTargetDir, "scala/genloneelement"), theVersion, theScalaVersion)
  }

  val genEmpty = TaskKey[Unit]("genempty", "Generate empty matcher tests")
  val genEmptyTask = genEmpty := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenEmpty.genTest(new File(testTargetDir, "scala/genempty"), theVersion, theScalaVersion)
  }

  val genCode = TaskKey[Unit]("gencode", "Generate Code, includes Must Matchers and They Word tests.")
  val genCodeTask = genCode := {
    val mainTargetDir = (sourceManaged in Compile).value
    val testTargetDir = (sourceManaged in Test).value
    val theVersion = version.value
    val theScalaVersion = scalaVersion.value

    GenGen.genMain(new File(mainTargetDir, "scala/gengen"), theVersion, theScalaVersion)
    GenTable.genMain(new File(mainTargetDir, "scala/gentables"), theVersion, theScalaVersion)
    GenMatchers.genMain(new File(mainTargetDir, "scala/genmatchers"), theVersion, theScalaVersion)
    GenFactories.genMain(new File(mainTargetDir, "scala/genfactories"), theVersion, theScalaVersion)
  }

  /*val genSafeStyles = TaskKey[Unit]("gensafestyles", "Generate safe style traits.")
  val genSafeStylesTask = genSafeStyles <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenSafeStyles.genMain(new File(mainTargetDir, "scala/gensafestyles"), theVersion, theScalaVersion)
  }

  val genSafeStyleTestsTaskKey = TaskKey[Unit]("gensafestyletests", "Generate Safe Style tests")
  val genSafeStyleTestsTask = genSafeStyleTestsTaskKey <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenSafeStyles.genTest(new File(testTargetDir, "scala/gensafestyles"), theVersion, theScalaVersion)
  }*/

  val scalatestDocSourceUrl =
    s"https://github.com/scalatest/releases-source/blob/main/scalatest/${releaseVersion}€{FILE_PATH}.scala"

  val scalacticDocSourceUrl =
    s"https://github.com/scalatest/releases-source/blob/main/scalactic/$releaseVersion€{FILE_PATH}.scala"

  val scalatestDocScalacOptionsSetting =
    scalacOptions in (Compile, doc) := {
      Seq[String](
        // -Ymacro-no-expand is not supported (or needed) under 2.13. In case we want
        // to run Scaladoc under 2.12 again, this is the line that is required:
        // "-Ymacro-no-expand", // avoids need to separate out macros in docsrc dir
        "-groups", // enables the @group tags in Scaladocs
        "-sourcepath", docsrcDir.value.getAbsolutePath,
        "-doc-title", projectTitle.value +" "+ releaseVersion,
        "-doc-source-url", scalatestDocSourceUrl)
    }

  val scalacticDocScalacOptionsSetting =
    scalacOptions in (Compile, doc) :=
      Seq[String](
        // -Ymacro-no-expand is not supported (or needed) under 2.13. In case we want
        // to run Scaladoc under 2.12 again, this is the line that is required:
        // "-Ymacro-no-expand", // avoids need to separate out macros in docsrc dir
        "-groups", // enables the @group tags in Scaladocs
        "-sourcepath", docsrcDir.value.getAbsolutePath,
        "-doc-title", projectTitle.value +" "+ releaseVersion,
        "-doc-source-url", scalacticDocSourceUrl)
}
// set scalacOptions in (Compile, console) += "-Xlog-implicits"
// set scalacOptions in (Compile, console) += "-Xlog-implicits"
// set scalacOptions in (Compile, console) += "-Xlog-implicits"
// set scalacOptions in (Compile, console) += "-nowarn"
