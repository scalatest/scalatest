import sbt._
import Keys._
import java.net.{URL, URLClassLoader}
import java.io.PrintWriter
import scala.io.Source
import com.typesafe.sbt.osgi.SbtOsgi._
import com.typesafe.sbt.SbtPgp._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

object ScalatestBuild extends Build {

  // To run gentests
  // rm -rf gentests
  // sbt genGenTests/test  (etc., look at specific failures on CI output)

  // To enable deprecation warnings on the fly
  // set scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")

  // To temporarily switch sbt to a different Scala version:
  // > ++ 2.10.5
  val buildScalaVersion = "2.11.8"

  val releaseVersion = "3.0.1"

  val scalacheckVersion = "1.13.4"

  val githubTag = "release-3.0.1" // for scaladoc source urls

  val scalatestDocSourceUrl =
    "https://github.com/scalatest/scalatest/tree/"+ githubTag +
    "/scalatest/€{FILE_PATH}.scala"

  val scalacticDocSourceUrl =
    "https://github.com/scalatest/scalatest/tree/"+ githubTag +
      "/scalactic/€{FILE_PATH}.scala"

  def envVar(name: String): Option[String] =
    try {
      Some(sys.env(name))
    }
    catch {
      case e: NoSuchElementException => None
    }

  def getGPGFilePath: String =
    envVar("SCALATEST_GPG_FILE") match {
      case Some(path) => path
      case None => (Path.userHome / ".gnupg" / "secring.gpg").getAbsolutePath
    }

  def getGPGPassphase: Option[Array[Char]] =
    envVar("SCALATEST_GPG_PASSPHASE") match {
      case Some(passphase) => Some(passphase.toCharArray)
      case None => None
    }

  def getNexusCredentials: Credentials =
    (envVar("SCALATEST_NEXUS_LOGIN"), envVar("SCALATEST_NEXUS_PASSWORD")) match {
      case (Some(login), Some(password)) => Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", login, password)
      case _ => Credentials(Path.userHome / ".ivy2" / ".credentials")
    }

  def getJavaHome: Option[File] =
    envVar("JAVA_HOME") match {
      case Some(javaHome) => Some(file(javaHome))
      case None =>
        val javaHome = new File(System.getProperty("java.home"))
        val javaHomeBin = new File(javaHome, "bin")
        val javac = new File(javaHomeBin, "javac")
        val javacExe = new File(javaHomeBin, "javac.exe")
        if (javac.exists || javacExe.exists)
          Some(file(javaHome.getAbsolutePath))
        else {
          println("WARNING: No JAVA_HOME detected, javac on PATH will be used.  Set JAVA_HOME enviroment variable to a JDK to remove this warning.")
          None
        }
    }

  def sharedSettings: Seq[Setting[_]] = Seq(
    javaHome := getJavaHome,
    scalaVersion := buildScalaVersion,
    crossScalaVersions := Seq(buildScalaVersion, "2.10.6", "2.12.0"),
    version := releaseVersion,
    scalacOptions ++= Seq("-feature", "-target:jvm-1.6"),
    resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public",
    libraryDependencies ++= scalaLibraries(scalaVersion.value),
    /*publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT")) Some("publish-snapshots" at nexus + "content/repositories/snapshots")
      else                             Some("publish-releases" at nexus + "service/local/staging/deploy/maven2")
    },*/
    publishTo := {
      val nexus = "https://oss.sonatype.org/"
      if (version.value.trim.endsWith("SNAPSHOT"))
        Some("publish-snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("publish-releases" at nexus + "service/local/staging/deploy/maven2")
    },
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
    pgpSecretRing := file(getGPGFilePath),
    pgpPassphrase := getGPGPassphase
  )

  lazy val scalatestDocSettings = Seq(
    docsrcDirSetting,
    scalatestDocScalacOptionsSetting
  )

  lazy val scalacticDocSettings = Seq(
    docsrcDirSetting,
    scalacticDocScalacOptionsSetting
  )

  def scalacheckDependency(config: String) =
    "org.scalacheck" %% "scalacheck" % scalacheckVersion % config

  def crossBuildLibraryDependencies(theScalaVersion: String) =
    CrossVersion.partialVersion(theScalaVersion) match {
      // if scala 2.11+ is used, add dependency on scala-xml module
      case Some((2, scalaMajor)) if scalaMajor >= 11 =>
        Seq(
          "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
          "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
          scalacheckDependency("optional")
        )
      case _ =>
        Seq(scalacheckDependency("optional"))
    }

  def scalaLibraries(theScalaVersion: String) =
    Seq(
      "org.scala-lang" % "scala-compiler" % theScalaVersion % "provided",
      "org.scala-lang" % "scala-reflect" % theScalaVersion // this is needed to compile macro
    )

  def scalatestLibraryDependencies =
    Seq(
      "org.scala-sbt" % "test-interface" % "1.0" % "optional",
      "org.easymock" % "easymockclassextension" % "3.2" % "optional",
      "org.jmock" % "jmock-legacy" % "2.8.1" % "optional",
      "org.mockito" % "mockito-all" % "1.10.19" % "optional",
      "org.testng" % "testng" % "6.7" % "optional",
      "com.google.inject" % "guice" % "4.0" % "optional",
      "junit" % "junit" % "4.10" % "optional",
      "org.seleniumhq.selenium" % "selenium-java" % "2.45.0" % "optional",
      "org.apache.ant" % "ant" % "1.7.1" % "optional",
      "org.ow2.asm" % "asm-all" % "4.1" % "optional",
      "org.pegdown" % "pegdown" % "1.4.2" % "optional"
    )

  def scalatestTestLibraryDependencies =
    Seq(
      "commons-io" % "commons-io" % "1.3.2" % "test",
      "org.eclipse.jetty" % "jetty-server" % "8.1.18.v20150929" % "test",
      "org.eclipse.jetty" % "jetty-webapp" % "8.1.18.v20150929" % "test"
    )

  def scalatestJSLibraryDependencies =
    Seq(
      "org.scala-js" %% "scalajs-test-interface" % "0.6.13"
    )

  def scalatestTestOptions =
    Seq(Tests.Argument(TestFrameworks.ScalaTest,
      "-l", "org.scalatest.tags.Slow",
      "-m", "org.scalatest",
      "-m", "org.scalactic",
      "-m", "org.scalactic.anyvals",
      "-m", "org.scalactic.algebra",
      "-m", "org.scalactic.enablers",
      "-m", "org.scalatest.fixture",
      "-m", "org.scalatest.concurrent",
      "-m", "org.scalatest.testng",
      "-m", "org.scalatest.junit",
      "-m", "org.scalatest.jmock",
      "-m", "org.scalatest.events",
      "-m", "org.scalatest.prop",
      "-m", "org.scalatest.tools",
      "-m", "org.scalatest.matchers",
      "-m", "org.scalatest.suiteprop",
      "-m", "org.scalatest.mock",
      "-m", "org.scalatest.path",
      "-m", "org.scalatest.selenium",
      "-m", "org.scalatest.exceptions",
      "-m", "org.scalatest.time",
      "-m", "org.scalatest.words",
      "-m", "org.scalatest.enablers",
      "-oDI",
      "-W", "120", "60",
      "-h", "target/html",
      "-u", "target/junit",
      "-fW", "target/result.txt"))

  def scalatestTestJSOptions =
    Seq(Tests.Argument(TestFrameworks.ScalaTest,
      "-l", "org.scalatest.tags.Slow",
      "-m", "org.scalatest",
      "-m", "org.scalactic",
      "-m", "org.scalactic.anyvals",
      "-m", "org.scalactic.algebra",
      "-m", "org.scalactic.enablers",
      "-m", "org.scalatest.fixture",
      "-m", "org.scalatest.concurrent",
      "-m", "org.scalatest.testng",
      "-m", "org.scalatest.junit",
      "-m", "org.scalatest.events",
      "-m", "org.scalatest.prop",
      "-m", "org.scalatest.tools",
      "-m", "org.scalatest.matchers",
      "-m", "org.scalatest.suiteprop",
      "-m", "org.scalatest.mock",
      "-m", "org.scalatest.path",
      "-m", "org.scalatest.selenium",
      "-m", "org.scalatest.exceptions",
      "-m", "org.scalatest.time",
      "-m", "org.scalatest.words",
      "-m", "org.scalatest.enablers",
      "-oDIF"))

  lazy val commonTest = Project("common-test", file("common-test"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Common test classes used by scalactic and scalatest",
      libraryDependencies += scalacheckDependency("optional")
    ).dependsOn(scalacticMacro, LocalProject("scalatest"))

  lazy val commonTestJS = Project("commonTestJS", file("common-test.js"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Common test classes used by scalactic.js and scalatest.js",
      libraryDependencies += scalacheckDependency("optional"),
      sourceGenerators in Compile += {
        Def.task{
          GenCommonTestJS.genMain((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalacticMacroJS, LocalProject("scalatestJS")).enablePlugins(ScalaJSPlugin)

  lazy val scalacticMacro = Project("scalacticMacro", file("scalactic-macro"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Scalactic Macro",
      organization := "org.scalactic",
      sourceGenerators in Compile += {
        Def.task{
          ScalacticGenResourcesJVM.genResources((sourceManaged in Compile).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenAnyVals.genMain((sourceManaged in Compile).value / "scala" / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value)
        }.taskValue
      },
      // Disable publishing macros directly, included in scalactic main jar
      publish := {},
      publishLocal := {}
    )

  lazy val scalacticMacroJS = Project("scalacticMacroJS", file("scalactic-macro.js"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Scalactic Macro.js",
      organization := "org.scalactic",
      sourceGenerators in Compile += {
        Def.task{
          GenScalacticJS.genMacroScala((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value) ++
          ScalacticGenResourcesJSVM.genResources((sourceManaged in Compile).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenAnyVals.genMain((sourceManaged in Compile).value / "scala" / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value)
        }.taskValue
      },
      // Disable publishing macros directly, included in scalactic main jar
      publish := {},
      publishLocal := {}
    ).enablePlugins(ScalaJSPlugin)

  lazy val scalactic = Project("scalactic", file("scalactic"))
    .settings(sharedSettings: _*)
    .settings(scalacticDocSettings: _*)
    .settings(
      projectTitle := "Scalactic",
      organization := "org.scalactic",
      initialCommands in console := "import org.scalactic._",
      sourceGenerators in Compile += {
        Def.task{
          GenVersions.genScalacticVersions((sourceManaged in Compile).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          ScalacticGenResourcesJVM.genFailureMessages((sourceManaged in Compile).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value)
        }.taskValue
      },
      // include the macro classes and resources in the main jar
      mappings in (Compile, packageBin) ++= mappings.in(scalacticMacro, Compile, packageBin).value,
      // include the macro sources in the main source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalacticMacro, Compile, packageSrc).value,
      scalacticDocSourcesSetting,
      docTaskSetting
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

  lazy val scalacticJS = Project("scalacticJS", file("scalactic.js"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Scalactic.js",
      organization := "org.scalactic",
      moduleName := "scalactic",
      sourceGenerators in Compile += {
        Def.task {
          GenScalacticJS.genScala((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value) ++
          ScalacticGenResourcesJSVM.genFailureMessages((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      },
      resourceGenerators in Compile += {
        Def.task {
          GenScalacticJS.genResource((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
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
        "Bundle-Description" -> "Scalactic.js is an open-source library for Scala-js projects.",
        "Bundle-DocURL" -> "http://www.scalactic.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalacticMacroJS % "compile-internal, test-internal").enablePlugins(ScalaJSPlugin)

  lazy val scalacticTest = Project("scalactic-test", file("scalactic-test"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Scalactic Test",
      organization := "org.scalactic",
      libraryDependencies += scalacheckDependency("test"),
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    ).dependsOn(scalactic, scalatest % "test", commonTest % "test")

  lazy val scalacticTestJS = Project("scalacticTestJS", file("scalactic-test.js"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Scalactic Test.js",
      organization := "org.scalactic",
      //jsDependencies += RuntimeDOM % "test",
      libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalacheckVersion % "test",
      scalaJSOptimizerOptions ~= { _.withDisableOptimizer(true) },
      //jsEnv := NodeJSEnv(executable = "node").value,
      //jsEnv := PhantomJSEnv().value,
      Seq(Compile, Test).flatMap(c => inConfig(c)(jsEnv := RhinoJSEnv().value)), // to use rhino
      scalaJSStage in Global := FastOptStage,
      //postLinkJSEnv := PhantomJSEnv().value,
      //postLinkJSEnv := NodeJSEnv(executable = "node").value,
      sourceGenerators in Test += {
        Def.task {
          GenScalacticJS.genTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    ).dependsOn(scalacticJS, scalatestJS % "test", commonTestJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatest = Project("scalatest", file("scalatest"))
   .settings(sharedSettings: _*)
   .settings(scalatestDocSettings: _*)
   .settings(
     projectTitle := "ScalaTest",
     organization := "org.scalatest",
     moduleName := "scalatest",
     initialCommands in console := """|import org.scalatest._
                                      |import org.scalactic._
                                      |import Matchers._""".stripMargin,
     libraryDependencies ++= crossBuildLibraryDependencies(scalaVersion.value),
     libraryDependencies ++= scalatestLibraryDependencies,
     genMustMatchersTask,
     genGenTask,
     genTablesTask,
     genCodeTask,
     genFactoriesTask,
     genCompatibleClassesTask,
     //genSafeStylesTask,
     sourceGenerators in Compile += Def.task {
       genFiles("gengen", "GenGen.scala")(GenGen.genMain)(baseDirectory.value, (sourceManaged in Compile).value, version.value, scalaVersion.value)
     }.taskValue,
     sourceGenerators in Compile += Def.task {
       genFiles("gentables", "GenTable.scala")(GenTable.genMain)(baseDirectory.value, (sourceManaged in Compile).value, version.value, scalaVersion.value)
     }.taskValue,
     sourceGenerators in Compile += Def.task {
       genFiles("genmatchers", "MustMatchers.scala")(GenMatchers.genMain)(baseDirectory.value, (sourceManaged in Compile).value, version.value, scalaVersion.value)
     }.taskValue,
     sourceGenerators in Compile += Def.task {
       genFiles("genfactories", "GenFactories.scala")(GenFactories.genMain)(baseDirectory.value, (sourceManaged in Compile).value, version.value, scalaVersion.value)
     }.taskValue,
     sourceGenerators in Compile += Def.task {
       genFiles("gencompcls", "GenCompatibleClasses.scala")(GenCompatibleClasses.genMain)(baseDirectory.value, (sourceManaged in Compile).value, version.value, scalaVersion.value)
     }.taskValue,
     sourceGenerators in Compile += Def.task {
       genFiles("genversions", "GenVersions.scala")(GenVersions.genScalaTestVersions)(baseDirectory.value, (sourceManaged in Compile).value, version.value, scalaVersion.value)
     }.taskValue,
     /*sourceGenerators in Compile += Def.task {
       genFiles("gensafestyles", "GenSafeStyles.scala")(GenSafeStyles.genMain)(baseDirectory.value, (sourceManaged in Compile).value, version.value, scalaVersion.value)
     }.taskValue,*/
     scalatestDocSourcesSetting,
     sourceGenerators in Compile += {
       Def.task{
         ScalaTestGenResourcesJVM.genResources((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
         ScalaTestGenResourcesJVM.genFailureMessages((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)
       }.taskValue
     },
     docTaskSetting
   ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest",
        "org.scalatest.compatible",
        "org.scalatest.concurrent",
        "org.scalatest.easymock",
        "org.scalatest.enablers",
        "org.scalatest.events",
        "org.scalatest.exceptions",
        "org.scalatest.fixture",
        "org.scalatest.jmock",
        "org.scalatest.junit",
        "org.scalatest.matchers",
        "org.scalatest.mock",
        "org.scalatest.mockito",
        "org.scalatest.path",
        "org.scalatest.prop",
        "org.scalatest.refspec",
        "org.scalatest.selenium",
        "org.scalatest.tags",
        "org.scalatest.tagobjects",
        "org.scalatest.testng",
        "org.scalatest.time",
        "org.scalatest.tools",
        "org.scalatest.verb",
        "org.scalatest.words"
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
   ).dependsOn(scalacticMacro % "compile-internal, test-internal", scalactic)

  lazy val scalatestTest = Project("scalatest-test", file("scalatest-test"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest Test",
      organization := "org.scalatest",
      libraryDependencies ++= crossBuildLibraryDependencies(scalaVersion.value),
      libraryDependencies ++= scalatestLibraryDependencies,
      libraryDependencies ++= scalatestTestLibraryDependencies,
      testOptions in Test := scalatestTestOptions,
      logBuffered in Test := false,
      //fork in Test := true,
      //parallelExecution in Test := true,
      //testForkedParallel in Test := true,
      baseDirectory in Test := file("./"),
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    ).dependsOn(scalatest % "test", commonTest % "test")

  lazy val scalatestJS = Project("scalatestJS", file("scalatest.js"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest",
      organization := "org.scalatest",
      moduleName := "scalatest",
      initialCommands in console := """|import org.scalatest._
                                      |import org.scalactic._
                                      |import Matchers._""".stripMargin,
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + scalatestApp.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      libraryDependencies ++= scalatestJSLibraryDependencies,
      libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalacheckVersion % "optional",
      //jsDependencies += RuntimeDOM % "test",
      sourceGenerators in Compile += {
        Def.task {
          GenScalaTestJS.genHtml((sourceManaged in Compile).value, version.value, scalaVersion.value)

          GenScalaTestJS.genScala((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value) ++
          GenVersions.genScalaTestVersions((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          GenScalaTestJS.genJava((sourceManaged in Compile).value / "java", version.value, scalaVersion.value) ++
          ScalaTestGenResourcesJSVM.genResources((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          ScalaTestGenResourcesJSVM.genFailureMessages((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      },
      genFactoriesTask,
      //genSafeStylesTask,
      sourceGenerators in Compile += Def.task {
        genFiles("genfactories", "GenFactories.scala")(GenFactories.genMainJS)(baseDirectory.value, (sourceManaged in Compile).value, version.value, scalaVersion.value)
      }.taskValue,
      sourceGenerators in Compile += Def.task {
        genFiles("gengen", "GenGen.scala")(GenGen.genMain)(baseDirectory.value, (sourceManaged in Compile).value, version.value, scalaVersion.value)
      }.taskValue,
      sourceGenerators in Compile += Def.task {
        genFiles("gentables", "GenTable.scala")(GenTable.genMainForScalaJS)(baseDirectory.value, (sourceManaged in Compile).value, version.value, scalaVersion.value)
      }.taskValue,
      sourceGenerators in Compile += Def.task {
        genFiles("genmatchers", "MustMatchers.scala")(GenMatchers.genMainForScalaJS)(baseDirectory.value, (sourceManaged in Compile).value, version.value, scalaVersion.value)
      }.taskValue,
      /*sourceGenerators in Compile += Def.task {
        genFiles("gensafestyles", "GenSafeStyles.scala")(GenSafeStyles.genMainForScalaJS)(baseDirectory.value, (sourceManaged in Compile).value, version.value, scalaVersion.value)
      }.taskValue,*/
      /*sourceGenerators in Compile += Def.task {
        genFiles("genversions", "GenVersions.scala")(GenVersions.genScalaTestVersions)(baseDirectory.value, (sourceManaged in Compile).value, version.value, scalaVersion.value)
      }.taskValue,*/
      scalatestJSDocTaskSetting
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest",
        "org.scalatest.compatible",
        "org.scalatest.concurrent",
        "org.scalatest.enablers",
        "org.scalatest.events",
        "org.scalatest.exceptions",
        "org.scalatest.fixture",
        "org.scalatest.matchers",
        "org.scalatest.path",
        "org.scalatest.prop",
        "org.scalatest.tags",
        "org.scalatest.tagobjects",
        "org.scalatest.time",
        "org.scalatest.tools",
        "org.scalatest.verb",
        "org.scalatest.words"
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
        "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc.",
        "Main-Class" -> "org.scalatest.tools.Runner"
      )
    ).dependsOn(scalacticMacroJS % "compile-internal, test-internal", scalacticJS).enablePlugins(ScalaJSPlugin)

  lazy val scalatestTestJS = Project("scalatestTestJS", file("scalatest-test.js"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest Test",
      organization := "org.scalatest",
      libraryDependencies ++= crossBuildLibraryDependencies(scalaVersion.value),
      libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalacheckVersion % "test",
      //jsDependencies += RuntimeDOM % "test",
      scalaJSOptimizerOptions ~= { _.withDisableOptimizer(true) },
      //jsEnv := NodeJSEnv(executable = "node").value,
      //jsEnv := PhantomJSEnv().value,
      Seq(Compile, Test).flatMap(c => inConfig(c)(jsEnv := RhinoJSEnv().value)), // to use rhino
      scalaJSStage in Global := FastOptStage,
      fork in test := false,
      testOptions in Test := scalatestTestJSOptions,
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestJS.genTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }/*,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("gengen", "GenGen.scala")(GenGen.genTest),
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genmatchers", "GenMustMatchersTests.scala")(GenMustMatchersTests.genTestForScalaJS)*/
    ).dependsOn(scalatestJS % "test", commonTestJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestApp = Project("scalatestApp", file("."))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest App",
      name := "scalatest-app",
      organization := "org.scalatest",
      libraryDependencies ++= crossBuildLibraryDependencies(scalaVersion.value),
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
      unmanagedResourceDirectories in Compile += baseDirectory.value / "scalatest" / "src" / "main" / "resources"
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest",
        "org.scalatest.compatible",
        "org.scalatest.concurrent",
        "org.scalatest.easymock",
        "org.scalatest.enablers",
        "org.scalatest.events",
        "org.scalatest.exceptions",
        "org.scalatest.fixture",
        "org.scalatest.jmock",
        "org.scalatest.junit",
        "org.scalatest.matchers",
        "org.scalatest.mock",
        "org.scalatest.mockito",
        "org.scalatest.path",
        "org.scalatest.prop",
        "org.scalatest.refspec",
        "org.scalatest.selenium",
        "org.scalatest.tags",
        "org.scalatest.tagobjects",
        "org.scalatest.testng",
        "org.scalatest.time",
        "org.scalatest.tools",
        "org.scalatest.verb",
        "org.scalatest.words",
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
    ).dependsOn(scalacticMacro % "compile-internal, test-internal", scalactic % "compile-internal", scalatest % "compile-internal").aggregate(scalactic, scalatest, scalacticTest, scalatestTest)

  lazy val scalatestAppJS = Project("scalatestAppJS", file("scalatest-app.js"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest App",
      name := "scalatest-app",
      organization := "org.scalatest",
      moduleName := "scalatest-app",
      libraryDependencies ++= crossBuildLibraryDependencies(scalaVersion.value),
      libraryDependencies ++= scalatestJSLibraryDependencies,
      // include the scalactic classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalacticJS, Compile, packageBin).value,
      // include the scalactic sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalacticJS, Compile, packageSrc).value,
      // include the scalatest classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestJS, Compile, packageBin).value,
      // include the scalatest sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestJS, Compile, packageSrc).value,
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      }
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest",
        "org.scalatest.compatible",
        "org.scalatest.concurrent",
        "org.scalatest.enablers",
        "org.scalatest.events",
        "org.scalatest.exceptions",
        "org.scalatest.fixture",
        "org.scalatest.matchers",
        "org.scalatest.path",
        "org.scalatest.prop",
        "org.scalatest.tags",
        "org.scalatest.tagobjects",
        "org.scalatest.time",
        "org.scalatest.tools",
        "org.scalatest.verb",
        "org.scalatest.words",
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
    ).dependsOn(scalacticMacroJS % "compile-internal, test-internal", scalacticJS % "compile-internal", scalatestJS % "compile-internal").aggregate(scalacticJS, scalatestJS, scalacticTestJS, scalatestTestJS).enablePlugins(ScalaJSPlugin)

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

  lazy val genRegularTests1 = Project("genRegularTests1", file("gentests/GenRegular1"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genRegularTask1,
      sourceGenerators in Test += Def.task {
        genFiles("genregular1", "GenRegular1.scala")(GenRegularTests1.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genRegularTests2 = Project("genRegularTests2", file("gentests/GenRegular2"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genRegularTask2,
      sourceGenerators in Test += Def.task {
        genFiles("genregular2", "GenRegular2.scala")(GenRegularTests2.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genRegularTests3 = Project("genRegularTests3", file("gentests/GenRegular3"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genRegularTask3,
      sourceGenerators in Test += Def.task {
        genFiles("genregular3", "GenRegular3.scala")(GenRegularTests3.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genRegularTests4 = Project("genRegularTests4", file("gentests/GenRegular4"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genRegularTask4,
      libraryDependencies ++= scalatestLibraryDependencies,
      testOptions in Test := scalatestTestOptions,
      sourceGenerators in Test += Def.task {
        genFiles("genregular4", "GenRegularTests1.scala")(GenRegularTests4.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genRegularTests5 = Project("genRegularTests5", file("gentests/GenRegular5"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genRegularTask5,
      libraryDependencies ++= scalatestLibraryDependencies,
      testOptions in Test := scalatestTestOptions,
      sourceGenerators in Test += Def.task {
        genFiles("genregular5", "GenRegularTests1.scala")(GenRegularTests5.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genMustMatchersTests1 = Project("genMustMatchersTests1", file("gentests/MustMatchers1"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genMustMatchersTask,
      sourceGenerators in Test += Def.task {
        genFiles("genmatchers1", "GenMustMatchersTests.scala")(GenMustMatchersTests1.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genMustMatchersTests2 = Project("genMustMatchersTests2", file("gentests/MustMatchers2"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genMustMatchersTask,
      sourceGenerators in Test += Def.task {
        genFiles("genmatchers2", "GenMustMatchersTests.scala")(GenMustMatchersTests2.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genMustMatchersTests3 = Project("genMustMatchersTests3", file("gentests/MustMatchers3"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genMustMatchersTask,
      sourceGenerators in Test += Def.task {
        genFiles("genmatchers3", "GenMustMatchersTests.scala")(GenMustMatchersTests3.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genMustMatchersTests4 = Project("genMustMatchersTests4", file("gentests/MustMatchers4"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genMustMatchersTask,
      sourceGenerators in Test += Def.task {
        genFiles("genmatchers4", "GenMustMatchersTests.scala")(GenMustMatchersTests4.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genGenTests = Project("genGenTests", file("gentests/GenGen"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genGenTask,
      sourceGenerators in Test += Def.task {
        genFiles("gengen", "GenGen.scala")(GenGen.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genTablesTests = Project("genTablesTests", file("gentests/GenTables"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genTablesTask,
      sourceGenerators in Test += Def.task {
        genFiles("gentables", "GenTable.scala")(GenTable.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genInspectorsTests = Project("genInspectorsTests", file("gentests/GenInspectors"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genInspectorsTask,
      sourceGenerators in Test += Def.task {
        genFiles("geninspectors", "GenInspectors.scala")(GenInspectors.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genInspectorsShorthandsTests1 = Project("genInspectorsShorthandsTests1", file("gentests/GenInspectorsShorthands1"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genInspectorsShorthandsTask1,
      sourceGenerators in Test += Def.task {
        genFiles("geninspectorsshorthands1", "GenInspectorsShorthands.scala")(GenInspectorsShorthands1.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genInspectorsShorthandsTests2 = Project("genInspectorsShorthandsTests2", file("gentests/GenInspectorsShorthands2"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genInspectorsShorthandsTask2,
      sourceGenerators in Test += Def.task {
        genFiles("geninspectorsshorthands2", "GenInspectorsShorthands.scala")(GenInspectorsShorthands2.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genTheyTests = Project("genTheyTests", file("gentests/GenThey"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genTheyWordTask,
      sourceGenerators in Test += Def.task {
        genFiles("genthey", "GenTheyWord.scala")(GenTheyWord.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genContainTests1 = Project("genContainTests1", file("gentests/GenContain1"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genContainTask1,
      sourceGenerators in Test += Def.task {
        genFiles("gencontain1", "GenContain1.scala")(GenContain1.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genContainTests2 = Project("genContainTests2", file("gentests/GenContain2"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genContainTask2,
      sourceGenerators in Test += Def.task {
        genFiles("gencontain2", "GenContain2.scala")(GenContain2.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genSortedTests = Project("genSortedTests", file("gentests/GenSorted"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genSortedTask,
      sourceGenerators in Test += Def.task {
        genFiles("gensorted", "GenSorted.scala")(GenSorted.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genLoneElementTests = Project("genLoneElementTests", file("gentests/GenLoneElement"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genLoneElementTask,
      sourceGenerators in Test += Def.task {
        genFiles("genloneelement", "GenLoneElement.scala")(GenLoneElement.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genEmptyTests = Project("genEmptyTests", file("gentests/GenEmpty"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genEmptyTask,
      sourceGenerators in Test += Def.task {
        genFiles("genempty", "GenEmpty.scala")(GenEmpty.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  /*lazy val genSafeStyleTests = Project("genSafeStyleTests", file("gentests/GenSafeStyles"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genSafeStyleTestsTask,
      sourceGenerators in Test += Def.task {
        genFiles("gensafestyletests", "GenSafeStyles.scala")(GenSafeStyles.genTest)(baseDirectory.value, (sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")*/

  lazy val gentests = Project("gentests", file("gentests"))
    .aggregate(genMustMatchersTests1, genMustMatchersTests2, genMustMatchersTests3, genMustMatchersTests4, genGenTests, genTablesTests, genInspectorsTests, genInspectorsShorthandsTests1,
               genInspectorsShorthandsTests2, genTheyTests, genContainTests1, genContainTests2, genSortedTests, genLoneElementTests, genEmptyTests/*, genSafeStyleTests*/)

  lazy val examples = Project("examples", file("examples"), delegates = scalatest :: Nil)
    .settings(
      scalaVersion := buildScalaVersion,
      libraryDependencies += scalacheckDependency("compile")
    ).dependsOn(scalacticMacro, scalactic, scalatest)

  lazy val examplesJS = Project("examplesJS", file("examples.js"), delegates = scalatest :: Nil)
    .settings(
      scalaVersion := buildScalaVersion,
      libraryDependencies += scalacheckDependency("compile"),
      sourceGenerators in Test += {
        Def.task {
          GenExamplesJS.genScala((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalacticMacroJS, scalacticJS, scalatestJS).enablePlugins(ScalaJSPlugin)

  def genFiles(name: String, generatorSource: String)(gen: (File, String, String) => Unit)(basedir: File, outDir: File, theVersion: String, theScalaVersion: String): Seq[File] = {
    val tdir = outDir / "scala" / name
    val jdir = outDir / "java" / name
    val genSource = basedir / "project" / generatorSource

    def results = (tdir ** "*.scala").get ++ (jdir ** "*.java").get
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
// set scalacOptions in (Compile, console) += "-Xlog-implicits"
// set scalacOptions in (Compile, console) += "-Xlog-implicits"
// set scalacOptions in (Compile, console) += "-Xlog-implicits"
// set scalacOptions in (Compile, console) += "-nowarn"
