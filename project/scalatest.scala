import sbt._
import Keys._
import java.net.{URL, URLClassLoader}
import java.io.PrintWriter
import scala.io.Source
import com.typesafe.sbt.osgi.SbtOsgi._
import com.typesafe.sbt.SbtPgp._
import org.scalajs.sbtplugin.ScalaJSPlugin

object ScalatestBuild extends Build {
 
  // To run gentests
  // rm -rf gentests
  // sbt genGenTests/test  (etc., look at specific failures on CI output)

  // To enable deprecation warnings on the fly
  // set scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation")

  // To temporarily switch sbt to a different Scala version:
  // > ++ 2.10.4
  val buildScalaVersion = "2.11.5"

  val releaseVersion = "2.3.0-SNAP2"
  val githubTag = "release-2.3.0-SNAP2-for-scala-2.11-and-2.10" // for scaladoc source urls

  val docSourceUrl =
    "https://github.com/scalatest/scalatest/tree/"+ githubTag +
    "€{FILE_PATH}.scala"

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
    crossScalaVersions := Seq(buildScalaVersion, "2.10.4"),
    version := releaseVersion,
    scalacOptions ++= Seq("-feature", "-target:jvm-1.6"),
    resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public",
    libraryDependencies ++= scalaLibraries(scalaVersion.value),
    publishTo <<= version { v: String =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT")) Some("publish-snapshots" at nexus + "content/repositories/snapshots")
      else                             Some("publish-releases" at nexus + "service/local/staging/deploy/maven2")
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

  lazy val sharedDocSettings = Seq(
    docsrcDirSetting,
    docScalacOptionsSetting
  )

  def scalacheckDependency(config: String) =
    "org.scalacheck" %% "scalacheck" % "1.12.1" % config

  def crossBuildLibraryDependencies(theScalaVersion: String) =
    CrossVersion.partialVersion(theScalaVersion) match {
      // if scala 2.11+ is used, add dependency on scala-xml module
      case Some((2, scalaMajor)) if scalaMajor >= 11 =>
        Seq(
          "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
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
      "org.easymock" % "easymockclassextension" % "3.1" % "optional",
      "org.jmock" % "jmock-legacy" % "2.5.1" % "optional",
      "org.mockito" % "mockito-all" % "1.9.0" % "optional",
      "org.testng" % "testng" % "6.8.7" % "optional",
      "com.google.inject" % "guice" % "2.0" % "optional",
      "junit" % "junit" % "4.10" % "optional",
      "org.seleniumhq.selenium" % "selenium-java" % "2.35.0" % "optional",
      "org.apache.ant" % "ant" % "1.7.1" % "optional",
      "commons-io" % "commons-io" % "1.3.2" % "test",
      "org.eclipse.jetty" % "jetty-server" % "8.1.8.v20121106" % "test",
      "org.eclipse.jetty" % "jetty-webapp" % "8.1.8.v20121106" % "test",
      "org.ow2.asm" % "asm-all" % "4.1" % "optional",
      "org.pegdown" % "pegdown" % "1.4.2" % "optional"
    )

  def scalatestJSLibraryDependencies =
    Seq(
      "org.scala-js" %% "scalajs-test-interface" % "0.6.0",
      "org.easymock" % "easymockclassextension" % "3.1" % "optional",
      "org.jmock" % "jmock-legacy" % "2.5.1" % "optional",
      "org.mockito" % "mockito-all" % "1.9.0" % "optional",
      "org.testng" % "testng" % "6.8.7" % "optional",
      "com.google.inject" % "guice" % "2.0" % "optional",
      "junit" % "junit" % "4.10" % "optional",
      "org.seleniumhq.selenium" % "selenium-java" % "2.35.0" % "optional",
      "org.apache.ant" % "ant" % "1.7.1" % "optional",
      "commons-io" % "commons-io" % "1.3.2" % "test",
      "org.eclipse.jetty" % "jetty-server" % "8.1.8.v20121106" % "test",
      "org.eclipse.jetty" % "jetty-webapp" % "8.1.8.v20121106" % "test",
      "org.ow2.asm" % "asm-all" % "4.1" % "optional",
      "org.pegdown" % "pegdown" % "1.4.2" % "optional"
    )

  def scalatestTestOptions =
    Seq(Tests.Argument("-l", "org.scalatest.tags.Slow",
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
      "-m", "org.scalautils",
      "-oDI",
      "-h", "target/html",
      "-u", "target/junit",
      "-fW", "target/result.txt"))

  lazy val commonTest = Project("common-test", file("common-test"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Common test classes used by scalactic and scalatest",
      libraryDependencies += scalacheckDependency("optional")
    ).dependsOn(scalacticMacro, LocalProject("scalatest"))

  lazy val scalacticMacro = Project("scalacticMacro", file("scalactic-macro"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Scalactic Macro",
      organization := "org.scalactic",
      // Disable publishing macros directly, included in scalactic main jar
      publish := {},
      publishLocal := {}
    )

  lazy val scalactic = Project("scalactic", file("scalactic"))
    .settings(sharedSettings: _*)
    .settings(sharedDocSettings: _*)
    .settings(
      projectTitle := "Scalactic",
      organization := "org.scalactic",
      initialCommands in console := "import org.scalactic._",
      sourceGenerators in Compile += {
        Def.task{
          GenVersions.genScalacticVersions((sourceManaged in Compile).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value)
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
        "org.scalautils"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "Scalactic",
        "Bundle-Description" -> "Scalactic is an open-source library for Scala projects.",
        "Bundle-DocURL" -> "http://www.scalactic.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalacticMacro % "compile-internal, test-internal").aggregate(LocalProject("scalactic-test"))  // avoid dependency in pom on non-existent scalactic-macro artifact, per discussion in http://grokbase.com/t/gg/simple-build-tool/133shekp07/sbt-avoid-dependence-in-a-macro-based-project

  lazy val scalacticJS = Project("scalacticJS", file("scalactic.js"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Scalactic.js",
      organization := "org.scalactic",
      sourceGenerators in Compile += {
        Def.task {
          GenScalacticJS.genScala((sourceManaged in Compile).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value)
        }.taskValue
      },
      resourceGenerators in Compile += {
        Def.task {
          GenScalacticJS.genResource((sourceManaged in Compile).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalacticMacro % "compile-internal, test-internal").enablePlugins(ScalaJSPlugin)

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

  lazy val scalatest = Project("scalatest", file("scalatest"))
   .settings(sharedSettings: _*)
   .settings(sharedDocSettings: _*)
   .settings(
     projectTitle := "ScalaTest",
     organization := "org.scalatest",
     initialCommands in console := """|import org.scalatest._
                                      |import org.scalactic._
                                      |import Matchers._""".stripMargin,
     ivyXML :=
       <dependency org="org.eclipse.jetty.orbit" name="javax.servlet" rev="3.0.0.v201112011016">
         <artifact name="javax.servlet" type="orbit" ext="jar"/>
       </dependency>,
     libraryDependencies ++= crossBuildLibraryDependencies(scalaVersion.value),
     libraryDependencies ++= scalatestLibraryDependencies,
     genMustMatchersTask,
     genGenTask,
     genTablesTask,
     genCodeTask,
     genFactoriesTask,
     genCompatibleClassesTask,
     sourceGenerators in Compile <+=
         (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("gengen", "GenGen.scala")(GenGen.genMain),
     sourceGenerators in Compile <+=
         (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("gentables", "GenTable.scala")(GenTable.genMain),
     sourceGenerators in Compile <+=
         (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("genmatchers", "MustMatchers.scala")(GenMatchers.genMain),
     sourceGenerators in Compile <+=
         (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("genfactories", "GenFactories.scala")(GenFactories.genMain),
     sourceGenerators in Compile <+=
         (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("gencompcls", "GenCompatibleClasses.scala")(GenCompatibleClasses.genMain),
     sourceGenerators in Compile <+=
         (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("genversions", "GenVersions.scala")(GenVersions.genScalaTestVersions),
     scalatestDocSourcesSetting,
     docTaskSetting
   ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest",
        "org.scalatest.concurrent",
        "org.scalatest.enablers",
        "org.scalatest.events",
        "org.scalatest.exceptions",
        "org.scalatest.fixture",
        "org.scalatest.junit",
        "org.scalatest.matchers",
        "org.scalatest.mock",
        "org.scalatest.path",
        "org.scalatest.prop",
        "org.scalatest.selenium",
        "org.scalatest.tags",
        "org.scalatest.tagobjects",
        "org.scalatest.testng",
        "org.scalatest.time",
        "org.scalatest.tools",
        "org.scalatest.verb",
        "org.scalatest.words",
        "org.scalactic",
        "org.scalactic.enablers",
        "org.scalactic.anyvals",
        "org.scalactic.algebra",
        "org.scalautils"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc.",
        "Main-Class" -> "org.scalatest.tools.Runner"
      )
   ).dependsOn(scalacticMacro % "compile-internal, test-internal", scalactic).aggregate(LocalProject("scalatest-test"))

  lazy val scalatestTest = Project("scalatest-test", file("scalatest-test"))
    .settings(sharedSettings: _*)
    .settings(sharedDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Test",
      organization := "org.scalatest",
      libraryDependencies ++= crossBuildLibraryDependencies(scalaVersion.value),
      libraryDependencies ++= scalatestLibraryDependencies,
      testOptions in Test := scalatestTestOptions, 
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    ).dependsOn(scalatest % "test", commonTest % "test")

  lazy val scalatestJS = Project("scalatestJS", file("scalatest.js"))
    .settings(sharedSettings: _*)
    .settings(sharedDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest",
      organization := "org.scalatest",
      initialCommands in console := """|import org.scalatest._
                                      |import org.scalactic._
                                      |import Matchers._""".stripMargin,
      ivyXML :=
        <dependency org="org.eclipse.jetty.orbit" name="javax.servlet" rev="3.0.0.v201112011016">
          <artifact name="javax.servlet" type="orbit" ext="jar"/>
        </dependency>,
      libraryDependencies ++= crossBuildLibraryDependencies(scalaVersion.value),
      libraryDependencies ++= scalatestJSLibraryDependencies,
      sourceGenerators in Compile += {
        Def.task {
          GenScalaTestJS.genScala((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value) ++
          GenVersions.genScalaTestVersions((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          GenScalaTestJS.genJava((sourceManaged in Compile).value / "java", version.value, scalaVersion.value)
        }.taskValue
      },
      /*genMustMatchersTask,
      genGenTask,
      genTablesTask,
      genCodeTask,
      genFactoriesTask,
      genCompatibleClassesTask,
      sourceGenerators in Compile <+=
        (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("gengen", "GenGen.scala")(GenGen.genMain),
      sourceGenerators in Compile <+=
        (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("gentables", "GenTable.scala")(GenTable.genMain),
      sourceGenerators in Compile <+=
        (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("genmatchers", "MustMatchers.scala")(GenMatchers.genMain),
      sourceGenerators in Compile <+=
        (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("genfactories", "GenFactories.scala")(GenFactories.genMain),
      sourceGenerators in Compile <+=
        (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("gencompcls", "GenCompatibleClasses.scala")(GenCompatibleClasses.genMain),
      sourceGenerators in Compile <+=
        (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("genversions", "GenVersions.scala")(GenVersions.genScalaTestVersions),*/
      docTaskSetting
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest",
        "org.scalatest.concurrent",
        "org.scalatest.enablers",
        "org.scalatest.events",
        "org.scalatest.exceptions",
        "org.scalatest.fixture",
        "org.scalatest.junit",
        "org.scalatest.matchers",
        "org.scalatest.mock",
        "org.scalatest.path",
        "org.scalatest.prop",
        "org.scalatest.selenium",
        "org.scalatest.tags",
        "org.scalatest.tagobjects",
        "org.scalatest.testng",
        "org.scalatest.time",
        "org.scalatest.tools",
        "org.scalatest.verb",
        "org.scalatest.words",
        "org.scalactic",
        "org.scalactic.enablers",
        "org.scalactic.anyvals",
        "org.scalactic.algebra",
        "org.scalautils"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc.",
        "Main-Class" -> "org.scalatest.tools.Runner"
      )
    ).dependsOn(scalacticMacro % "compile-internal, test-internal", scalacticJS).enablePlugins(ScalaJSPlugin)

  lazy val scalatestAll = Project("scalatest-all", file("."))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest All",
      name := "scalatest-all",
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
      }
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest",
        "org.scalatest.concurrent",
        "org.scalatest.enablers",
        "org.scalatest.events",
        "org.scalatest.exceptions",
        "org.scalatest.fixture",
        "org.scalatest.junit",
        "org.scalatest.matchers",
        "org.scalatest.mock",
        "org.scalatest.path",
        "org.scalatest.prop",
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
        "org.scalautils"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest",
        "Bundle-Description" -> "ScalaTest is an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc.",
        "Main-Class" -> "org.scalatest.tools.Runner"
      )
    ).dependsOn(scalacticMacro % "compile-internal, test-internal", scalactic % "compile-internal", scalatest % "compile-internal").aggregate(scalactic, scalatest)

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
    testOptions in Test := Seq(Tests.Argument("-h", "target/html"))
  )

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

  lazy val genMustMatchersTests1 = Project("genMustMatchersTests1", file("gentests/MustMatchers1"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genMustMatchersTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genmatchers1", "GenMustMatchersTests.scala")(GenMustMatchersTests1.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genMustMatchersTests2 = Project("genMustMatchersTests2", file("gentests/MustMatchers2"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genMustMatchersTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genmatchers2", "GenMustMatchersTests.scala")(GenMustMatchersTests2.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genMustMatchersTests3 = Project("genMustMatchersTests3", file("gentests/MustMatchers3"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genMustMatchersTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genmatchers3", "GenMustMatchersTests.scala")(GenMustMatchersTests3.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genMustMatchersTests4 = Project("genMustMatchersTests4", file("gentests/MustMatchers4"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genMustMatchersTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genmatchers4", "GenMustMatchersTests.scala")(GenMustMatchersTests4.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genGenTests = Project("genGenTests", file("gentests/GenGen"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genGenTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("gengen", "GenGen.scala")(GenGen.genTest)
    ).dependsOn(scalatest, commonTest, scalacticMacro % "compile-internal, test-internal")

  lazy val genTablesTests = Project("genTablesTests", file("gentests/GenTables"))
    .settings(gentestsSharedSettings: _*)
    .settings(
      genTablesTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("gentables", "GenTable.scala")(GenTable.genTest)
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

  lazy val gentests = Project("gentests", file("gentests"))
    .aggregate(genMustMatchersTests1, genMustMatchersTests2, genMustMatchersTests3, genMustMatchersTests4, genGenTests, genTablesTests, genInspectorsTests, genInspectorsShorthandsTests1,
               genInspectorsShorthandsTests2, genTheyTests, genContainTests1, genContainTests2, genSortedTests, genLoneElementTests, genEmptyTests)

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
  
  val genMustMatchers = TaskKey[Unit]("genmatchers", "Generate Must Matchers")
  val genMustMatchersTask = genMustMatchers <<= (sourceManaged in Compile, sourceManaged in Test, name, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, projName: String, theVersion: String, theScalaVersion: String) =>
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
  val genGenTask = genGen <<= (sourceManaged in Compile, sourceManaged in Test, name, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, projName: String, theVersion: String, theScalaVersion: String) =>
    projName match {
      case "scalatest" => 
        GenGen.genMain(new File(mainTargetDir, "scala/gengen"), theVersion, theScalaVersion)
      case "gentests" =>
        GenGen.genTest(new File(testTargetDir, "scala/gengen"), theVersion, theScalaVersion)
    }
  }
  
  val genTables = TaskKey[Unit]("gentables", "Generate Tables")
  val genTablesTask = genTables <<= (sourceManaged in Compile, sourceManaged in Test, name, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, projName: String, theVersion: String, theScalaVersion: String) =>
    projName match {
      case "scalatest" => 
        GenTable.genMain(new File(mainTargetDir, "scala/gentables"), theVersion, theScalaVersion)
      case "gentests" => 
        GenTable.genTest(new File(testTargetDir, "scala/gentables"), theVersion, theScalaVersion)
    }
  }
  
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
  
  val genFactories = TaskKey[Unit]("genfactories", "Generate Matcher Factories")
  val genFactoriesTask = genFactories <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenFactories.genMain(new File(mainTargetDir, "scala/genfactories"), theVersion, theScalaVersion)
  }

  val genCompatibleClasses = TaskKey[Unit]("gencompcls", "Generate Compatible Classes for Java 6 & 7")
  val genCompatibleClassesTask = genCompatibleClasses <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenCompatibleClasses.genMain(new File(mainTargetDir, "scala/gencompclass"), theVersion, theScalaVersion)
  }

  val genVersions = TaskKey[Unit]("genversions", "Generate Versions object")
  val genVersionsTask = genVersions <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenVersions.genScalaTestVersions(new File(mainTargetDir, "scala/gencompclass"), theVersion, theScalaVersion)
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

  val genCode = TaskKey[Unit]("gencode", "Generate Code, includes Must Matchers and They Word tests.")
  val genCodeTask = genCode <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenGen.genMain(new File(mainTargetDir, "scala/gengen"), theVersion, theScalaVersion)
    GenTable.genMain(new File(mainTargetDir, "scala/gentables"), theVersion, theScalaVersion)
    GenMatchers.genMain(new File(mainTargetDir, "scala/genmatchers"), theVersion, theScalaVersion)
    GenFactories.genMain(new File(mainTargetDir, "scala/genfactories"), theVersion, theScalaVersion)
  }

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

    if (projectName == "scalatest") {
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

  val docScalacOptionsSetting =
    scalacOptions in (Compile, doc) ++= 
      Seq[String](
        "-Ymacro-no-expand", // avoids need to separate out macros in docsrc dir
        "-sourcepath", docsrcDir.value.getAbsolutePath,
        "-doc-title", projectTitle.value +" "+ releaseVersion,
        "-doc-source-url", docSourceUrl)

  val docTaskSetting =
    doc in Compile := docTask((doc in Compile).value,
                              (sourceDirectory in Compile).value,
                              name.value)

  val scalacticDocTaskSetting =
    doc in Compile := docTask((doc in Compile).value,
                              (sourceDirectory in Compile).value,
                              name.value)
}
// set scalacOptions in (Compile, console) += "-Xlog-implicits"
// set scalacOptions in (Compile, console) += "-Xlog-implicits"
// set scalacOptions in (Compile, console) += "-Xlog-implicits"
// set scalacOptions in (Compile, console) += "-nowarn"
