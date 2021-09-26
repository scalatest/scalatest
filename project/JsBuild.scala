import sbt._
import Keys._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.{scalaJSLinkerConfig, jsEnv}
import com.typesafe.sbt.osgi.SbtOsgi
import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.sbt.osgi.SbtOsgi
import com.typesafe.sbt.osgi.SbtOsgi.autoImport._
import com.typesafe.tools.mima.plugin.MimaKeys.{mimaPreviousArtifacts, mimaCurrentClassfiles, mimaBinaryIssueFilters}
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

trait JsBuild { this: BuildCommons =>

  val scalaJSVersion = Option(System.getenv("SCALAJS_VERSION")).getOrElse("1.1.1")
  val sjsPrefix = if (scalaJSVersion.startsWith("1.")) "_sjs1_" else "_sjs0.6_"

  lazy val deleteJsDependenciesTask = taskKey[Unit]("Delete JS_DEPENDENCIES")

  def scalatestJSLibraryDependencies =
    Seq(
      "org.scala-js" %% "scalajs-test-interface" % scalaJSVersion
    )

  lazy val scalacticMacroJS = project.in(file("scalactic-macro.js"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Scalactic Macro.js",
      organization := "org.scalactic",
      sourceGenerators in Compile += {
        // We'll delete JS_DEPENDENCIES in scalactic-macro.js
        Def.task{
          GenScalacticJS.genMacroScala((sourceManaged in Compile).value, version.value, scalaVersion.value) ++
          ScalacticGenResourcesJSVM.genResources((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
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
      deleteJsDependenciesTask := Def.task {
        val jsDependenciesFile = (classDirectory in Compile).value
        (jsDependenciesFile/ "JS_DEPENDENCIES").delete()
        ()
        //val loader: ClassLoader = ClasspathUtilities.toLoader(classpath.map(_.data).map(_.getAbsoluteFile))
        //loader.loadClass("your.class.Here").newInstance()
      }.triggeredBy(compile in Compile).value,
      scalacOptions in (Compile, doc) := List.empty
    ).enablePlugins(ScalaJSPlugin)

  lazy val scalacticJS = project.in(file("scalactic.js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalacticDocSettings: _*)
    .settings(
      projectTitle := "Scalactic.js",
      organization := "org.scalactic",
      moduleName := "scalactic",
      sourceGenerators in Compile += {
        Def.task {
          GenScalacticJS.genScala((sourceManaged in Compile).value, version.value, scalaVersion.value) ++
          ScalacticGenResourcesJSVM.genFailureMessages((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenArrayHelper.genMain((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value)
        }.taskValue
      },
      resourceGenerators in Compile += {
        Def.task {
          GenScalacticJS.genResource((resourceManaged in Compile).value, version.value, scalaVersion.value) ++
          GenScalacticJS.genHtml((resourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification")),
      // include the macro classes and resources in the main jar
      mappings in (Compile, packageBin) ++= mappings.in(scalacticMacroJS, Compile, packageBin).value,
      // include the macro sources in the main source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalacticMacroJS, Compile, packageSrc).value,
      mimaPreviousArtifacts := Set(organization.value %%% moduleName.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar"), 
      mimaBinaryIssueFilters ++= {
        Seq(
          exclude[ReversedMissingMethodProblem]("org.scalactic.ObjectDiffer.diffImpl")  // New function in private object
        )
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

  lazy val scalatestJS = project.in(file("scalatest.js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest",
      organization := "org.scalatest",
      moduleName := "scalatest",
      initialCommands in console := """|import org.scalatest._
                                      |import org.scalactic._
                                      |import Matchers._""".stripMargin,
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + rootProject.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      libraryDependencies ++= scalatestJSLibraryDependencies,
      //jsDependencies += RuntimeDOM % "test",
      Compile / sourceGenerators += {
        Def.task {
          GenScalaTestJS.genScala((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value) ++
          GenVersions.genScalaTestVersions((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          ScalaTestGenResourcesJSVM.genFailureMessages((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          ScalaTestGenResourcesJSVM.genResources((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          GenConfigMap.genMain((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)
        }
      },
      javaSourceManaged := target.value / "java",
      managedSourceDirectories in Compile += javaSourceManaged.value,
      Compile / sourceGenerators += {
        Def.task{
          GenScalaTestJS.genJava((javaSourceManaged in Compile).value, version.value, scalaVersion.value)
        }
      },

      //unmanagedResourceDirectories in Compile <+= sourceManaged( _ / "resources" ),
      Compile / sourceGenerators += {
        Def.task{
          GenScalaTestJS.genHtml((resourceManaged in Compile).value, version.value, scalaVersion.value)
          GenTable.genMainForScalaJS((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          GenMatchers.genMainForScalaJS((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          GenFactories.genMainJS((sourceManaged in Compile).value / "scala" / "org" / "scalatest" / "matchers", version.value, scalaVersion.value)
          //GenSafeStyles.genMainForScalaJS((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)
        }
      },
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification")),
      scalatestJSDocTaskSetting,
      mimaPreviousArtifacts := Set(organization.value %%% moduleName.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar"), 
      mimaBinaryIssueFilters ++= {
       Seq(
         ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalatest.FailureMessages#wasNeverReady.apply"), // Generated function from error message bundle
         ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalatest.Resources.wasNeverReady"),  // Generated function from error message bundle
         ProblemFilters.exclude[DirectAbstractMethodProblem]("org.scalatest.concurrent.Futures#FutureConcept.futureValueImpl"), // Private function.
         ProblemFilters.exclude[DirectMissingMethodProblem]("org.scalatest.concurrent.Futures#FutureConcept.futureValueImpl"), // Private funciton, for Scala 2.11 and 2.10.
       )
     }
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest",
        "org.scalatest.compatible",
        "org.scalatest.concurrent",
        "org.scalatest.check",
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
        "org.scalatest.tags",
        "org.scalatest.tagobjects",
        "org.scalatest.time",
        "org.scalatest.tools",
        "org.scalatest.verb",
        "org.scalatest.words", 
        "org.scalatest.wordspec"
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

  lazy val scalatestAppJS = project.in(file("scalatest-app.js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest App",
      name := "scalatest-app",
      organization := "org.scalatest",
      moduleName := "scalatest-app",
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
      },
      mimaPreviousArtifacts := Set(organization.value %%% moduleName.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
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
    ).dependsOn(scalacticMacroJS % "compile-internal, test-internal", scalacticJS % "compile-internal", scalatestJS % "compile-internal").enablePlugins(ScalaJSPlugin)  

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
      "-m", "org.scalatest.events",
      "-m", "org.scalatest.prop",
      "-m", "org.scalatest.tools",
      "-m", "org.scalatest.matchers",
      "-m", "org.scalatest.matchers",
      "-m", "org.scalatest.matchers.should",
      "-m", "org.scalatest.matchers.must",
      "-m", "org.scalatest.matchers.dsl",
      "-m", "org.scalatest.verbs",
      "-m", "org.scalatest.suiteprop",
      "-m", "org.scalatest.path",
      "-m", "org.scalatest.exceptions",
      "-m", "org.scalatest.time",
      "-m", "org.scalatest.words",
      "-m", "org.scalatest.enablers",
      "-m", "org.scalatest.expectations",
      "-m", "org.scalatest.diagrams",
      "-oDIF"))  

  lazy val commonTestJS = project.in(file("common-test.js"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Common test classes used by scalactic.js and scalatest.js",
      //libraryDependencies ++= crossBuildTestLibraryDependencies.value,
      sourceGenerators in Compile += {
        Def.task{
          GenCommonTestJS.genMain((sourceManaged in Compile).value, version.value, scalaVersion.value) ++
          GenGen.genMain((sourceManaged in Compile).value / "scala" / "org" / "scalatest" / "prop", version.value, scalaVersion.value) ++
          GenCompatibleClasses.genTest((sourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      scalacOptions in (Compile, doc) := List.empty
    ).dependsOn(scalacticMacroJS, LocalProject("scalatestJS")).enablePlugins(ScalaJSPlugin)

  lazy val scalacticTestJS = project.in(file("scalactic-test.js"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Scalactic Test.js",
      organization := "org.scalactic",
      scalaJSLinkerConfig ~= { _.withOptimizer(false) },
      testOptions in Test ++=
        Seq(Tests.Argument(TestFrameworks.ScalaTest, "-oDIF")),
      //jsEnv := NodeJSEnv(executable = "node").value,
      //jsEnv := PhantomJSEnv().value,
      jsEnv := {
        import org.scalajs.jsenv.nodejs.NodeJSEnv
        new NodeJSEnv(
          NodeJSEnv.Config()
            .withArgs(List(/*"--max_new_space_size=3000", */"--max_old_space_size=3000")))
      },
      parallelExecution in Test := false,
      fork in Test := false,
      //Seq(Compile, Test).flatMap(c => inConfig(c)(jsEnv := RhinoJSEnv().value)), // to use rhino
      sourceGenerators in Test += {
        Def.task {
          GenScalacticJS.genTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification"))
    ).dependsOn(scalacticJS, scalatestJS % "test", commonTestJS % "test").enablePlugins(ScalaJSPlugin)

  def sharedTestSettingsJS: Seq[Setting[_]] = 
    Seq(
      organization := "org.scalatest",
      //jsDependencies += RuntimeDOM % "test",
      scalaJSLinkerConfig ~= { _.withOptimizer(false) },
      //jsEnv := NodeJSEnv(executable = "node").value,
      //jsEnv := PhantomJSEnv().value,
      jsEnv := {
        import org.scalajs.jsenv.nodejs.NodeJSEnv
        new NodeJSEnv(
          NodeJSEnv.Config()
            .withArgs(List(/*"--max_new_space_size=3000", */"--max_old_space_size=3000")))
      },
      //Seq(Compile, Test).flatMap(c => inConfig(c)(jsEnv := RhinoJSEnv().value)), // to use rhino
      testOptions in Test := scalatestTestJSOptions,
      parallelExecution in Test := false,
      fork in Test := false,
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification"))
    )

  lazy val scalatestTestJS = Project("scalatestTestJS", file("scalatest-test.js"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettingsJS: _*)
    .settings(
      projectTitle := "ScalaTest Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestJS.genTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      },
      sourceGenerators in Test +=
        Def.task {
          GenGen.genTestForJS((sourceManaged in Test).value, version.value, scalaVersion.value)
        },
      sourceGenerators in Test +=
        Def.task {
          GenMustMatchersTests.genTestForScalaJS((sourceManaged in Test).value, version.value, scalaVersion.value)
        }
    ).dependsOn(scalatestJS % "test", commonTestJS % "test").enablePlugins(ScalaJSPlugin)  

  val scalatestJSDocTaskSetting =
    doc in Compile := docTask((doc in Compile).value,
      (resourceManaged in Compile).value,
      name.value)

  lazy val examplesJS = project.in(file("examples.js"))
    .settings(
      scalaVersionsSettings,
      sourceGenerators in Test += {
        Def.task {
          GenExamplesJS.genScala((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalacticMacroJS, scalacticJS, scalatestJS).enablePlugins(ScalaJSPlugin)      

  lazy val scalatestCoreJS = project.in(file("modules/js/scalatest-core.js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Core JS",
      organization := "org.scalatest",
      moduleName := "scalatest-core",
      initialCommands in console := """|import org.scalatest._
                                      |import org.scalactic._
                                      |import Matchers._""".stripMargin,
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + rootProject.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      libraryDependencies ++= scalatestJSLibraryDependencies,
      //jsDependencies += RuntimeDOM % "test",
      Compile / sourceGenerators += {
        Def.task {
          GenModulesJS.genScalaTestCore((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value) ++
          GenVersions.genScalaTestVersions((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          ScalaTestGenResourcesJSVM.genFailureMessages((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          ScalaTestGenResourcesJSVM.genResources((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          GenConfigMap.genMain((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)
        }
      },
      javaSourceManaged := target.value / "java",
      managedSourceDirectories in Compile += javaSourceManaged.value,
      Compile / sourceGenerators += {
        Def.task{
          GenScalaTestJS.genJava((javaSourceManaged in Compile).value, version.value, scalaVersion.value)
        }
      },

      //unmanagedResourceDirectories in Compile <+= sourceManaged( _ / "resources" ),
      Compile / sourceGenerators += {
        Def.task{
          GenScalaTestJS.genHtml((resourceManaged in Compile).value, version.value, scalaVersion.value)
          GenTable.genMainForScalaJS((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)
          //GenSafeStyles.genMainForScalaJS((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)
        }
      },
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification")),
      scalatestJSDocTaskSetting,
      mimaPreviousArtifacts := Set(organization.value %%% moduleName.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest",
        "org.scalatest.compatible",
        "org.scalatest.concurrent",
        "org.scalatest.enablers",
        "org.scalatest.events",
        "org.scalatest.exceptions",
        "org.scalatest.fixture",
        "org.scalatest.verbs",
        "org.scalatest.prop",
        "org.scalatest.tags",
        "org.scalatest.tagobjects",
        "org.scalatest.time",
        "org.scalatest.tools"
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
        "Bundle-Name" -> "ScalaTest Core JS",
        "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc.",
        "Main-Class" -> "org.scalatest.tools.Runner"
      )
    ).dependsOn(scalacticMacroJS % "compile-internal, test-internal", scalacticJS).enablePlugins(ScalaJSPlugin)

  lazy val scalatestFeatureSpecJS = project.in(file("modules/js/scalatest-featurespec.js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest FeatureSpec JS",
      organization := "org.scalatest",
      name := "scalatest-featurespec",
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + rootProject.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      Compile / sourceGenerators += {
        Def.task {
          GenModulesJS.genScalaTestFeatureSpec((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }
      },
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification")),
      mimaPreviousArtifacts := Set(organization.value %%% moduleName.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.featurespec"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest FeatureSpec JS",
        "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCoreJS, scalacticMacroJS % "compile-internal, test-internal").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFlatSpecJS = project.in(file("modules/js/scalatest-flatspec.js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest FlatSpec JS",
      organization := "org.scalatest",
      name := "scalatest-flatspec",
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + rootProject.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      Compile / sourceGenerators += {
        Def.task {
          GenModulesJS.genScalaTestFlatSpec((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }
      },
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification")),
      mimaPreviousArtifacts := Set(organization.value %%% moduleName.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.flatspec"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest FlatSpec JS",
        "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCoreJS, scalacticMacroJS % "compile-internal, test-internal").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFreeSpecJS = project.in(file("modules/js/scalatest-freespec.js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest FreeSpec JS",
      organization := "org.scalatest",
      name := "scalatest-freespec",
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + rootProject.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      Compile / sourceGenerators += {
        Def.task {
          GenModulesJS.genScalaTestFreeSpec((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }
      },
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification")),
      mimaPreviousArtifacts := Set(organization.value %%% moduleName.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.freespec"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest FreeSpec JS",
        "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCoreJS, scalacticMacroJS % "compile-internal, test-internal").enablePlugins(ScalaJSPlugin)      

  lazy val scalatestFunSuiteJS = project.in(file("modules/js/scalatest-funsuite.js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest FunSuite JS",
      organization := "org.scalatest",
      name := "scalatest-funsuite",
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + rootProject.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      Compile / sourceGenerators += {
        Def.task {
          GenModulesJS.genScalaTestFunSuite((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }
      },
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification")),
      mimaPreviousArtifacts := Set(organization.value %%% moduleName.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.funsuite"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest FunSuite JS",
        "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCoreJS, scalacticMacroJS % "compile-internal, test-internal").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFunSpecJS = project.in(file("modules/js/scalatest-funspec.js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest FunSpec JS",
      organization := "org.scalatest",
      name := "scalatest-funspec",
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + rootProject.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      Compile / sourceGenerators += {
        Def.task {
          GenModulesJS.genScalaTestFunSpec((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }
      },
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification")),
      mimaPreviousArtifacts := Set(organization.value %%% moduleName.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.funspec"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest FunSpec JS",
        "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCoreJS, scalacticMacroJS % "compile-internal, test-internal").enablePlugins(ScalaJSPlugin)  

  lazy val scalatestPropSpecJS = project.in(file("modules/js/scalatest-propspec.js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest PropSpec JS",
      organization := "org.scalatest",
      name := "scalatest-propspec",
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + rootProject.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      Compile / sourceGenerators += {
        Def.task {
          GenModulesJS.genScalaTestPropSpec((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }
      },
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification")),
      mimaPreviousArtifacts := Set(organization.value %%% moduleName.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.propspec"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest PropSpec JS",
        "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCoreJS, scalacticMacroJS % "compile-internal, test-internal").enablePlugins(ScalaJSPlugin)

  lazy val scalatestWordSpecJS = project.in(file("modules/js/scalatest-wordspec.js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest WordSpec JS",
      organization := "org.scalatest",
      name := "scalatest-wordspec",
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + rootProject.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      Compile / sourceGenerators += {
        Def.task {
          GenModulesJS.genScalaTestWordSpec((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }
      },
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification")),
      mimaPreviousArtifacts := Set(organization.value %%% moduleName.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.wordspec"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest WordSpec JS",
        "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCoreJS, scalacticMacroJS % "compile-internal, test-internal").enablePlugins(ScalaJSPlugin)

  lazy val scalatestDiagramsJS = project.in(file("modules/js/scalatest-diagrams.js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest Diagrams JS",
      organization := "org.scalatest",
      name := "scalatest-diagrams",
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + rootProject.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      Compile / sourceGenerators += {
        Def.task {
          GenModulesJS.genScalaTestDiagrams((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }
      },
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification")),
      mimaPreviousArtifacts := Set(organization.value %%% moduleName.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.diagrams"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest Diagrams JS",
        "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalacticMacroJS % "compile-internal, test-internal", scalatestCoreJS).enablePlugins(ScalaJSPlugin)

  lazy val scalatestMatchersCoreJS = project.in(file("modules/js/scalatest-matchers-core.js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest Matchers Core JS",
      organization := "org.scalatest",
      name := "scalatest-matchers-core",
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + rootProject.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      Compile / sourceGenerators += {
        Def.task {
          GenModulesJS.genScalaTestMatchersCore((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value) ++ 
          GenFactories.genMainJS((sourceManaged in Compile).value / "scala" / "org" / "scalatest" / "matchers", version.value, scalaVersion.value)
        }
      },
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification")),
      mimaPreviousArtifacts := Set(organization.value %%% moduleName.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
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
        "Bundle-Name" -> "ScalaTest Matchers Core JS",
        "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalacticMacroJS % "compile-internal, test-internal", scalatestCoreJS).enablePlugins(ScalaJSPlugin)

  lazy val scalatestShouldMatchersJS = project.in(file("modules/js/scalatest-shouldmatchers.js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest Should Matchers JS",
      organization := "org.scalatest",
      name := "scalatest-shouldmatchers",
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + rootProject.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      Compile / sourceGenerators += {
        Def.task {
          GenModulesJS.genScalaTestShouldMatchers((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }
      },
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification")),
      mimaPreviousArtifacts := Set(organization.value %%% moduleName.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
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
        "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalacticMacroJS % "compile-internal, test-internal", scalatestMatchersCoreJS).enablePlugins(ScalaJSPlugin)

  lazy val scalatestMustMatchersJS = project.in(file("modules/js/scalatest-mustmatchers.js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest Must Matchers JS",
      organization := "org.scalatest",
      name := "scalatest-mustmatchers",
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + rootProject.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      Compile / sourceGenerators += {
        Def.task {
          GenMatchers.genMainForScalaJS((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)
        }
      },
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification")),
      mimaPreviousArtifacts := Set(organization.value %%% moduleName.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
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
        "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalacticMacroJS % "compile-internal, test-internal", scalatestMatchersCoreJS).enablePlugins(ScalaJSPlugin)    

  lazy val scalatestModulesJS = project.in(file("modules/js/modules-aggregation"))
    .settings(sharedSettings: _*)
    .settings(
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      scalacOptions in (Compile, doc) := List.empty
    ).aggregate(
      scalatestCoreJS, 
      scalatestFeatureSpecJS, 
      scalatestFlatSpecJS, 
      scalatestFreeSpecJS, 
      scalatestFunSuiteJS, 
      scalatestFunSpecJS, 
      scalatestPropSpecJS, 
      scalatestWordSpecJS, 
      scalatestDiagramsJS, 
      scalatestMatchersCoreJS, 
      scalatestShouldMatchersJS, 
      scalatestMustMatchersJS
    )            

}