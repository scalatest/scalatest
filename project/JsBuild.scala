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

  lazy val deleteJsDependenciesTask = taskKey[Unit]("Delete JS_DEPENDENCIES")

  lazy val scalacticMacroJS = project.in(file("js/scalactic-macro"))
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

  lazy val scalacticJS = project.in(file("js/scalactic"))
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
        "Bundle-Description" -> "Scalactic.js is an open-source library for Scala-js projects.",
        "Bundle-DocURL" -> "http://www.scalactic.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalacticMacroJS % "compile-internal, test-internal").enablePlugins(ScalaJSPlugin)

  lazy val scalatestAppJS = project.in(file("js/scalatest-app"))
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
    ).dependsOn(
      scalacticMacroJS % "compile-internal, test-internal", 
      scalacticJS % "compile-internal", 
      scalatestCoreJS % "compile-internal", 
      scalatestFeatureSpecJS % "compile-internal", 
      scalatestFlatSpecJS % "compile-internal", 
      scalatestFreeSpecJS % "compile-internal", 
      scalatestFunSuiteJS % "compile-internal", 
      scalatestFunSpecJS % "compile-internal", 
      scalatestPropSpecJS % "compile-internal", 
      scalatestWordSpecJS % "compile-internal", 
      scalatestDiagramsJS % "compile-internal", 
      scalatestMatchersCoreJS % "compile-internal", 
      scalatestShouldMatchersJS % "compile-internal", 
      scalatestMustMatchersJS % "compile-internal")
     .enablePlugins(ScalaJSPlugin)  

  lazy val commonTestJS = project.in(file("js/common-test"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Common test classes used by scalactic.js and scalatest.js",
      libraryDependencies ++= crossBuildTestLibraryDependencies.value,
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

  lazy val scalacticTestJS = project.in(file("js/scalactic-test"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Scalactic Test.js",
      organization := "org.scalactic",
      testOptions in Test ++=
        Seq(Tests.Argument(TestFrameworks.ScalaTest, "-oDIF")),
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
      testOptions in Test := scalatestTestJSNativeOptions,
      parallelExecution in Test := false,
      fork in Test := false,
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification"))
    )

  lazy val scalatestTestJS = project.in(file("js/scalatest-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettingsJS: _*)
    .settings(
      projectTitle := "ScalaTest Test",
      scalaJSLinkerConfig ~= { _.withOptimizer(false).withSemantics(_.withStrictFloats(true)) },
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
    ).dependsOn(commonTestJS % "test").enablePlugins(ScalaJSPlugin)
     .aggregate(
       scalatestDiagramsTestJS, 
       scalatestFeatureSpecTestJS, 
       scalatestFlatSpecTestJS, 
       scalatestFreeSpecTestJS, 
       scalatestFunSpecTestJS, 
       scalatestFunSuiteTestJS, 
       scalatestPropSpecTestJS, 
       scalatestWordSpecTestJS
     )

  lazy val scalatestDiagramsTestJS = project.in(file("js/diagrams-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettingsJS: _*)
    .settings(
      projectTitle := "ScalaTest Diagrams Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestJS.genDiagramsTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFeatureSpecTestJS = project.in(file("js/featurespec-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettingsJS: _*)
    .settings(
      projectTitle := "ScalaTest FeatureSpec Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestJS.genFeatureSpecTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFlatSpecTestJS = project.in(file("js/flatspec-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettingsJS: _*)
    .settings(
      projectTitle := "ScalaTest FlatSpec Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestJS.genFlatSpecTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFreeSpecTestJS = project.in(file("js/freespec-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettingsJS: _*)
    .settings(
      projectTitle := "ScalaTest FreeSpec Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestJS.genFreeSpecTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFunSpecTestJS = project.in(file("js/funspec-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettingsJS: _*)
    .settings(
      projectTitle := "ScalaTest FunSpec Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestJS.genFunSpecTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFunSuiteTestJS = project.in(file("js/funsuite-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettingsJS: _*)
    .settings(
      projectTitle := "ScalaTest FunSuite Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestJS.genFunSuiteTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestJS % "test").enablePlugins(ScalaJSPlugin)         

  lazy val scalatestPropSpecTestJS = project.in(file("js/propspec-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettingsJS: _*)
    .settings(
      projectTitle := "ScalaTest PropSpec Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestJS.genPropSpecTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestWordSpecTestJS = project.in(file("js/wordspec-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettingsJS: _*)
    .settings(
      projectTitle := "ScalaTest WordSpec Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestJS.genWordSpecTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestJS % "test").enablePlugins(ScalaJSPlugin)         

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

  lazy val scalatestCoreJS = project.in(file("js/core"))
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
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar"), 
      mimaBinaryIssueFilters ++= {
       Seq(
         exclude[DirectMissingMethodProblem]("org.scalatest.FailureMessages.flexmarkClassNotFound"), // Private class function 
         exclude[DirectMissingMethodProblem]("org.scalatest.Resources.rawFlexmarkClassNotFound"), // Private class function 
         exclude[DirectMissingMethodProblem]("org.scalatest.Resources.flexmarkClassNotFound") // Private class function
       )
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

  lazy val scalatestFeatureSpecJS = project.in(file("js/featurespec"))
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

  lazy val scalatestFlatSpecJS = project.in(file("js/flatspec"))
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

  lazy val scalatestFreeSpecJS = project.in(file("js/freespec"))
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

  lazy val scalatestFunSuiteJS = project.in(file("js/funsuite"))
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

  lazy val scalatestFunSpecJS = project.in(file("js/funspec"))
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

  lazy val scalatestPropSpecJS = project.in(file("js/propspec"))
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

  lazy val scalatestWordSpecJS = project.in(file("js/wordspec"))
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

  lazy val scalatestDiagramsJS = project.in(file("js/diagrams"))
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

  lazy val scalatestMatchersCoreJS = project.in(file("js/matchers-core"))
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

  lazy val scalatestShouldMatchersJS = project.in(file("js/shouldmatchers"))
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

  lazy val scalatestMustMatchersJS = project.in(file("js/mustmatchers"))
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

  lazy val scalatestJS = project.in(file("js/scalatest"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "ScalaTest JS",
      organization := "org.scalatest",
      name := "scalatest",
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + rootProject.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification")),
      mimaPreviousArtifacts := Set(organization.value %%% moduleName.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar"), 
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      }
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.privatePackage := Seq.empty, 
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest JS",
        "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(
      scalacticMacroJS % "compile-internal, test-internal", 
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
    ).enablePlugins(ScalaJSPlugin)  

}