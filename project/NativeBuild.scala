import sbt._
import Keys._
import scalanativecrossproject.ScalaNativeCrossPlugin.autoImport._

import scalanative.sbtplugin.ScalaNativePlugin
import scalanative.sbtplugin.ScalaNativePluginInternal.NativeTest
import ScalaNativePlugin.autoImport._

import com.typesafe.sbt.osgi.SbtOsgi
import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.sbt.osgi.SbtOsgi
import com.typesafe.sbt.osgi.SbtOsgi.autoImport._

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

trait NativeBuild { this: BuildCommons =>

  lazy val nativeCrossBuildLibraryDependencies = Def.setting {
    CrossVersion.partialVersion(scalaVersion.value) match {
      // if scala 2.11+ is used, add dependency on scala-xml module
      case Some((2, scalaMajor)) if scalaMajor >= 11 =>
        Seq(
          "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
        )
      case _ =>
        Seq.empty
    }
  }

  lazy val scalacticMacroNative = Project("scalacticMacroNative", file("native/scalactic-macro"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Scalactic Macro.native",
      organization := "org.scalactic",
      sourceGenerators in Compile += {
        Def.task{
          GenScalacticNative.genMacroScala((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value) ++
          ScalacticGenResourcesJSVM.genResources((sourceManaged in Compile).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenAnyVals.genMain((sourceManaged in Compile).value / "scala" / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value, false) ++
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
    ).enablePlugins(ScalaNativePlugin)

  lazy val scalacticNative = Project("scalacticNative", file("native/scalactic"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalacticDocSettings: _*)
    .settings(
      projectTitle := "Scalactic.native",
      organization := "org.scalactic",
      moduleName := "scalactic",
      sourceGenerators in Compile += {
        Def.task {
          GenScalacticNative.genScala((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value) ++
          ScalacticGenResourcesJSVM.genFailureMessages((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value) ++
          GenArrayHelper.genMain((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value)
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
  ).dependsOn(scalacticMacroNative % "compile-internal, test-internal").enablePlugins(ScalaNativePlugin)

  lazy val scalatestNative = Project("scalatestNative", file("native/scalatest"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Native",
      organization := "org.scalatest",
      moduleName := "scalatest",
      sourceGenerators in Compile += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      }
    ).settings(osgiSettings: _*).settings(
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest FlatSpec Native",
        "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(
      scalatestCoreNative,
      scalatestFeatureSpecNative,
      scalatestFlatSpecNative,
      scalatestFreeSpecNative,
      scalatestFunSuiteNative,
      scalatestFunSpecNative,
      scalatestPropSpecNative,
      scalatestWordSpecNative,
      scalatestDiagramsNative,
      scalatestMatchersCoreNative,
      scalatestShouldMatchersNative,
      scalatestMustMatchersNative
    ).aggregate(
      scalatestCoreNative,
      scalatestFeatureSpecNative,
      scalatestFlatSpecNative,
      scalatestFreeSpecNative,
      scalatestFunSuiteNative,
      scalatestFunSpecNative,
      scalatestPropSpecNative,
      scalatestWordSpecNative,
      scalatestDiagramsNative,
      scalatestMatchersCoreNative,
      scalatestShouldMatchersNative,
      scalatestMustMatchersNative
    ).enablePlugins(ScalaNativePlugin)

  lazy val scalatestAppNative = Project("scalatestAppNative", file("scalatest-app.native"))
      .enablePlugins(SbtOsgi)
      .settings(sharedSettings: _*)
      .settings(
        projectTitle := "ScalaTest App",
        name := "scalatest-app",
        organization := "org.scalatest",
        moduleName := "scalatest-app",
        libraryDependencies ++= nativeCrossBuildLibraryDependencies.value,
        libraryDependencies += "org.scala-native" %%% "test-interface" % "0.4.0-M2",
        // include the scalactic classes and resources in the jar
        mappings in (Compile, packageBin) ++= mappings.in(scalacticNative, Compile, packageBin).value,
        // include the scalactic sources in the source jar
        mappings in (Compile, packageSrc) ++= mappings.in(scalacticNative, Compile, packageSrc).value,
        // include the scalatest classes and resources in the jar
        mappings in (Compile, packageBin) ++= mappings.in(scalacticNative, Compile, packageBin).value,
        // include the scalatest sources in the source jar
        mappings in (Compile, packageSrc) ++= mappings.in(scalacticNative, Compile, packageSrc).value,
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
        scalacticMacroNative % "compile-internal, test-internal",
        scalacticNative % "compile-internal",
        scalatestCoreNative % "compile-internal",
        scalatestFeatureSpecNative % "compile-internal",
        scalatestFlatSpecNative % "compile-internal",
        scalatestFreeSpecNative % "compile-internal",
        scalatestFunSuiteNative % "compile-internal",
        scalatestFunSpecNative % "compile-internal",
        scalatestPropSpecNative % "compile-internal",
        scalatestWordSpecNative % "compile-internal",
        scalatestDiagramsNative % "compile-internal",
        scalatestMatchersCoreNative % "compile-internal",
        scalatestShouldMatchersNative % "compile-internal",
        scalatestMustMatchersNative % "compile-internal")
       .enablePlugins(ScalaNativePlugin)

  lazy val scalatestCoreNative = Project("scalatestCoreNative", file("native/core"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Core Native",
      organization := "org.scalatest",
      moduleName := "scalatest-core",
      libraryDependencies += "org.scala-native" %%% "test-interface" % "0.4.0-M2",
      sourceGenerators in Compile += {
        Def.task {
          GenScalaTestNative.genHtml((resourceManaged in Compile).value, version.value, scalaVersion.value)

          GenModulesNative.genScalaTestCore((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value) ++
          GenVersions.genScalaTestVersions((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          ScalaTestGenResourcesJSVM.genResources((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          ScalaTestGenResourcesJSVM.genFailureMessages((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          GenConfigMap.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      },
      javaSourceManaged := target.value / "java",
      managedSourceDirectories in Compile += javaSourceManaged.value,
      sourceGenerators in Compile += {
        Def.task{
          GenScalaTestNative.genJava((javaSourceManaged in Compile).value / "java", version.value, scalaVersion.value)
        }.taskValue
      },
      resourceGenerators in Compile += {
        Def.task {
          GenScalaTestNative.genHtml((resourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      sourceGenerators in Compile += {
        Def.task{
          GenTable.genMainForScalaJS((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
          //GenSafeStyles.genMainForScalaJS((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      },
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
      "Bundle-Name" -> "ScalaTest Core Native",
      "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc.",
      "Main-Class" -> "org.scalatest.tools.Runner"
    )
  ).dependsOn(scalacticMacroNative % "compile-internal, test-internal", scalacticNative).enablePlugins(ScalaNativePlugin)

  lazy val scalatestFeatureSpecNative = Project("scalatestFeatureSpecNative", file("native/featurespec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest FeatureSpec Native",
      organization := "org.scalatest",
      moduleName := "scalatest-featurespec",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesNative.genScalaTestFeatureSpec((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.featurespec"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest FeatureSpec Native",
      "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestCoreNative, scalacticMacroNative % "compile-internal, test-internal").enablePlugins(ScalaNativePlugin)

  lazy val scalatestFlatSpecNative = Project("scalatestFlatSpecNative", file("native/flatspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest FlatSpec Native",
      organization := "org.scalatest",
      moduleName := "scalatest-flatspec",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesNative.genScalaTestFlatSpec((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.flatspec"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest FlatSpec Native",
      "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestCoreNative, scalacticMacroNative % "compile-internal, test-internal").enablePlugins(ScalaNativePlugin)

  lazy val scalatestFreeSpecNative = Project("scalatestFreeSpecNative", file("native/freespec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest FreeSpec Native",
      organization := "org.scalatest",
      moduleName := "scalatest-freespec",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesNative.genScalaTestFreeSpec((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.freespec"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest FreeSpec Native",
      "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestCoreNative, scalacticMacroNative % "compile-internal, test-internal").enablePlugins(ScalaNativePlugin)

  lazy val scalatestFunSuiteNative = Project("scalatestFunSuiteNative", file("native/funsuite"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest FunSuite Native",
      organization := "org.scalatest",
      moduleName := "scalatest-funsuite",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesNative.genScalaTestFunSuite((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.funsuite"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest FunSuite Native",
      "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestCoreNative, scalacticMacroNative % "compile-internal, test-internal").enablePlugins(ScalaNativePlugin)

  lazy val scalatestFunSpecNative = Project("scalatestFunSpecNative", file("native/funspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest FunSpec Native",
      organization := "org.scalatest",
      moduleName := "scalatest-funspec",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesNative.genScalaTestFunSpec((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.funspec"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest FunSpec Native",
      "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestCoreNative, scalacticMacroNative % "compile-internal, test-internal").enablePlugins(ScalaNativePlugin)

  lazy val scalatestPropSpecNative = Project("scalatestPropSpecNative", file("native/propspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest PropSpec Native",
      organization := "org.scalatest",
      moduleName := "scalatest-propspec",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesNative.genScalaTestPropSpec((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.propspec"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest PropSpec Native",
      "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestCoreNative, scalacticMacroNative % "compile-internal, test-internal").enablePlugins(ScalaNativePlugin)

  lazy val scalatestWordSpecNative = Project("scalatestWordSpecNative", file("native/wordspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest WordSpec Native",
      organization := "org.scalatest",
      moduleName := "scalatest-wordspec",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesNative.genScalaTestWordSpec((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.wordspec"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest WordSpec Native",
      "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestCoreNative, scalacticMacroNative % "compile-internal, test-internal").enablePlugins(ScalaNativePlugin)

  lazy val scalatestDiagramsNative = Project("scalatestDiagramsNative", file("native/diagrams"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Diagrams Native",
      organization := "org.scalatest",
      moduleName := "scalatest-diagrams",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesNative.genScalaTestDiagrams((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.diagrams"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest Diagrams Native",
      "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalacticMacroNative % "compile-internal, test-internal", scalatestCoreNative).enablePlugins(ScalaNativePlugin)

  lazy val scalatestMatchersCoreNative = Project("scalatestMatchersCoreNative", file("native/matchers-core"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Matchers Core Native",
      organization := "org.scalatest",
      moduleName := "scalatest-matchers-core",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesNative.genScalaTestMatchersCore((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value) ++
          GenFactories.genMainJS((sourceManaged in Compile).value / "org" / "scalatest" / "matchers", version.value, scalaVersion.value)
        }.taskValue
      }
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
      "Bundle-Name" -> "ScalaTest Matchers Core Native",
      "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalacticMacroNative % "compile-internal, test-internal", scalatestCoreNative).enablePlugins(ScalaNativePlugin)

  lazy val scalatestShouldMatchersNative = Project("scalatestShouldMatchersNative", file("native/shouldmatchers"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Should Matchers Native",
      organization := "org.scalatest",
      moduleName := "scalatest-shouldmatchers",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesNative.genScalaTestShouldMatchers((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.matchers.should"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest Should Matchers Native",
      "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalacticMacroNative % "compile-internal, test-internal", scalatestMatchersCoreNative).enablePlugins(ScalaNativePlugin)

  lazy val scalatestMustMatchersNative = Project("scalatestMustMatchersNative", file("native/mustmatchers"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Must Matchers Native",
      organization := "org.scalatest",
      moduleName := "scalatest-mustmatchers",
      sourceGenerators in Compile += {
        Def.task {
          GenMatchers.genMainForScalaJS((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      }
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.matchers.must"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest Must Matchers Native",
      "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalacticMacroNative % "compile-internal, test-internal", scalatestMatchersCoreNative).enablePlugins(ScalaNativePlugin)

  def scalatestTestNativeOptions =
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
      "-m", "org.scalatest.featurespec",
      "-m", "org.scalatest.flatspec",
      "-m", "org.scalatest.freespec",
      "-oDIF"))

  lazy val commonTestNative = Project("commonTestNative", file("native/common-test"))
      .settings(sharedSettings: _*)
      .settings(
        projectTitle := "Common test classes used by scalactic.native and scalatest.native",
        sourceGenerators in Compile += {
          Def.task{
            GenCommonTestNative.genMain((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
            GenGen.genMain((sourceManaged in Compile).value / "scala" / "org" / "scalatest" / "prop", version.value, scalaVersion.value) ++
            GenCompatibleClasses.genTest((sourceManaged in Compile).value, version.value, scalaVersion.value)
          }.taskValue
        },
        publishArtifact := false,
        publish := {},
        publishLocal := {},
        scalacOptions in (Compile, doc) := List.empty
      ).dependsOn(scalacticMacroNative, LocalProject("scalatestNative")).enablePlugins(ScalaNativePlugin)

  lazy val scalacticTestNative = Project("scalacticTestNative", file("native/scalactic-test"))
    .settings(sharedSettings: _*)
    .settings(
      projectTitle := "Scalactic Test.native",
      organization := "org.scalactic",
      testOptions in Test ++=
        Seq(Tests.Argument(TestFrameworks.ScalaTest, "-oDIF")),
      /*nativeOptimizerDriver in NativeTest := {
        val orig = tools.OptimizerDriver((nativeConfig in NativeTest).value)
        orig.withPasses(orig.passes.filterNot(p => p == pass.DeadBlockElimination || p == pass.GlobalBoxingElimination))
      },*/
      nativeLinkStubs in NativeTest := true,
      sourceGenerators in Test += {
        Def.task {
          GenScalacticNative.genTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    ).dependsOn(scalacticNative, scalatestNative % "test", commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  def sharedTestSettingsNative: Seq[Setting[_]] =
    Seq(
      organization := "org.scalatest",
      libraryDependencies ++= nativeCrossBuildLibraryDependencies.value,
      // libraryDependencies += "io.circe" %%% "circe-parser" % "0.7.1" % "test",
      fork in test := false,
      /*nativeOptimizerDriver in NativeTest := {
        val orig = tools.OptimizerDriver((nativeConfig in NativeTest).value)
        orig.withPasses(orig.passes.filterNot(p => p == pass.DeadBlockElimination || p == pass.GlobalBoxingElimination))
      },
      nativeOptimizerReporter in NativeTest := new tools.OptimizerReporter {
        override def onStart(batchId: Int, batchDefns: Seq[scalanative.nir.Defn]): Unit = {
          println(s"start $batchId")
        }
        override def onPass(batchId: Int, passId: Int, pass: scala.scalanative.optimizer.Pass, batchDefns: Seq[scalanative.nir.Defn]): Unit = {
          println(s"$batchId ${pass.getClass.getSimpleName}")
        }
        override def onComplete(batchId: Int, batchDefns: Seq[scalanative.nir.Defn]): Unit = {
          println(s"end $batchId")
        }
      },*/
      nativeLinkStubs in NativeTest := true,
      testOptions in Test := scalatestTestNativeOptions,
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    )

  lazy val scalatestTestNative = Project("scalatestTestNative", file("native/scalatest-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestNative.genTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      },
      sourceGenerators in Test +=
        Def.task {
          GenGen.genTestForNative((sourceManaged in Test).value, version.value, scalaVersion.value)
        }/*,  // OOM even with 14gb heap size, will turn this one when 32gb machine is common or if newer scala-native use lesser memory.
      sourceGenerators in Test +=
        Def.task {
          GenMustMatchersTests.genTestForScalaNative((sourceManaged in Test).value, version.value, scalaVersion.value)
        }*/
    ).dependsOn(scalatestNative % "test", commonTestNative % "test")
     .enablePlugins(ScalaNativePlugin)

  lazy val scalatestDiagramsTestNative = Project("scalatestDiagramsTestNative", file("native/diagrams-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest Diagrams Test",
      nativeLink := file("test.hnir"),
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestNative.genDiagramsTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestFeatureSpecTestNative = Project("scalatestFeatureSpecTestNative", file("native/featurespec-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest FeatureSpec Test",
      nativeLink := file("test2.hnir"),
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestNative.genFeatureSpecTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestFlatSpecTestNative = Project("scalatestFlatSpecTestNative", file("native/flatspec-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest FlatSpec Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestNative.genFlatSpecTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestFreeSpecTestNative = Project("scalatestFreeSpecTestNative", file("native/freespec-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest FreeSpec Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestNative.genFreeSpecTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestFunSpecTestNative = Project("scalatestFunSpecTestNative", file("native/funspec-test"))
    .settings(sharedSettings: _*)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest FunSpec Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestNative.genFunSpecTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)          

  lazy val scalatestModulesNative = (project in file("modules/native/modules-aggregation"))
    .settings(sharedSettings: _*)
    .settings(
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      scalacOptions in (Compile, doc) := List.empty
    ).aggregate(
      scalatestCoreNative, 
      scalatestFeatureSpecNative, 
      scalatestFlatSpecNative, 
      scalatestFreeSpecNative, 
      scalatestFunSuiteNative, 
      scalatestFunSpecNative, 
      scalatestPropSpecNative, 
      scalatestWordSpecNative, 
      scalatestDiagramsNative, 
      scalatestMatchersCoreNative, 
      scalatestShouldMatchersNative, 
      scalatestMustMatchersNative
    )  

}