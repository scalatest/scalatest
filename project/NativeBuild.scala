import sbt._
import Keys._
import scalanativecrossproject.ScalaNativeCrossPlugin.autoImport._

import scalanative.sbtplugin.ScalaNativePlugin
import ScalaNativePlugin.autoImport._

import com.typesafe.sbt.osgi.SbtOsgi
import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.sbt.osgi.SbtOsgi
import com.typesafe.sbt.osgi.SbtOsgi.autoImport._

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

trait NativeBuild { this: BuildCommons =>

  private lazy val nativeSharedSettings = Seq(
    // This hack calls class directory as "resource" that forces to add all NIRs that was generated
    // by scala-native for classes that has `EnableReflectiveInstantiation` annotation
    // it requires because otherway all this NIRs is ignored by OSGI
    // and enduser will has a error like
    // [info] Linking (2152 ms)
    // [error] missing symbols:
    // [error] * M89org.scalatest.tools.FrameworkL29org.scalatest.tools.Framework$SN$ReflectivelyInstantiate$RE
    // [error]   - from M29org.scalatest.tools.FrameworkIE
    // [error] * T89org.scalatest.tools.FrameworkL29org.scalatest.tools.Framework$SN$ReflectivelyInstantiate$
    //
    // Details: https://github.com/scala-native/scala-native/issues/1930
    Compile / resourceDirectories += (Compile / classDirectory).value, 
    crossScalaVersions := Seq("2.13.16", "2.12.20")
  )

  lazy val scalacticMacroNative = project.in(file("native/scalactic-macro"))
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(
      projectTitle := "Scalactic Macro.native",
      organization := "org.scalactic",
      Compile / sourceGenerators += {
        Def.task{
          GenScalacticNative.genMacroScala((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
          ScalacticGenResourcesJSVM.genResources((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenAnyVals.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value, false) ++
          GenEvery.genMain((Compile / sourceManaged).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenColCompatHelper.genMain((Compile / sourceManaged).value / "org" / "scalactic", version.value, scalaVersion.value)
        }.taskValue
      },
      // Disable publishing macros directly, included in scalactic main jar
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      Compile / doc / scalacOptions := List.empty
    ).enablePlugins(ScalaNativePlugin)

  lazy val scalacticNative = project.in(file("native/scalactic"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(scalacticDocSettings: _*)
    .settings(
      projectTitle := "Scalactic.native",
      organization := "org.scalactic",
      moduleName := "scalactic",
      Compile / sourceGenerators += {
        Def.task {
          GenScalacticNative.genScala((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
          ScalacticGenResourcesJSVM.genFailureMessages((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
          GenArrayHelper.genMain((Compile / sourceManaged).value / "org" / "scalactic", version.value, scalaVersion.value) ++ 
          GenCompatibleClasses.genScalacticMain((Compile / sourceManaged).value / "org" / "scalactic", version.value, scalaVersion.value)
        }.taskValue
      },
      Compile / resourceGenerators += {
        Def.task {
          GenScalacticJS.genResource((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value)
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

  lazy val scalatestNative = project.in(file("native/scalatest"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Native",
      organization := "org.scalatest",
      moduleName := "scalatest",
      Compile / sourceGenerators += {
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
      scalatestExpectationsNative,
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
      scalatestExpectationsNative,
      scalatestMatchersCoreNative,
      scalatestShouldMatchersNative,
      scalatestMustMatchersNative
    ).enablePlugins(ScalaNativePlugin)

  lazy val scalatestAppNative = project.in(file("scalatest-app.native"))
      .enablePlugins(SbtOsgi)
      .settings(sharedSettings ++ nativeSharedSettings)
      .settings(
        projectTitle := "ScalaTest App",
        name := "scalatest-app",
        organization := "org.scalatest",
        moduleName := "scalatest-app",
        libraryDependencies ++= nativeCrossBuildLibraryDependencies.value,
        libraryDependencies += "org.scala-native" %%% "test-interface" % nativeVersion,
        // include the scalactic classes and resources in the jar
        Compile / packageBin / mappings ++= (scalacticNative / Compile / packageBin / mappings).value,
        // include the scalactic sources in the source jar
        Compile / packageSrc / mappings ++= (scalacticNative / Compile / packageSrc / mappings).value,
        // include the scalatest classes and resources in the jar
        Compile / packageBin / mappings ++= (scalacticNative / Compile / packageBin / mappings).value,
        // include the scalatest sources in the source jar
        Compile / packageSrc / mappings ++= (scalacticNative / Compile / packageSrc / mappings).value,
        Compile / sourceGenerators += {
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
        scalatestExpectationsNative % "compile-internal",
        scalatestMatchersCoreNative % "compile-internal",
        scalatestShouldMatchersNative % "compile-internal",
        scalatestMustMatchersNative % "compile-internal")
       .enablePlugins(ScalaNativePlugin)

  lazy val scalatestCoreNative = project.in(file("native/core"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Core Native",
      organization := "org.scalatest",
      moduleName := "scalatest-core",
      libraryDependencies ++= nativeCrossBuildLibraryDependencies.value,
      libraryDependencies += "org.scala-native" %%% "test-interface" % nativeVersion,
      Compile / sourceGenerators += {
        Def.task {
          GenScalaTestNative.genHtml((Compile / resourceManaged).value, version.value, scalaVersion.value)

          GenModulesNative.genScalaTestCore((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
          GenVersions.genScalaTestVersions((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          ScalaTestGenResourcesJSVM.genResources((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          ScalaTestGenResourcesJSVM.genFailureMessages((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          GenGen.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalatest" / "prop", version.value, scalaVersion.value) ++
          GenConfigMap.genMain((Compile / sourceManaged).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      },
      javaSourceManaged := target.value / "java",
      Compile / managedSourceDirectories += javaSourceManaged.value,
      Compile / sourceGenerators += {
        Def.task{
          GenScalaTestNative.genJava((Compile / javaSourceManaged).value / "java", version.value, scalaVersion.value)
        }.taskValue
      },
      Compile / resourceGenerators += {
        Def.task {
          GenScalaTestNative.genHtml((Compile / resourceManaged).value, version.value, scalaVersion.value)
        }.taskValue
      },
      Compile / sourceGenerators += {
        Def.task{
          GenTable.genMainForScalaJS((Compile / sourceManaged).value / "org" / "scalatest", version.value, scalaVersion.value)
          //GenSafeStyles.genMainForScalaJS((Compile / sourceManaged).value / "org" / "scalatest", version.value, scalaVersion.value)
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

  lazy val scalatestFeatureSpecNative = project.in(file("native/featurespec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest FeatureSpec Native",
      organization := "org.scalatest",
      moduleName := "scalatest-featurespec",
      Compile / sourceGenerators += {
        Def.task {
          GenModulesNative.genScalaTestFeatureSpec((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++ 
          GenSafeStyles.genFeatureSpec((Compile / sourceManaged).value / "org" / "scalatest" / "featurespec", version.value, scalaVersion.value, true)
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

  lazy val scalatestFlatSpecNative = project.in(file("native/flatspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest FlatSpec Native",
      organization := "org.scalatest",
      moduleName := "scalatest-flatspec",
      Compile / sourceGenerators += {
        Def.task {
          GenModulesNative.genScalaTestFlatSpec((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++ 
          GenSafeStyles.genFlatSpec((Compile / sourceManaged).value / "org" / "scalatest" / "flatspec", version.value, scalaVersion.value, true)
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

  lazy val scalatestFreeSpecNative = project.in(file("native/freespec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest FreeSpec Native",
      organization := "org.scalatest",
      moduleName := "scalatest-freespec",
      Compile / sourceGenerators += {
        Def.task {
          GenModulesNative.genScalaTestFreeSpec((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
          GenSafeStyles.genFreeSpec((Compile / sourceManaged).value / "org" / "scalatest" / "freespec", version.value, scalaVersion.value, true)
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

  lazy val scalatestFunSuiteNative = project.in(file("native/funsuite"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest FunSuite Native",
      organization := "org.scalatest",
      moduleName := "scalatest-funsuite",
      Compile / sourceGenerators += {
        Def.task {
          GenModulesNative.genScalaTestFunSuite((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
          GenSafeStyles.genFunSuite((Compile / sourceManaged).value / "org" / "scalatest" / "funsuite", version.value, scalaVersion.value, true)
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

  lazy val scalatestFunSpecNative = project.in(file("native/funspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest FunSpec Native",
      organization := "org.scalatest",
      moduleName := "scalatest-funspec",
      Compile / sourceGenerators += {
        Def.task {
          GenModulesNative.genScalaTestFunSpec((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++ 
          GenSafeStyles.genFunSpec((Compile / sourceManaged).value / "org" / "scalatest" / "funspec", version.value, scalaVersion.value, true)
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

  lazy val scalatestPropSpecNative = project.in(file("native/propspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest PropSpec Native",
      organization := "org.scalatest",
      moduleName := "scalatest-propspec",
      Compile / sourceGenerators += {
        Def.task {
          GenModulesNative.genScalaTestPropSpec((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++ 
          GenSafeStyles.genPropSpec((Compile / sourceManaged).value / "org" / "scalatest" / "propspec", version.value, scalaVersion.value, true)
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

  lazy val scalatestWordSpecNative = project.in(file("native/wordspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest WordSpec Native",
      organization := "org.scalatest",
      moduleName := "scalatest-wordspec",
      Compile / sourceGenerators += {
        Def.task {
          GenModulesNative.genScalaTestWordSpec((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++ 
          GenSafeStyles.genWordSpec((Compile / sourceManaged).value / "org" / "scalatest" / "wordspec", version.value, scalaVersion.value, true)
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

  lazy val scalatestDiagramsNative = project.in(file("native/diagrams"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Diagrams Native",
      organization := "org.scalatest",
      moduleName := "scalatest-diagrams",
      Compile / sourceGenerators += {
        Def.task {
          GenModulesNative.genScalaTestDiagrams((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value)
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

  lazy val scalatestExpectationsNative = project.in(file("native/expectations"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Expectations Native",
      organization := "org.scalatest",
      moduleName := "scalatest-expectations",
      Compile / sourceGenerators += {
        Def.task {
          GenModulesNative.genScalaTestExpectations((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.expectations"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest Expectations Native",
      "Bundle-Description" -> "ScalaTest.js is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalacticMacroNative % "compile-internal, test-internal", scalatestCoreNative).enablePlugins(ScalaNativePlugin)

  lazy val scalatestMatchersCoreNative = project.in(file("native/matchers-core"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Matchers Core Native",
      organization := "org.scalatest",
      moduleName := "scalatest-matchers-core",
      Compile / sourceGenerators += {
        Def.task {
          GenModulesNative.genScalaTestMatchersCore((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
          GenFactories.genMainJS((Compile / sourceManaged).value / "org" / "scalatest" / "matchers", version.value, scalaVersion.value)
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

  lazy val scalatestShouldMatchersNative = project.in(file("native/shouldmatchers"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Should Matchers Native",
      organization := "org.scalatest",
      moduleName := "scalatest-shouldmatchers",
      Compile / sourceGenerators += {
        Def.task {
          GenModulesNative.genScalaTestShouldMatchers((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value)
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

  lazy val scalatestMustMatchersNative = project.in(file("native/mustmatchers"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Must Matchers Native",
      organization := "org.scalatest",
      moduleName := "scalatest-mustmatchers",
      Compile / sourceGenerators += {
        Def.task {
          GenMatchers.genMainForScalaJS((Compile / sourceManaged).value / "org" / "scalatest", version.value, scalaVersion.value)
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

  lazy val commonTestNative = project.in(file("native/common-test"))
      .settings(sharedSettings ++ nativeSharedSettings)
      .settings(
        projectTitle := "Common test classes used by scalactic.native and scalatest.native",
        Compile / sourceGenerators += {
          Def.task{
            GenCommonTestNative.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
            GenCompatibleClasses.genTest((Compile / sourceManaged).value, version.value, scalaVersion.value)
          }.taskValue
        },
        publishArtifact := false,
        publish := {},
        publishLocal := {},
        Compile / doc / scalacOptions := List.empty
      ).dependsOn(scalacticMacroNative, LocalProject("scalatestNative")).enablePlugins(ScalaNativePlugin)

  lazy val scalacticTestNative = project.in(file("native/scalactic-test"))
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(
      projectTitle := "Scalactic Test.native",
      organization := "org.scalactic",
      Test / testOptions ++=
        Seq(Tests.Argument(TestFrameworks.ScalaTest, "-oDIF")),
      Test / sourceGenerators += {
        Def.task {
          GenScalacticNative.genTest((Test / sourceManaged).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    ).dependsOn(scalacticNative, scalatestNative % "test", commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestTestNative = project.in(file("native/scalatest-test"))
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest Test",
      Test / sourceGenerators += {
        Def.task {
          GenScalaTestNative.genTest((Test / sourceManaged).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      },
      Test / sourceGenerators +=
        Def.task {
          GenGen.genTestForNative((Test / sourceManaged).value, version.value, scalaVersion.value)
        }/*,  // OOM even with 14gb heap size, will turn this one when 32gb machine is common or if newer scala-native use lesser memory.
      Test / sourceGenerators +=
        Def.task {
          GenMustMatchersTests.genTestForScalaNative((Test / sourceManaged).value, version.value, scalaVersion.value)
        }*/
    ).dependsOn(scalatestNative % "test", commonTestNative % "test")
     .enablePlugins(ScalaNativePlugin)
     .aggregate(
       scalatestDiagramsTestNative, 
       scalatestExpectationsTestNative, 
       scalatestFeatureSpecTestNative, 
       scalatestFlatSpecTestNative, 
       scalatestFreeSpecTestNative, 
       scalatestFunSpecTestNative, 
       scalatestFunSuiteTestNative, 
       scalatestPropSpecTestNative, 
       scalatestWordSpecTestNative
    )

  lazy val scalatestDiagramsTestNative = project.in(file("native/diagrams-test"))
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest Diagrams Test",
      nativeLink := file("test.hnir"),
      Test / sourceGenerators += {
        Def.task {
          GenScalaTestNative.genDiagramsTest((Test / sourceManaged).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestExpectationsTestNative = project.in(file("native/expectations-test"))
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest Expectations Test",
      nativeLink := file("test.hnir"),
      Test / sourceGenerators += {
        Def.task {
          GenScalaTestNative.genExpectationsTest((Test / sourceManaged).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)  

  lazy val scalatestFeatureSpecTestNative = project.in(file("native/featurespec-test"))
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest FeatureSpec Test",
      nativeLink := file("test2.hnir"),
      Test / sourceGenerators += {
        Def.task {
          GenScalaTestNative.genFeatureSpecTest((Test / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
          GenSafeStyles.genFeatureSpecTest((Compile / sourceManaged).value / "org" / "scalatest" / "featurespec", version.value, scalaVersion.value, true)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestFlatSpecTestNative = project.in(file("native/flatspec-test"))
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest FlatSpec Test",
      Test / sourceGenerators += {
        Def.task {
          GenScalaTestNative.genFlatSpecTest((Test / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
          GenSafeStyles.genFlatSpecTest((Compile / sourceManaged).value / "org" / "scalatest" / "flatspec", version.value, scalaVersion.value, true)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestFreeSpecTestNative = project.in(file("native/freespec-test"))
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest FreeSpec Test",
      Test / sourceGenerators += {
        Def.task {
          GenScalaTestNative.genFreeSpecTest((Test / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
          GenSafeStyles.genFreeSpecTest((Compile / sourceManaged).value / "org" / "scalatest" / "freespec", version.value, scalaVersion.value, true)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestFunSpecTestNative = project.in(file("native/funspec-test"))
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest FunSpec Test",
      Test / sourceGenerators += {
        Def.task {
          GenScalaTestNative.genFunSpecTest((Test / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
          GenSafeStyles.genFunSpecTest((Test / sourceManaged).value / "scala" / "org" / "scalatest" / "funspec", version.value, scalaVersion.value, true)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestFunSuiteTestNative = project.in(file("native/funsuite-test"))
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest FunSuite Test",
      Test / sourceGenerators += {
        Def.task {
          GenScalaTestNative.genFunSuiteTest((Test / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
          GenSafeStyles.genFunSuiteTest((Test / sourceManaged).value / "scala" / "org" / "scalatest" / "funsuite", version.value, scalaVersion.value, true)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestPropSpecTestNative = project.in(file("native/propspec-test"))
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest PropSpec Test",
      Test / sourceGenerators += {
        Def.task {
          GenScalaTestNative.genPropSpecTest((Test / sourceManaged).value / "scala", version.value, scalaVersion.value) ++ 
          GenSafeStyles.genPropSpecTest((Compile / sourceManaged).value / "org" / "scalatest" / "propspec", version.value, scalaVersion.value, true)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestWordSpecTestNative = project.in(file("native/wordspec-test"))
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest WordSpec Test",
      Test / sourceGenerators += {
        Def.task {
          GenScalaTestNative.genWordSpecTest((Test / sourceManaged).value / "scala", version.value, scalaVersion.value) ++ 
          GenSafeStyles.genWordSpecTest((Compile / sourceManaged).value / "org" / "scalatest" / "wordspec", version.value, scalaVersion.value, true)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)  

  lazy val scalatestModulesNative = project.in(file("modules/native/modules-aggregation"))
    .settings(sharedSettings ++ nativeSharedSettings)
    .settings(
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      Compile / doc / scalacOptions := List.empty
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
      scalatestExpectationsNative, 
      scalatestMatchersCoreNative, 
      scalatestShouldMatchersNative, 
      scalatestMustMatchersNative
    )    

}
