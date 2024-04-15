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

  private lazy val sharedNativeSettings = Seq(
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
    resourceDirectories in Compile += (classDirectory in Compile).value
  )

  lazy val scalacticMacroNative = project.in(file("native/scalactic-macro"))
    .settings(sharedSettings ++ sharedNativeSettings)
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

  lazy val scalacticNative = project.in(file("native/scalactic"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ sharedNativeSettings)
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

  lazy val scalatestNative = project.in(file("native/scalatest"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ sharedNativeSettings)
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

  lazy val scalatestAppNative = project.in(file("scalatest-app.native"))
      .enablePlugins(SbtOsgi)
      .settings(sharedSettings ++ sharedNativeSettings)
      .settings(
        projectTitle := "ScalaTest App",
        name := "scalatest-app",
        organization := "org.scalatest",
        moduleName := "scalatest-app",
        libraryDependencies ++= nativeCrossBuildLibraryDependencies.value,
        libraryDependencies += "org.scala-native" %%% "test-interface" % nativeVersion,
        // include the scalactic classes and resources in the jar
        mappings in (Compile, packageBin) ++= mappings.in(scalacticNative, Compile, packageBin).value,
        // include the scalactic sources in the source jar
        mappings in (Compile, packageSrc) ++= mappings.in(scalacticNative, Compile, packageSrc).value,
        // include the scalatest classes and resources in the jar
        mappings in (Compile, packageBin) ++= mappings.in(scalatestNative, Compile, packageBin).value,
        // include the scalatest sources in the source jar
        mappings in (Compile, packageSrc) ++= mappings.in(scalatestNative, Compile, packageSrc).value,
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

  lazy val scalatestCoreNative = project.in(file("native/core"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ sharedNativeSettings)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest Core Native",
      organization := "org.scalatest",
      moduleName := "scalatest-core",
      libraryDependencies ++= nativeCrossBuildLibraryDependencies.value,
      libraryDependencies += "org.scala-native" %%% "test-interface" % nativeVersion,
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

  lazy val scalatestFeatureSpecNative = project.in(file("native/featurespec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ sharedNativeSettings)
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

  lazy val scalatestFlatSpecNative = project.in(file("native/flatspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ sharedNativeSettings)
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

  lazy val scalatestFreeSpecNative = project.in(file("native/freespec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ sharedNativeSettings)
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

  lazy val scalatestFunSuiteNative = project.in(file("native/funsuite"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ sharedNativeSettings)
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

  lazy val scalatestFunSpecNative = project.in(file("native/funspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ sharedNativeSettings)
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

  lazy val scalatestPropSpecNative = project.in(file("native/propspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ sharedNativeSettings)
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

  lazy val scalatestWordSpecNative = project.in(file("native/wordspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ sharedNativeSettings)
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

  lazy val scalatestDiagramsNative = project.in(file("native/diagrams"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ sharedNativeSettings)
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

  lazy val scalatestMatchersCoreNative = project.in(file("native/matchers-core"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ sharedNativeSettings)
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

  lazy val scalatestShouldMatchersNative = project.in(file("native/shouldmatchers"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ sharedNativeSettings)
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

  lazy val scalatestMustMatchersNative = project.in(file("native/mustmatchers"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings ++ sharedNativeSettings)
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

  lazy val commonTestNative = project.in(file("native/common-test"))
      .settings(sharedSettings ++ sharedNativeSettings)
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

  lazy val scalacticTestNative = project.in(file("native/scalactic-test"))
    .settings(sharedSettings ++ sharedNativeSettings)
    .settings(
      projectTitle := "Scalactic Test.native",
      organization := "org.scalactic",
      testOptions in Test ++=
        Seq(Tests.Argument(TestFrameworks.ScalaTest, "-oDIF")),
      sourceGenerators in Test += {
        Def.task {
          GenScalacticNative.genTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    ).dependsOn(scalacticNative, scalatestNative % "test", commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestTestNative = project.in(file("native/scalatest-test"))
    .settings(sharedSettings ++ sharedNativeSettings)
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
     .aggregate(
       scalatestDiagramsTestNative, 
       scalatestFeatureSpecTestNative, 
       scalatestFlatSpecTestNative, 
       scalatestFreeSpecTestNative, 
       scalatestFunSpecTestNative, 
       scalatestFunSuiteTestNative, 
       scalatestPropSpecTestNative, 
       scalatestWordSpecTestNative
    )

  lazy val scalatestDiagramsTestNative = project.in(file("native/diagrams-test"))
    .settings(sharedSettings ++ sharedNativeSettings)
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

  lazy val scalatestFeatureSpecTestNative = project.in(file("native/featurespec-test"))
    .settings(sharedSettings ++ sharedNativeSettings)
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

  lazy val scalatestFlatSpecTestNative = project.in(file("native/flatspec-test"))
    .settings(sharedSettings ++ sharedNativeSettings)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest FlatSpec Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestNative.genFlatSpecTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestFreeSpecTestNative = project.in(file("native/freespec-test"))
    .settings(sharedSettings ++ sharedNativeSettings)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest FreeSpec Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestNative.genFreeSpecTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestFunSpecTestNative = project.in(file("native/funspec-test"))
    .settings(sharedSettings ++ sharedNativeSettings)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest FunSpec Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestNative.genFunSpecTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestFunSuiteTestNative = project.in(file("native/funsuite-test"))
    .settings(sharedSettings ++ sharedNativeSettings)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest FunSuite Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestNative.genFunSuiteTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestPropSpecTestNative = project.in(file("native/propspec-test"))
    .settings(sharedSettings ++ sharedNativeSettings)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest PropSpec Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestNative.genPropSpecTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestWordSpecTestNative = project.in(file("native/wordspec-test"))
    .settings(sharedSettings ++ sharedNativeSettings)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest WordSpec Test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestNative.genWordSpecTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(commonTestNative % "test").enablePlugins(ScalaNativePlugin)  

  lazy val scalatestModulesNative = project.in(file("modules/native/modules-aggregation"))
    .settings(sharedSettings ++ sharedNativeSettings)
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
