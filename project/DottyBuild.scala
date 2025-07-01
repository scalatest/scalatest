import scalanative.sbtplugin.ScalaNativePlugin.autoImport.nativeVersion
import sbt._
import Keys._
import com.typesafe.tools.mima.plugin.MimaKeys.{mimaPreviousArtifacts, mimaCurrentClassfiles, mimaBinaryIssueFilters}
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._
import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.sbt.osgi.SbtOsgi
import com.typesafe.sbt.osgi.SbtOsgi.autoImport._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.{scalaJSLinkerConfig, jsEnv, scalaJSVersion}

import scalanative.sbtplugin.ScalaNativePlugin

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

trait DottyBuild { this: BuildCommons =>

  // List of available night build at https://repo1.maven.org/maven2/ch/epfl/lamp/dotty-compiler_0.27/
  // lazy val dottyVersion = dottyLatestNightlyBuild.get
  lazy val dottyVersion = System.getProperty("scalatest.dottyVersion", "3.3.6")
  lazy val dottySettings = List(
    scalaVersion := dottyVersion,
    scalacOptions ++= List("-language:implicitConversions", "-noindent", "-Xprint-suspension")
  )

  // https://github.com/sbt/sbt/issues/2205#issuecomment-144375501
  private lazy val packageManagedSources =
    Compile / packageSrc / mappings ++= { // publish generated sources
      val srcs = (Compile / managedSources).value
      val sdirs = (Compile / managedSourceDirectories).value
      val base = baseDirectory.value
      import Path._
      (srcs --- sdirs --- base) pair (relativeTo(sdirs) | relativeTo(base) | flat)
    }

  lazy val scalacticDotty = project.in(file("dotty/scalactic"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(scalacticDocSettings: _*)
    .settings(
      projectTitle := "Scalactic",
      organization := "org.scalactic",
      moduleName := "scalactic",
      console / initialCommands := "import org.scalactic._",
      packageManagedSources,
      Compile / sourceGenerators += {
        Def.task {
          // From scalactic-macro
          GenScalacticDotty.genMacroScala((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
          ScalacticGenResourcesJVM.genResources((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenAnyVals.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value, true) ++
          GenEvery.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenColCompatHelper.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          // end from scalactic-macro
          GenScalacticDotty.genScala((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
          GenVersions.genScalacticVersions((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          ScalacticGenResourcesJVM.genFailureMessages((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenArrayHelper.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++ 
          GenCompatibleClasses.genScalacticMain((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value)
        }.taskValue
      },
      Compile / resourceGenerators += Def.task {
        GenScalacticDotty.genResource((Compile / resourceManaged).value)
      }.taskValue,
      //scalacticDocSourcesSetting,
      //docTaskSetting,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (Compile / classDirectory).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
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
  )

  lazy val scalacticDottyJS = project.in(file("dotty/scalactic.js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(scalacticDocSettings: _*)
    .settings(
      projectTitle := "Scalactic",
      organization := "org.scalactic",
      moduleName := "scalactic",
      console / initialCommands := "import org.scalactic._",
      packageManagedSources,
      Compile / sourceGenerators += {
        Def.task {
          // From scalactic-macro
          GenScalacticDotty.genMacroScala((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
          ScalacticGenResourcesJSVM.genResources((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenAnyVals.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value, true) ++
          GenEvery.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenColCompatHelper.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          // end from scalactic-macro
          GenScalacticDotty.genScalaJS((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
          GenVersions.genScalacticVersions((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          ScalacticGenResourcesJSVM.genFailureMessages((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenArrayHelper.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++ 
          GenCompatibleClasses.genScalacticMain((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value)
        }.taskValue
      },
      Compile / resourceGenerators += Def.task {
        GenScalacticDotty.genResource((Compile / resourceManaged).value)
      }.taskValue,
      //scalacticDocSourcesSetting,
      //docTaskSetting,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (Compile / classDirectory).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
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
  ).enablePlugins(ScalaJSPlugin)

  lazy val scalacticDottyNative = project.in(file("dotty/scalactic.native"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(scalacticDocSettings: _*)
    .settings(
      projectTitle := "Scalactic",
      organization := "org.scalactic",
      moduleName := "scalactic",
      console / initialCommands := "import org.scalactic._",
      packageManagedSources,
      Compile / sourceGenerators += {
        Def.task {
          // From scalactic-macro
          GenScalacticDotty.genMacroScala((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
          ScalacticGenResourcesJSVM.genResources((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenAnyVals.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value, true) ++
          GenEvery.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenColCompatHelper.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          // end from scalactic-macro
          GenScalacticDotty.genScalaNative((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++ 
          GenVersions.genScalacticVersions((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          ScalacticGenResourcesJSVM.genFailureMessages((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenArrayHelper.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value) ++ 
          GenCompatibleClasses.genScalacticMain((Compile / sourceManaged).value / "scala" / "org" / "scalactic", version.value, scalaVersion.value)
        }.taskValue
      },
      Compile / resourceGenerators += Def.task {
        GenScalacticDotty.genResource((Compile / resourceManaged).value)
      }.taskValue,
      //scalacticDocSourcesSetting,
      //docTaskSetting,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (Compile / classDirectory).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
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
  ).enablePlugins(ScalaNativePlugin)

  lazy val scalatestCoreDotty = project.in(file("dotty/core"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest Core Dotty",
      organization := "org.scalatest",
      moduleName := "scalatest-core",
      console / initialCommands := """|import org.scalatest._
                                       |import org.scalactic._
                                       |import Matchers._""".stripMargin,
      libraryDependencies ++= scalaXmlDependency(scalaVersion.value),
      libraryDependencies ++= scalatestLibraryDependencies,
      packageManagedSources,
      Compile / sourceGenerators += 
        Def.task {
          GenModulesDotty.genScalaTestCore((Compile / sourceManaged).value, version.value, scalaVersion.value) ++
          GenVersions.genScalaTestVersions((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          ScalaTestGenResourcesJVM.genResources((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
          ScalaTestGenResourcesJVM.genFailureMessages((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)  ++
          GenGen.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalatest" / "prop", version.value, scalaVersion.value) ++
          GenConfigMap.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue,
      javaSourceManaged := target.value / "java",
      Compile / managedSourceDirectories += javaSourceManaged.value,
      Compile / sourceGenerators += Def.task {
        GenScalaTestDotty.genJava((Compile / javaSourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
      Compile / resourceGenerators += Def.task {
          GenScalaTestDotty.genHtml((Compile / resourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
      Compile / sourceGenerators += Def.task {
        GenTable.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
        GenCompatibleClasses.genScalaTestMain((Compile / sourceManaged).value / "scala" / "org" / "scalatest" / "tools", version.value, scalaVersion.value)
        //GenSafeStyles.genMain((Compile / sourceManaged).value / "org" / "scalatest", version.value, scalaVersion.value)
      }.taskValue,
      //scalatestJSDocTaskSetting,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (Compile / classDirectory).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar"),
      mimaBinaryIssueFilters ++= {
        Seq(
          exclude[MissingClassProblem]("org.scalatest.tools.SbtCommandParser$"),
          exclude[MissingClassProblem]("org.scalatest.tools.SbtCommandParser")
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
      "org.scalactic.*",
      "scala.util.parsing.*;version=\"$<range;[==,=+);$<replace;1.0.4;-;.>>\"",
      "scala.xml.*;version=\"$<range;[==,=+);$<replace;1.0.4;-;.>>\"",
      "scala.*;version=\"$<range;[==,=+);$<replace;"+scalaBinaryVersion.value+";-;.>>\"",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest Core Dotty",
      "Bundle-Description" -> "ScalaTest is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc.",
      "Main-Class" -> "org.scalatest.tools.Runner"
    )
  ).dependsOn(scalacticDotty, scalatestCompatible)

  lazy val scalatestCoreDottyJS = project.in(file("dotty/core.js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest Core Dotty",
      organization := "org.scalatest",
      moduleName := "scalatest-core",
      console / initialCommands := """|import org.scalatest._
                                       |import org.scalactic._
                                       |import Matchers._""".stripMargin,
      libraryDependencies += "org.scala-lang.modules" %%% "scala-xml" % "2.1.0", 
      libraryDependencies ++= scalatestJSLibraryDependencies.value, 
      packageManagedSources,
      Compile / sourceGenerators += Def.task {
        GenModulesDotty.genScalaTestCoreJS((Compile / sourceManaged).value, version.value, scalaVersion.value) ++
        GenScalaTestDotty.genScalaJS((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
        GenVersions.genScalaTestVersions((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
        ScalaTestGenResourcesJSVM.genResources((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
        ScalaTestGenResourcesJSVM.genFailureMessages((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)  ++
        GenGen.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalatest" / "prop", version.value, scalaVersion.value) ++
        GenConfigMap.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)
      }.taskValue,
      javaSourceManaged := target.value / "java",
      Compile / managedSourceDirectories += javaSourceManaged.value,
      Compile / sourceGenerators += Def.task {
        GenScalaTestDotty.genJava((Compile / javaSourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
      Compile / resourceGenerators += Def.task {
          GenScalaTestDotty.genHtml((Compile / resourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
      Compile / sourceGenerators += Def.task {
        GenTable.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)
      }.taskValue,
      //scalatestJSDocTaskSetting,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (Compile / classDirectory).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar"),
      mimaBinaryIssueFilters ++= {
        Seq(
          exclude[MissingClassProblem]("org.scalatest.tools.SbtCommandParser$"),
          exclude[MissingClassProblem]("org.scalatest.tools.SbtCommandParser")
        )
      }
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest", 
      "org.scalatest.compatible", 
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
      "org.scalactic.*",
      "scala.util.parsing.*;version=\"$<range;[==,=+);$<replace;1.0.4;-;.>>\"",
      "scala.xml.*;version=\"$<range;[==,=+);$<replace;1.0.4;-;.>>\"",
      "scala.*;version=\"$<range;[==,=+);$<replace;"+scalaBinaryVersion.value+";-;.>>\"",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest Core Dotty",
      "Bundle-Description" -> "ScalaTest is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc.",
      "Main-Class" -> "org.scalatest.tools.Runner"
    )
  ).dependsOn(scalacticDottyJS).enablePlugins(ScalaJSPlugin)

  lazy val scalatestCoreDottyNative = project.in(file("dotty/core.native"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest Core Dotty",
      organization := "org.scalatest",
      moduleName := "scalatest-core",
      console / initialCommands := """|import org.scalatest._
                                       |import org.scalactic._
                                       |import Matchers._""".stripMargin,
      libraryDependencies += "org.scala-lang.modules" %%% "scala-xml" % "2.3.0",
      libraryDependencies += ("org.scala-native" %%% "test-interface" % nativeVersion),
      packageManagedSources,
      Compile / sourceGenerators += Def.task {
        GenScalaTestDotty.genScalaNative((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
        GenModulesDotty.genScalaTestCoreNative((Compile / sourceManaged).value, version.value, scalaVersion.value) ++
        GenVersions.genScalaTestVersions((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
        ScalaTestGenResourcesJSVM.genResources((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value) ++
        ScalaTestGenResourcesJSVM.genFailureMessages((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)  ++
        GenGen.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalatest" / "prop", version.value, scalaVersion.value) ++
        GenConfigMap.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)
      }.taskValue,
      javaSourceManaged := target.value / "java",
      Compile / managedSourceDirectories += javaSourceManaged.value,
      Compile / sourceGenerators += Def.task {
        GenScalaTestDotty.genJava((Compile / javaSourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
      Compile / resourceGenerators += Def.task {
          GenScalaTestDotty.genHtml((Compile / resourceManaged).value / "scala", version.value, scalaVersion.value)
      }.taskValue,
      Compile / sourceGenerators += Def.task {
        GenTable.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)
      }.taskValue,
      //scalatestJSDocTaskSetting,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (Compile / classDirectory).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar"),
      mimaBinaryIssueFilters ++= {
        Seq(
          exclude[MissingClassProblem]("org.scalatest.tools.SbtCommandParser$"),
          exclude[MissingClassProblem]("org.scalatest.tools.SbtCommandParser")
        )
      }
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.*"
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
      "Bundle-Name" -> "ScalaTest Core Dotty",
      "Bundle-Description" -> "ScalaTest is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc.",
      "Main-Class" -> "org.scalatest.tools.Runner"
    )
  ).dependsOn(scalacticDottyNative).enablePlugins(ScalaNativePlugin)

  private implicit class DottyProjectEx(private val p: Project) {
    /** common settings for all scalatest modules */
    def scalatestModule(name: String, title: String, isNative: Boolean): Project =  {
      val selectedDottySettings = if (isNative) dottySettings else dottySettings
      p.enablePlugins(SbtOsgi)
       .settings(sharedSettings: _*)
       .settings(selectedDottySettings: _*)
       .settings(
         projectTitle := title,
         organization := "org.scalatest",
         moduleName := name,
         packageManagedSources,
         osgiSettings,
         OsgiKeys.additionalHeaders := Map(
           "Bundle-Name" -> title,
           "Bundle-Description" -> "ScalaTest is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
           "Bundle-DocURL" -> "http://www.scalatest.org/",
           "Bundle-Vendor" -> "Artima, Inc."
         ),
      )
    }

    /** common settings for all scalatest sub modules (all modules, except the `scalatest` module) */
    def scalatestSubModule(name: String, title: String, isNative: Boolean, gen: GenModulesDotty.GenFn): Project =
      scalatestModule(name, title, isNative).settings(
        Compile / sourceGenerators += Def.task {
          gen((Compile / sourceManaged).value, version.value, scalaVersion.value)
        }.taskValue,
        OsgiKeys.importPackage := Seq(
          "org.scalatest.*",
          "*;resolution:=optional"
        ),
      )

    /** common settings for all scalatest `style` modules such as `featurespec`, `funsuite`,.. */
    def scalatestStyleModule(style: String, title: String): Project =
      scalatestSubModule(s"scalatest-$style", title, false, GenModulesDotty(style))
        .settings(
          OsgiKeys.exportPackage := Seq(s"org.scalatest.$style"),
        ).dependsOn(scalatestCoreDotty)

    /** common settings for all scalatest js `style` modules such as `featurespec`, `funsuite`,.. */
    def scalatestStyleModuleJS(style: String, title: String): Project =
      scalatestSubModule(s"scalatest-$style", title, false, GenModulesDotty.applyJS(style))
        .settings(
          OsgiKeys.exportPackage := Seq(s"org.scalatest.$style"),
        ).dependsOn(scalatestCoreDottyJS).enablePlugins(ScalaJSPlugin)

    /** common settings for all scalatest native `style` modules such as `featurespec`, `funsuite`,.. */
    def scalatestStyleModuleNative(style: String, title: String): Project =
      scalatestSubModule(s"scalatest-$style", title, true, GenModulesDotty.applyNative(style))
        .settings(
          OsgiKeys.exportPackage := Seq(s"org.scalatest.$style.*"),
        ).dependsOn(scalatestCoreDottyNative).enablePlugins(ScalaNativePlugin)    
        
  }
  
  lazy val scalatestFeatureSpecDotty = project.in(file("dotty/featurespec"))
    .scalatestStyleModule("featurespec", "ScalaTest FeatureSpec Dotty")

  lazy val scalatestFeatureSpecDottyJS = project.in(file("dotty/featurespec.js"))
    .scalatestStyleModuleJS("featurespec", "ScalaTest FeatureSpec Dotty JS")

  lazy val scalatestFeatureSpecDottyNative = project.in(file("dotty/featurespec.native"))
    .scalatestStyleModuleNative("featurespec", "ScalaTest FeatureSpec Dotty Native")    

  lazy val scalatestFlatSpecDotty = project.in(file("dotty/flatspec"))
    .scalatestStyleModule("flatspec", "ScalaTest FlatSpec Dotty")

  lazy val scalatestFlatSpecDottyJS = project.in(file("dotty/flatspec.js"))
    .scalatestStyleModuleJS("flatspec", "ScalaTest FlatSpec Dotty JS")

  lazy val scalatestFlatSpecDottyNative = project.in(file("dotty/flatspec.native"))
    .scalatestStyleModuleNative("flatspec", "ScalaTest FlatSpec Dotty Native")    

  lazy val scalatestFreeSpecDotty = project.in(file("dotty/freespec"))
    .scalatestStyleModule("freespec", "ScalaTest FreeSpec Dotty")

  lazy val scalatestFreeSpecDottyJS = project.in(file("dotty/freespec.js"))
    .scalatestStyleModuleJS("freespec", "ScalaTest FreeSpec Dotty JS")

  lazy val scalatestFreeSpecDottyNative = project.in(file("dotty/freespec.native"))
    .scalatestStyleModuleNative("freespec", "ScalaTest FreeSpec Dotty Native")    

  lazy val scalatestFunSuiteDotty = project.in(file("dotty/funsuite"))
    .scalatestStyleModule("funsuite", "ScalaTest FunSuite Dotty")

  lazy val scalatestFunSuiteDottyJS = project.in(file("dotty/funsuite.js"))
    .scalatestStyleModuleJS("funsuite", "ScalaTest FunSuite Dotty JS")

  lazy val scalatestFunSuiteDottyNative = project.in(file("dotty/funsuite.native"))
    .scalatestStyleModuleNative("funsuite", "ScalaTest FunSuite Dotty Native")    

  lazy val scalatestFunSpecDotty = project.in(file("dotty/funspec"))
    .scalatestStyleModule("funspec", "ScalaTest FunSpec Dotty")

  lazy val scalatestFunSpecDottyJS = project.in(file("dotty/funspec.js"))
    .scalatestStyleModuleJS("funspec", "ScalaTest FunSpec Dotty JS")

  lazy val scalatestFunSpecDottyNative = project.in(file("dotty/funspec.native"))
    .scalatestStyleModuleNative("funspec", "ScalaTest FunSpec Dotty Native")    

  lazy val scalatestPropSpecDotty = project.in(file("dotty/propspec"))
    .scalatestStyleModule("propspec", "ScalaTest PropSpec Dotty")

  lazy val scalatestPropSpecDottyJS = project.in(file("dotty/propspec.js"))
    .scalatestStyleModuleJS("propspec", "ScalaTest PropSpec Dotty JS")

  lazy val scalatestPropSpecDottyNative = project.in(file("dotty/propspec.native"))
    .scalatestStyleModuleNative("propspec", "ScalaTest PropSpec Dotty Native")    

  lazy val scalatestRefSpecDotty = project.in(file("dotty/refspec"))
    .scalatestStyleModule("refspec", "ScalaTest RefSpec Dotty")

  lazy val scalatestRefSpecDottyJS = project.in(file("dotty/refspec.js"))
    .scalatestStyleModuleJS("refspec", "ScalaTest RefSpec Dotty JS")

  lazy val scalatestRefSpecDottyNative = project.in(file("dotty/refspec.native"))
    .scalatestStyleModuleNative("refspec", "ScalaTest RefSpec Dotty Native")    

  lazy val scalatestWordSpecDotty = project.in(file("dotty/wordspec"))
    .scalatestStyleModule("wordspec", "ScalaTest WordSpec Dotty")

  lazy val scalatestWordSpecDottyJS = project.in(file("dotty/wordspec.js"))
    .scalatestStyleModuleJS("wordspec", "ScalaTest WordSpec Dotty JS")

  lazy val scalatestWordSpecDottyNative = project.in(file("dotty/wordspec.native"))
    .scalatestStyleModuleNative("wordspec", "ScalaTest WordSpec Dotty Native")    

  lazy val scalatestDiagramsDotty = project.in(file("dotty/diagrams"))
    .scalatestStyleModule("diagrams", "ScalaTest Diagrams Dotty")

  lazy val scalatestDiagramsDottyJS = project.in(file("dotty/diagrams.js"))
    .scalatestSubModule(
      "scalatest-diagrams", 
      "ScalaTest Diagrams Dotty JS", 
      false, 
      (targetDir, version, scalaVersion) =>
        GenScalaTestDotty.genDiagramsScalaJS(targetDir / "org" / "scalatest", version, scalaVersion)
    ).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest", 
        "org.scalatest.diagrams"
      ),
    ).dependsOn(scalatestCoreDottyJS).enablePlugins(ScalaJSPlugin)

  lazy val scalatestDiagramsDottyNative = project.in(file("dotty/diagrams.native"))
    .scalatestSubModule(
      "scalatest-diagrams", 
      "ScalaTest Diagrams Dotty Native", 
      true, 
      (targetDir, version, scalaVersion) =>
        GenScalaTestDotty.genDiagramsScalaNative(targetDir / "org" / "scalatest", version, scalaVersion)
    ).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest", 
        "org.scalatest.diagrams"
      ),
    ).dependsOn(scalatestCoreDottyNative).enablePlugins(ScalaNativePlugin)

  lazy val scalatestExpectationsDotty = project.in(file("dotty/expectations"))
    .scalatestModule("scalatest-expectations", "ScalaTest Expectations Dotty", false)
    .dependsOn(
      scalatestCoreDotty
    )

  lazy val scalatestExpectationsDottyJS = project.in(file("dotty/expectations.js"))
    .scalatestSubModule(
      "scalatest-expectations", 
      "ScalaTest Expectations Dotty JS", 
      false, 
      (targetDir, version, scalaVersion) =>
        GenScalaTestDotty.genExpectationsScalaJS(targetDir / "org" / "scalatest", version, scalaVersion)
    ).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.expectations"
      ),
    ).dependsOn(scalatestCoreDottyJS).enablePlugins(ScalaJSPlugin)

  lazy val scalatestExpectationsDottyNative = project.in(file("dotty/expectations.native"))
    .scalatestSubModule(
      "scalatest-expectations", 
      "ScalaTest Expectations Dotty Native", 
      true, 
      (targetDir, version, scalaVersion) =>
        GenScalaTestDotty.genExpectationsScalaNative(targetDir / "org" / "scalatest", version, scalaVersion)
    ).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.expectations"
      ),
    ).dependsOn(scalatestCoreDottyNative).enablePlugins(ScalaNativePlugin)    

  lazy val scalatestMatchersCoreDotty = project.in(file("dotty/matchers-core"))
    .scalatestSubModule(
      "scalatest-matchers-core",
      "ScalaTest Matchers Core Dotty",
      false, 
      (targetDir, version, scalaVersion) => {
        GenModulesDotty.genScalaTestMatchersCore(targetDir, version, scalaVersion) ++
          GenFactoriesDotty.genMain(targetDir / "org" / "scalatest" / "matchers" / "dsl", version, scalaVersion)
      }
    ).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.matchers",
        "org.scalatest.matchers.dsl"
      ),
    ).dependsOn(scalatestCoreDotty)

  lazy val scalatestMatchersCoreDottyJS = project.in(file("dotty/matchers-core.js"))
    .scalatestSubModule(
      "scalatest-matchers-core",
      "ScalaTest Matchers Core Dotty JS",
      false, 
      (targetDir, version, scalaVersion) => {
        GenModulesDotty.genScalaTestMatchersCoreJS(targetDir, version, scalaVersion) ++
        GenScalaTestDotty.genMatchersCoreScalaJS(targetDir, version, scalaVersion) ++
        GenFactoriesDotty.genMain(targetDir / "org" / "scalatest" / "matchers" / "dsl", version, scalaVersion)
      }
    ).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.matchers",
        "org.scalatest.matchers.dsl"
      ),
    ).dependsOn(scalatestCoreDottyJS).enablePlugins(ScalaJSPlugin)

  lazy val scalatestMatchersCoreDottyNative = project.in(file("dotty/matchers-core.native"))
    .scalatestSubModule(
      "scalatest-matchers-core",
      "ScalaTest Matchers Core Dotty Native",
      true, 
      (targetDir, version, scalaVersion) => {
        GenModulesDotty.genScalaTestMatchersCoreNative(targetDir, version, scalaVersion) ++
        GenScalaTestDotty.genMatchersCoreScalaNative(targetDir, version, scalaVersion) ++
        GenFactoriesDotty.genMain(targetDir / "org" / "scalatest" / "matchers" / "dsl", version, scalaVersion)
      }
    ).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest.matchers",
        "org.scalatest.matchers.dsl"
      ),
    ).dependsOn(scalatestCoreDottyNative).enablePlugins(ScalaNativePlugin)  

  lazy val scalatestShouldMatchersDotty = project.in(file("dotty/shouldmatchers"))
    .scalatestSubModule(
      "scalatest-shouldmatchers",
      "ScalaTest Should Matchers Dotty",
      false, 
      GenModulesDotty.genScalaTestShouldMatchers
    ).settings(
      OsgiKeys.exportPackage := Seq("org.scalatest.matchers.should"),
    ).dependsOn(scalatestMatchersCoreDotty)

  lazy val scalatestShouldMatchersDottyJS = project.in(file("dotty/shouldmatchers.js"))
    .scalatestSubModule(
      "scalatest-shouldmatchers",
      "ScalaTest Should Matchers Dotty JS",
      false, 
      (targetDir, version, scalaVersion) => {
        GenModulesDotty.genScalaTestShouldMatchersJS(targetDir, version, scalaVersion) ++ 
        GenScalaTestDotty.genShouldMatchersScalaJS(targetDir, version, scalaVersion)
      }
    ).settings(
      OsgiKeys.exportPackage := Seq("org.scalatest.matchers.should"),
    ).dependsOn(scalatestMatchersCoreDottyJS).enablePlugins(ScalaJSPlugin)

  lazy val scalatestShouldMatchersDottyNative = project.in(file("dotty/shouldmatchers.native"))
    .scalatestSubModule(
      "scalatest-shouldmatchers",
      "ScalaTest Should Matchers Dotty Native",
      true, 
      (targetDir, version, scalaVersion) => {
        GenModulesDotty.genScalaTestShouldMatchersNative(targetDir, version, scalaVersion) ++ 
        GenScalaTestDotty.genShouldMatchersScalaNative(targetDir, version, scalaVersion)
      }
    ).settings(
      OsgiKeys.exportPackage := Seq("org.scalatest.matchers.should"),
    ).dependsOn(scalatestMatchersCoreDottyNative).enablePlugins(ScalaNativePlugin)  

  lazy val scalatestMustMatchersDotty = project.in(file("dotty/mustmatchers"))
    .scalatestSubModule(
      "scalatest-mustmatchers",
      "ScalaTest Must Matchers Dotty",
      false, 
      (targetDir, version, scalaVersion) =>
        GenMatchers.genMainForDotty(targetDir / "org" / "scalatest", version, scalaVersion)
    ).settings(
    OsgiKeys.exportPackage := Seq("org.scalatest.matchers.must"),
  ).dependsOn(scalatestMatchersCoreDotty)

  lazy val scalatestMustMatchersDottyJS = project.in(file("dotty/mustmatchers.js"))
    .scalatestSubModule(
      "scalatest-mustmatchers",
      "ScalaTest Must Matchers Dotty JS ",
      false, 
      (targetDir, version, scalaVersion) =>
        GenMatchers.genMainForDottyJS(targetDir / "org" / "scalatest", version, scalaVersion) ++ 
        GenScalaTestDotty.genMustMatchersScalaJS(targetDir, version, scalaVersion)
    ).settings(
    OsgiKeys.exportPackage := Seq("org.scalatest.matchers.must"),
  ).dependsOn(scalatestMatchersCoreDottyJS).enablePlugins(ScalaJSPlugin)

  lazy val scalatestMustMatchersDottyNative = project.in(file("dotty/mustmatchers.native"))
    .scalatestSubModule(
      "scalatest-mustmatchers",
      "ScalaTest Must Matchers Dotty Native ",
      true, 
      (targetDir, version, scalaVersion) =>
        GenMatchers.genMainForDottyNative(targetDir / "org" / "scalatest", version, scalaVersion) ++ 
        GenScalaTestDotty.genMustMatchersScalaNative(targetDir, version, scalaVersion)
    ).settings(
    OsgiKeys.exportPackage := Seq("org.scalatest.matchers.must"),
  ).dependsOn(scalatestMatchersCoreDottyNative).enablePlugins(ScalaNativePlugin)

  lazy val scalatestModulesDotty = project.in(file("modules/dotty/modules-aggregation"))
    .settings(sharedSettings: _*)
    .settings(
      noPublishSettings,
      Compile / doc / scalacOptions := List.empty
    ).aggregate(
      scalatestCoreDotty, 
      scalatestFeatureSpecDotty, 
      scalatestFlatSpecDotty, 
      scalatestFreeSpecDotty, 
      scalatestFunSuiteDotty, 
      scalatestFunSpecDotty, 
      scalatestPropSpecDotty, 
      scalatestRefSpecDotty, 
      scalatestWordSpecDotty, 
      scalatestDiagramsDotty, 
      scalatestExpectationsDotty, 
      scalatestMatchersCoreDotty, 
      scalatestShouldMatchersDotty, 
      scalatestMustMatchersDotty
    )

  lazy val scalatestDotty = project.in(file("dotty/scalatest"))
    .scalatestModule("scalatest", "ScalaTest Dotty", false)
    .settings(
      // Little trick to get rid of bnd error when publish.
      Compile / sourceGenerators += Def.task {
        (crossTarget.value / "classes").mkdirs()
        Seq.empty[File]
      }.taskValue,
      OsgiKeys.privatePackage := Seq.empty, 
    ).dependsOn(
      scalatestCoreDotty, 
      scalatestFeatureSpecDotty, 
      scalatestFlatSpecDotty, 
      scalatestFreeSpecDotty, 
      scalatestFunSuiteDotty, 
      scalatestFunSpecDotty, 
      scalatestPropSpecDotty, 
      scalatestRefSpecDotty, 
      scalatestWordSpecDotty, 
      scalatestDiagramsDotty, 
      scalatestExpectationsDotty, 
      scalatestMatchersCoreDotty, 
      scalatestShouldMatchersDotty, 
      scalatestMustMatchersDotty
    ).aggregate(
      scalatestCoreDotty, 
      scalatestFeatureSpecDotty, 
      scalatestFlatSpecDotty, 
      scalatestFreeSpecDotty, 
      scalatestFunSuiteDotty, 
      scalatestFunSpecDotty, 
      scalatestPropSpecDotty, 
      scalatestRefSpecDotty, 
      scalatestWordSpecDotty, 
      scalatestDiagramsDotty, 
      scalatestExpectationsDotty, 
      scalatestMatchersCoreDotty, 
      scalatestShouldMatchersDotty, 
      scalatestMustMatchersDotty
    )

  lazy val scalatestDottyJS = project.in(file("dotty/scalatest.js"))
    .scalatestModule("scalatest", "ScalaTest Dotty JS", false)
    .settings(
      // Little trick to get rid of bnd error when publish.
      Compile / sourceGenerators += Def.task {
        (crossTarget.value / "classes").mkdirs()
        Seq.empty[File]
      }.taskValue,
      OsgiKeys.privatePackage := Seq.empty, 
    ).dependsOn(
      scalatestCoreDottyJS, 
      scalatestFeatureSpecDottyJS, 
      scalatestFlatSpecDottyJS, 
      scalatestFreeSpecDottyJS, 
      scalatestFunSuiteDottyJS, 
      scalatestFunSpecDottyJS, 
      scalatestPropSpecDottyJS, 
      scalatestRefSpecDottyJS, 
      scalatestWordSpecDottyJS, 
      scalatestDiagramsDottyJS, 
      scalatestExpectationsDottyJS, 
      scalatestMatchersCoreDottyJS, 
      scalatestShouldMatchersDottyJS, 
      scalatestMustMatchersDottyJS
    ).aggregate(
      scalatestCoreDottyJS, 
      scalatestFeatureSpecDottyJS, 
      scalatestFlatSpecDottyJS, 
      scalatestFreeSpecDottyJS, 
      scalatestFunSuiteDottyJS, 
      scalatestFunSpecDottyJS, 
      scalatestPropSpecDottyJS, 
      scalatestRefSpecDottyJS, 
      scalatestWordSpecDottyJS, 
      scalatestDiagramsDottyJS, 
      scalatestExpectationsDottyJS, 
      scalatestMatchersCoreDottyJS, 
      scalatestShouldMatchersDottyJS, 
      scalatestMustMatchersDottyJS
    ).enablePlugins(ScalaJSPlugin)

  lazy val scalatestDottyNative = project.in(file("dotty/scalatest.native"))
    .scalatestModule("scalatest", "ScalaTest Dotty Native", true)
    .settings(
      // Little trick to get rid of bnd error when publish.
      Compile / sourceGenerators += Def.task {
        (crossTarget.value / "classes").mkdirs()
        Seq.empty[File]
      }.taskValue,
      OsgiKeys.privatePackage := Seq.empty, 
    ).dependsOn(
      scalatestCoreDottyNative, 
      scalatestFeatureSpecDottyNative, 
      scalatestFlatSpecDottyNative, 
      scalatestFreeSpecDottyNative, 
      scalatestFunSuiteDottyNative, 
      scalatestFunSpecDottyNative, 
      scalatestPropSpecDottyNative, 
      scalatestRefSpecDottyNative, 
      scalatestWordSpecDottyNative, 
      scalatestDiagramsDottyNative, 
      scalatestExpectationsDottyNative, 
      scalatestMatchersCoreDottyNative, 
      scalatestShouldMatchersDottyNative, 
      scalatestMustMatchersDottyNative
    ).aggregate(
      scalatestCoreDottyNative, 
      scalatestFeatureSpecDottyNative, 
      scalatestFlatSpecDottyNative, 
      scalatestFreeSpecDottyNative, 
      scalatestFunSuiteDottyNative, 
      scalatestFunSpecDottyNative, 
      scalatestPropSpecDottyNative, 
      scalatestRefSpecDottyNative, 
      scalatestWordSpecDottyNative, 
      scalatestDiagramsDottyNative, 
      scalatestExpectationsDottyNative, 
      scalatestMatchersCoreDottyNative, 
      scalatestShouldMatchersDottyNative, 
      scalatestMustMatchersDottyNative
    ).enablePlugins(ScalaNativePlugin)   

  lazy val scalatestAppDotty = project.in(file("dotty/scalatest-app"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(scalatestDocSettings: _*)
    .settings(
      projectTitle := "ScalaTest App",
      name := "scalatest-app",
      organization := "org.scalatest",
      libraryDependencies ++= scalatestLibraryDependencies,
      // include the scalactic classes and resources in the jar
      Compile / packageBin / mappings ++= (scalacticDotty / Compile / packageBin / mappings).value,
      // include the scalactic sources in the source jar
      Compile / packageSrc / mappings ++= (scalacticDotty / Compile / packageSrc / mappings).value,
      // include the scalatestCompatible classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestCompatible / Compile / packageBin / mappings).value,
      // include the scalatestCompatible sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestCompatible / Compile / packageSrc / mappings).value,
      // include the scalatest classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestCoreDotty / Compile / packageBin / mappings).value,
      // include the scalatest sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestCoreDotty / Compile / packageSrc / mappings).value,
      // include the scalatestFeatureSpecDotty classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestFeatureSpecDotty / Compile / packageBin / mappings).value,
      // include the scalatestFeatureSpecDotty sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestFeatureSpecDotty / Compile / packageSrc / mappings).value,
      // include the scalatestFlatSpecDotty classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestFlatSpecDotty / Compile / packageBin / mappings).value,
      // include the scalatestFlatSpecDotty sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestFlatSpecDotty / Compile / packageSrc / mappings).value,
      // include the scalatestFreeSpecDotty classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestFreeSpecDotty / Compile / packageBin / mappings).value,
      // include the scalatestFreeSpecDotty sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestFreeSpecDotty / Compile / packageSrc / mappings).value,
      // include the scalatestFunSuiteDotty classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestFunSuiteDotty / Compile / packageBin / mappings).value,
      // include the scalatestFunSuiteDotty sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestFunSuiteDotty / Compile / packageSrc / mappings).value,
      // include the scalatestFunSpecDotty classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestFunSpecDotty / Compile / packageBin / mappings).value,
      // include the scalatestFunSpecDotty sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestFunSpecDotty / Compile / packageSrc / mappings).value,
      // include the scalatestPropSpecDotty classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestPropSpecDotty / Compile / packageBin / mappings).value,
      // include the scalatestPropSpecDotty sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestPropSpecDotty / Compile / packageSrc / mappings).value,
      // include the scalatestWordSpecDotty classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestWordSpecDotty / Compile / packageBin / mappings).value,
      // include the scalatestWordSpecDotty sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestWordSpecDotty / Compile / packageSrc / mappings).value,
      // include the scalatestDiagramsDotty classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestDiagramsDotty / Compile / packageBin / mappings).value,
      // include the scalatestDiagramsDotty sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestDiagramsDotty / Compile / packageSrc / mappings).value,
      // include the scalatestMatchersCoreDotty classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestMatchersCoreDotty / Compile / packageBin / mappings).value,
      // include the scalatestMatchersCoreDotty sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestMatchersCoreDotty / Compile / packageSrc / mappings).value,
      // include the scalatestShouldMatchersDotty classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestShouldMatchersDotty / Compile / packageBin / mappings).value,
      // include the scalatestShouldMatchersDotty sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestShouldMatchersDotty / Compile / packageSrc / mappings).value,
      // include the scalatestMustMatchersDotty classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestMustMatchersDotty / Compile / packageBin / mappings).value,
      // include the scalatestMustMatchersDotty sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestMustMatchersDotty / Compile / packageSrc / mappings).value, 
      Compile / sourceGenerators += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      scalatestDocSettings,
      Compile / unmanagedResourceDirectories += baseDirectory.value / "scalatest" / "src" / "main" / "resources",
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (Compile / classDirectory).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
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
        scalacticDotty % "compile-internal", 
        scalatestCompatible % "compile-internal", 
        scalatestCoreDotty % "compile-internal", 
        scalatestFeatureSpecDotty % "compile-internal", 
        scalatestFlatSpecDotty % "compile-internal", 
        scalatestFreeSpecDotty % "compile-internal", 
        scalatestFunSuiteDotty % "compile-internal", 
        scalatestFunSpecDotty % "compile-internal", 
        scalatestPropSpecDotty % "compile-internal", 
        scalatestWordSpecDotty % "compile-internal", 
        scalatestDiagramsDotty % "compile-internal", 
        scalatestMatchersCoreDotty % "compile-internal", 
        scalatestShouldMatchersDotty % "compile-internal", 
        scalatestMustMatchersDotty % "compile-internal")

  lazy val scalatestAppDottyJS = project.in(file("dotty/scalatest-app-js"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest App",
      name := "scalatest-app",
      organization := "org.scalatest",
      moduleName := "scalatest-app",
      //libraryDependencies ++= scalatestJSLibraryDependencies,
      libraryDependencies += ("org.scala-js" %% "scalajs-test-interface" % scalaJSVersion), 
      // include the scalactic classes and resources in the jar
      Compile / packageBin / mappings ++= (scalacticDottyJS / Compile / packageBin / mappings).value,
      // include the scalactic sources in the source jar
      Compile / packageSrc / mappings ++= (scalacticDottyJS / Compile / packageSrc / mappings).value,
      // include the scalatest classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestCoreDottyJS / Compile / packageBin / mappings).value,
      // include the scalatest sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestCoreDottyJS / Compile / packageSrc / mappings).value,
      // include the scalatestFeatureSpecDottyJS classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestFeatureSpecDottyJS / Compile / packageBin / mappings).value,
      // include the scalatestFeatureSpecDottyJS sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestFeatureSpecDottyJS / Compile / packageSrc / mappings).value,
      // include the scalatestFlatSpecDottyJS classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestFlatSpecDottyJS / Compile / packageBin / mappings).value,
      // include the scalatestFlatSpecDottyJS sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestFlatSpecDottyJS / Compile / packageSrc / mappings).value,
      // include the scalatestFreeSpecDottyJS classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestFreeSpecDottyJS / Compile / packageBin / mappings).value,
      // include the scalatestFreeSpecDottyJS sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestFreeSpecDottyJS / Compile / packageSrc / mappings).value,
      // include the scalatestFunSuiteDottyJS classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestFunSuiteDottyJS / Compile / packageBin / mappings).value,
      // include the scalatestFunSuiteDottyJS sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestFunSuiteDottyJS / Compile / packageSrc / mappings).value,
      // include the scalatestFunSpecDottyJS classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestFunSpecDottyJS / Compile / packageBin / mappings).value,
      // include the scalatestFunSpecDottyJS sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestFunSpecDottyJS / Compile / packageSrc / mappings).value,
      // include the scalatestPropSpecDottyJS classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestPropSpecDottyJS / Compile / packageBin / mappings).value,
      // include the scalatestPropSpecDottyJS sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestPropSpecDottyJS / Compile / packageSrc / mappings).value,
      // include the scalatestWordSpecDottyJS classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestWordSpecDottyJS / Compile / packageBin / mappings).value,
      // include the scalatestWordSpecDottyJS sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestWordSpecDottyJS / Compile / packageSrc / mappings).value,
      // include the scalatestDiagramsDottyJS classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestDiagramsDottyJS / Compile / packageBin / mappings).value,
      // include the scalatestDiagramsDottyJS sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestDiagramsDottyJS / Compile / packageSrc / mappings).value,
      // include the scalatestMatchersCoreDottyJS classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestMatchersCoreDottyJS / Compile / packageBin / mappings).value,
      // include the scalatestMatchersCoreDottyJS sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestMatchersCoreDottyJS / Compile / packageSrc / mappings).value,
      // include the scalatestShouldMatchersDottyJS classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestShouldMatchersDottyJS / Compile / packageBin / mappings).value,
      // include the scalatestShouldMatchersDottyJS sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestShouldMatchersDottyJS / Compile / packageSrc / mappings).value,
      // include the scalatestMustMatchersDottyJS classes and resources in the jar
      Compile / packageBin / mappings ++= (scalatestMustMatchersDottyJS / Compile / packageBin / mappings).value,
      // include the scalatestMustMatchersDottyJS sources in the source jar
      Compile / packageSrc / mappings ++= (scalatestMustMatchersDottyJS / Compile / packageSrc / mappings).value,
      Compile / sourceGenerators += {
        // Little trick to get rid of bnd error when publish.
        Def.task{
          (new File(crossTarget.value, "classes")).mkdirs()
          Seq.empty[File]
        }.taskValue
      },
      mimaPreviousArtifacts := Set(organization.value %%% moduleName.value % previousReleaseVersion),
      mimaCurrentClassfiles := (Compile / classDirectory).value.getParentFile / (moduleName.value + sjsPrefix + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
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
      scalacticDottyJS % "compile-internal", 
      scalatestCoreDottyJS % "compile-internal", 
      scalatestFeatureSpecDottyJS % "compile-internal", 
      scalatestFlatSpecDottyJS % "compile-internal", 
      scalatestFreeSpecDottyJS % "compile-internal", 
      scalatestFunSuiteDottyJS % "compile-internal", 
      scalatestFunSpecDottyJS % "compile-internal", 
      scalatestPropSpecDottyJS % "compile-internal", 
      scalatestWordSpecDottyJS % "compile-internal", 
      scalatestDiagramsDottyJS % "compile-internal", 
      scalatestMatchersCoreDottyJS % "compile-internal", 
      scalatestShouldMatchersDottyJS % "compile-internal", 
      scalatestMustMatchersDottyJS % "compile-internal")
     .enablePlugins(ScalaJSPlugin)

  lazy val scalatestAppDottyNative = project.in(file("dotty/scalatest-app.native"))
      .enablePlugins(SbtOsgi)
      .settings(sharedSettings)
      .settings(dottySettings: _*)
      .settings(
        projectTitle := "ScalaTest App",
        name := "scalatest-app",
        organization := "org.scalatest",
        moduleName := "scalatest-app",
        //libraryDependencies ++= nativeCrossBuildLibraryDependencies.value,
        libraryDependencies += ("org.scala-native" %%% "test-interface" % nativeVersion),
        // include the scalacticDottyNative classes and resources in the jar
        Compile / packageBin / mappings ++= (scalacticDottyNative / Compile / packageBin / mappings).value,
        // include the scalacticDottyNative sources in the source jar
        Compile / packageSrc / mappings ++= (scalacticDottyNative / Compile / packageSrc / mappings).value,
        // include the scalacticDottyNative classes and resources in the jar
        Compile / packageBin / mappings ++= (scalatestCoreDottyNative / Compile / packageBin / mappings).value,
        // include the scalacticDottyNative sources in the source jar
        Compile / packageSrc / mappings ++= (scalatestCoreDottyNative / Compile / packageSrc / mappings).value,
        // include the scalatestFeatureSpecDottyNative classes and resources in the jar
        Compile / packageBin / mappings ++= (scalatestFeatureSpecDottyNative / Compile / packageBin / mappings).value,
        // include the scalatestFeatureSpecDottyNative sources in the source jar
        Compile / packageSrc / mappings ++= (scalatestFeatureSpecDottyNative / Compile / packageSrc / mappings).value,
        // include the scalatestFlatSpecDottyNative classes and resources in the jar
        Compile / packageBin / mappings ++= (scalatestFlatSpecDottyNative / Compile / packageBin / mappings).value,
        // include the scalatestFlatSpecDottyNative sources in the source jar
        Compile / packageSrc / mappings ++= (scalatestFlatSpecDottyNative / Compile / packageSrc / mappings).value,
        // include the scalatestFreeSpecDottyNative classes and resources in the jar
        Compile / packageBin / mappings ++= (scalatestFreeSpecDottyNative / Compile / packageBin / mappings).value,
        // include the scalatestFreeSpecDottyNative sources in the source jar
        Compile / packageSrc / mappings ++= (scalatestFreeSpecDottyNative / Compile / packageSrc / mappings).value,
        // include the scalatestFunSuiteDottyNative classes and resources in the jar
        Compile / packageBin / mappings ++= (scalatestFunSuiteDottyNative / Compile / packageBin / mappings).value,
        // include the scalatestFunSuiteDottyNative sources in the source jar
        Compile / packageSrc / mappings ++= (scalatestFunSuiteDottyNative / Compile / packageSrc / mappings).value,
        // include the scalatestFunSpecDottyNative classes and resources in the jar
        Compile / packageBin / mappings ++= (scalatestFunSpecDottyNative / Compile / packageBin / mappings).value,
        // include the scalatestFunSpecDottyNative sources in the source jar
        Compile / packageSrc / mappings ++= (scalatestFunSpecDottyNative / Compile / packageSrc / mappings).value,
        // include the scalatestPropSpecDottyNative classes and resources in the jar
        Compile / packageBin / mappings ++= (scalatestPropSpecDottyNative / Compile / packageBin / mappings).value,
        // include the scalatestPropSpecDottyNative sources in the source jar
        Compile / packageSrc / mappings ++= (scalatestPropSpecDottyNative / Compile / packageSrc / mappings).value,
        // include the scalatestWordSpecDottyNative classes and resources in the jar
        Compile / packageBin / mappings ++= (scalatestWordSpecDottyNative / Compile / packageBin / mappings).value,
        // include the scalatestWordSpecDottyNative sources in the source jar
        Compile / packageSrc / mappings ++= (scalatestWordSpecDottyNative / Compile / packageSrc / mappings).value,
        // include the scalatestDiagramsDottyNative classes and resources in the jar
        Compile / packageBin / mappings ++= (scalatestDiagramsDottyNative / Compile / packageBin / mappings).value,
        // include the scalatestDiagramsDottyNative sources in the source jar
        Compile / packageSrc / mappings ++= (scalatestDiagramsDottyNative / Compile / packageSrc / mappings).value,
        // include the scalatestMatchersCoreDottyNative classes and resources in the jar
        Compile / packageBin / mappings ++= (scalatestMatchersCoreDottyNative / Compile / packageBin / mappings).value,
        // include the scalatestMatchersCoreDottyNative sources in the source jar
        Compile / packageSrc / mappings ++= (scalatestMatchersCoreDottyNative / Compile / packageSrc / mappings).value,
        // include the scalatestShouldMatchersDottyNative classes and resources in the jar
        Compile / packageBin / mappings ++= (scalatestShouldMatchersDottyNative / Compile / packageBin / mappings).value,
        // include the scalatestShouldMatchersDottyNative sources in the source jar
        Compile / packageSrc / mappings ++= (scalatestShouldMatchersDottyNative / Compile / packageSrc / mappings).value,
        // include the scalatestMustMatchersDottyNative classes and resources in the jar
        Compile / packageBin / mappings ++= (scalatestMustMatchersDottyNative / Compile / packageBin / mappings).value,
        // include the scalatestMustMatchersDottyNative sources in the source jar
        Compile / packageSrc / mappings ++= (scalatestMustMatchersDottyNative / Compile / packageSrc / mappings).value,
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
        scalacticDottyNative % "compile-internal",
        scalatestCoreDottyNative % "compile-internal",
        scalatestFeatureSpecDottyNative % "compile-internal",
        scalatestFlatSpecDottyNative % "compile-internal",
        scalatestFreeSpecDottyNative % "compile-internal",
        scalatestFunSuiteDottyNative % "compile-internal",
        scalatestFunSpecDottyNative % "compile-internal",
        scalatestPropSpecDottyNative % "compile-internal",
        scalatestWordSpecDottyNative % "compile-internal",
        scalatestDiagramsDottyNative % "compile-internal",
        scalatestMatchersCoreDottyNative % "compile-internal",
        scalatestShouldMatchersDottyNative % "compile-internal",
        scalatestMustMatchersDottyNative % "compile-internal")
       .enablePlugins(ScalaNativePlugin)      

  private lazy val noPublishSettings = Seq(
    publishArtifact := false,
    publish := {},
    publishLocal := {},
  )

  lazy val commonTestDotty = project.in(file("dotty/common-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "Common test classes used by scalactic and scalatest",
      libraryDependencies ++= crossBuildTestLibraryDependencies.value,
      Compile / sourceGenerators += Def.task {
        GenCommonTestDotty.genMain((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++ 
        GenCompatibleClasses.genTest((Compile / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
      noPublishSettings, 
    ).dependsOn(scalacticDotty, LocalProject("scalatestDotty"))

  lazy val commonTestDottyJS = project.in(file("dotty/common-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "Common test classes used by scalactic and scalatest",
      libraryDependencies ++= crossBuildTestLibraryDependencies.value,
      Compile / sourceGenerators += Def.task {
        GenCommonTestDotty.genMainJS((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++ 
        GenCompatibleClasses.genTest((Compile / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
      noPublishSettings,
    ).dependsOn(scalacticDottyJS, LocalProject("scalatestDottyJS")).enablePlugins(ScalaJSPlugin)

  lazy val commonTestDottyNative = project.in(file("dotty/common-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "Common test classes used by scalactic and scalatest",
      libraryDependencies ++= crossBuildTestLibraryDependencies.value,
      Compile / sourceGenerators += Def.task {
        GenCommonTestDotty.genMainNative((Compile / sourceManaged).value / "scala", version.value, scalaVersion.value) ++
        GenGen.genMain((Compile / sourceManaged).value / "scala" / "org" / "scalatest" / "prop", version.value, scalaVersion.value) ++
        GenCompatibleClasses.genTest((Compile / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
      noPublishSettings,
    ).dependsOn(scalacticDottyNative, LocalProject("scalatestDottyNative")).enablePlugins(ScalaNativePlugin)  

  lazy val scalacticTestDotty = project.in(file("dotty/scalactic-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "Scalactic Test",
      organization := "org.scalactic",
      Test / testOptions ++=
        Seq(Tests.Argument(TestFrameworks.ScalaTest,
          "-oDIF",
          "-W", "120", "60")),    
      Test / logBuffered := false,
      noPublishSettings,
      Test / sourceGenerators += Def.task {
        GenScalacticDotty.genTest((Test / sourceManaged).value, version.value, scalaVersion.value) /*++
        GenAnyVals.genTest((Test / sourceManaged).value / "scala" / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value)*/
      }.taskValue
    ).dependsOn(scalacticDotty, scalatestDotty % "test", commonTestDotty % "test")

  lazy val scalacticTestDottyJS = project.in(file("dotty/scalactic-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "Scalactic Test JS",
      organization := "org.scalactic",
      scalaJSLinkerConfig ~= { _.withOptimizer(false) },
      Test / testOptions ++=
        Seq(Tests.Argument(TestFrameworks.ScalaTest, "-oDIF")),
      jsEnv := {
        import org.scalajs.jsenv.nodejs.NodeJSEnv
        new NodeJSEnv(
          NodeJSEnv.Config()
            .withArgs(List("--max_old_space_size=3000")))
      }, 
      Test / parallelExecution := false,
      Test / fork := false,
      Test / logBuffered := false,
      noPublishSettings,
      Test / sourceGenerators += Def.task {
        GenScalacticDotty.genTestJS((Test / sourceManaged).value, version.value, scalaVersion.value) /*++
        GenAnyVals.genTest((Test / sourceManaged).value / "scala" / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value)*/
      }.taskValue
    ).dependsOn(scalacticDottyJS, scalatestDottyJS % "test", commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalacticTestDottyNative = project.in(file("dotty/scalactic-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "Scalactic Test Native",
      organization := "org.scalactic",
      publishArtifact := false,
      publish := {},
      publishLocal := {}, 
      Test / sourceGenerators += Def.task {
        GenScalacticDotty.genTestNative((Test / sourceManaged).value, version.value, scalaVersion.value) /*++
        GenAnyVals.genTest((Test / sourceManaged).value / "scala" / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value)*/
      }.taskValue
    ).dependsOn(scalacticDottyNative, scalatestDottyNative % "test", commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)  

  def sharedTestSettingsDotty: Seq[Setting[_]] = 
    Seq(
      organization := "org.scalatest",
      libraryDependencies ++= scalatestLibraryDependencies,
      libraryDependencies ++= 
        Seq(
          "org.scalatestplus" %% "testng-7-5" % plusTestNGVersion % "test",
          "org.scalatestplus" %% "junit-4-13" % plusJUnitVersion % "test"
        ),
      Test / testOptions := scalatestTestOptions,
      Test / logBuffered := false,
      //Test / fork := true,
      //Test / parallelExecution := true,
      //Test / testForkedParallel := true,
      Test / baseDirectory := file("./"),
    ) ++ noPublishSettings

  lazy val scalatestTestDotty = project.in(file("dotty/scalatest-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest Test",
      javaSourceManaged := target.value / "java",
      Test / sourceGenerators += Def.task {
        GenRegularTests4.genJava((Compile / javaSourceManaged).value) ++
        GenScalaTestDotty.genTest((Test / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDotty % "test").aggregate(
      scalatestDiagramsTestDotty, 
      scalatestExpectationsTestDotty, 
      scalatestFeatureSpecTestDotty, 
      scalatestFlatSpecTestDotty, 
      scalatestFreeSpecTestDotty, 
      scalatestFunSpecTestDotty, 
      scalatestFunSuiteTestDotty, 
      scalatestPropSpecTestDotty, 
      scalatestWordSpecTestDotty
    )

  def sharedTestSettingsDottyJS: Seq[Setting[_]] = 
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
      Test / testOptions := scalatestTestJSNativeOptions,
      Test / parallelExecution := false,
      Test / fork := false,
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      scalacOptions ++= (if (scalaBinaryVersion.value == "2.10" || scalaVersion.value.startsWith("2.13")) Seq.empty[String] else Seq("-Ypartial-unification"))
    )  

  lazy val scalatestTestDottyJS = project.in(file("dotty/scalatest-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest Test",
      scalaJSLinkerConfig ~= { _.withOptimizer(false) },
      Test / sourceGenerators += Def.task {
        //GenRegularTests4.genJava((Compile / javaSourceManaged).value) ++
        GenScalaTestDotty.genTestJS((Test / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(scalacticDottyJS, scalatestDottyJS % "test", commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)
     .aggregate(
       scalatestDiagramsTestDottyJS, 
       scalatestExpectationsTestDottyJS, 
       scalatestFeatureSpecTestDottyJS, 
       scalatestFlatSpecTestDottyJS, 
       scalatestFreeSpecTestDottyJS, 
       scalatestFunSpecTestDottyJS, 
       scalatestFunSuiteTestDottyJS, 
       scalatestPropSpecTestDottyJS, 
       scalatestWordSpecTestDottyJS
     ).enablePlugins(ScalaJSPlugin)

  lazy val scalatestTestDottyNative = project.in(file("dotty/scalatest-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative: _*)
    .settings(
      projectTitle := "ScalaTest Test",
      Test / sourceGenerators += Def.task {
        //GenRegularTests4.genJava((Compile / javaSourceManaged).value) ++
        GenScalaTestDotty.genTestNative((Test / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(scalacticDottyNative, scalatestDottyNative % "test", commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)
     .aggregate(
       scalatestDiagramsTestDottyNative, 
       scalatestExpectationsTestDottyNative, 
       scalatestFeatureSpecTestDottyNative, 
       scalatestFlatSpecTestDottyNative, 
       scalatestFreeSpecTestDottyNative, 
       scalatestFunSpecTestDottyNative, 
       scalatestFunSuiteTestDottyNative, 
       scalatestPropSpecTestDottyNative, 
       scalatestWordSpecTestDottyNative
     ).enablePlugins(ScalaNativePlugin)   


  lazy val scalatestDiagramsTestDotty = project.in(file("dotty/diagrams-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest Diagrams Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genDiagramsTest((Test / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDotty % "test")

  lazy val scalatestDiagramsTestDottyJS = project.in(file("dotty/diagrams-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest Diagrams Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genDiagramsTestJS((Test / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestDiagramsTestDottyNative = project.in(file("dotty/diagrams-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative)
    .settings(
      projectTitle := "ScalaTest Diagrams Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genDiagramsTestNative((Test / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)

  lazy val scalatestExpectationsTestDotty = project.in(file("dotty/expectations-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest Expectations Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genExpectationsTest((Test / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDotty % "test")

  lazy val scalatestExpectationsTestDottyJS = project.in(file("dotty/expectations-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest Expectations Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genExpectationsTestJS((Test / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestExpectationsTestDottyNative = project.in(file("dotty/expectations-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative)
    .settings(
      projectTitle := "ScalaTest Expectations Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genExpectationsTestNative((Test / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)    

  lazy val scalatestFeatureSpecTestDotty = project.in(file("dotty/featurespec-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest FeatureSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genFeatureSpecTest((Test / sourceManaged).value, version.value, scalaVersion.value) ++ 
        GenSafeStyles.genFeatureSpecTest((Compile / sourceManaged).value / "org" / "scalatest" / "featurespec", version.value, scalaVersion.value, false).filter { f =>
          f.getName != "FixtureFeatureSpecSpec.scala" && 
          f.getName != "FeatureSpecSpec.scala"
        }
      }.taskValue,
    ).dependsOn(commonTestDotty % "test")

  lazy val scalatestFeatureSpecTestDottyJS = project.in(file("dotty/featurespec-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest FeatureSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genFeatureSpecTestJS((Test / sourceManaged).value, version.value, scalaVersion.value) ++ 
        GenSafeStyles.genFeatureSpecTest((Compile / sourceManaged).value / "org" / "scalatest" / "featurespec", version.value, scalaVersion.value, true).filter { f =>
          f.getName != "FixtureFeatureSpecSpec.scala" && 
          f.getName != "FeatureSpecSpec.scala"
        }
      }.taskValue,
    ).dependsOn(commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFeatureSpecTestDottyNative = project.in(file("dotty/featurespec-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative)
    .settings(
      projectTitle := "ScalaTest FeatureSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genFeatureSpecTestNative((Test / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)  

  lazy val scalatestFlatSpecTestDotty = project.in(file("dotty/flatspec-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest FlatSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genFlatSpecTest((Test / sourceManaged).value, version.value, scalaVersion.value) ++ 
        GenSafeStyles.genFlatSpecTest((Compile / sourceManaged).value / "org" / "scalatest" / "flatspec", version.value, scalaVersion.value, false).filter { f =>
          f.getName != "FixtureFlatSpecSpec.scala" && 
          f.getName != "FlatSpecSpec.scala"
        }
      }.taskValue,
    ).dependsOn(commonTestDotty % "test")

  lazy val scalatestFlatSpecTestDottyJS = project.in(file("dotty/flatspec-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest FlatSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genFlatSpecTestJS((Test / sourceManaged).value, version.value, scalaVersion.value) ++ 
        GenSafeStyles.genFlatSpecTest((Compile / sourceManaged).value / "org" / "scalatest" / "flatspec", version.value, scalaVersion.value, true).filter { f =>
          f.getName != "FixtureFlatSpecSpec.scala" && 
          f.getName != "FlatSpecSpec.scala"
        }
      }.taskValue,
    ).dependsOn(commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFlatSpecTestDottyNative = project.in(file("dotty/flatspec-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative)
    .settings(
      projectTitle := "ScalaTest FlatSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genFlatSpecTestNative((Test / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)  

  lazy val scalatestFreeSpecTestDotty = project.in(file("dotty/freespec-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest FreeSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genFreeSpecTest((Test / sourceManaged).value, version.value, scalaVersion.value) ++ 
        GenSafeStyles.genFreeSpecTest((Compile / sourceManaged).value / "org" / "scalatest" / "freespec", version.value, scalaVersion.value, false).filter { f =>
          f.getName != "FixtureFreeSpecSpec.scala" && 
          f.getName != "FreeSpecSpec.scala"
        }
      }.taskValue,
    ).dependsOn(commonTestDotty % "test")

  lazy val scalatestFreeSpecTestDottyJS = project.in(file("dotty/freespec-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest FreeSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genFreeSpecTestJS((Test / sourceManaged).value, version.value, scalaVersion.value) ++ 
        GenSafeStyles.genFreeSpecTest((Compile / sourceManaged).value / "org" / "scalatest" / "freespec", version.value, scalaVersion.value, true).filter { f =>
          f.getName != "FixtureFreeSpecSpec.scala" && 
          f.getName != "FreeSpecSpec.scala"
        }
      }.taskValue,
    ).dependsOn(commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFreeSpecTestDottyNative = project.in(file("dotty/freespec-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative)
    .settings(
      projectTitle := "ScalaTest FreeSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genFreeSpecTestNative((Test / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)  

  lazy val scalatestFunSpecTestDotty = project.in(file("dotty/funspec-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest FunSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genFunSpecTest((Test / sourceManaged).value, version.value, scalaVersion.value) ++ 
        GenSafeStyles.genFunSpecTest((Compile / sourceManaged).value / "org" / "scalatest" / "funspec", version.value, scalaVersion.value, false).filter { f =>
          f.getName != "FixtureFunSpecSpec.scala" && 
          f.getName != "FunSpecSpec.scala"
        }
      }.taskValue,
    ).dependsOn(commonTestDotty % "test")

  lazy val scalatestFunSpecTestDottyJS = project.in(file("dotty/funspec-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest FunSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genFunSpecTestJS((Test / sourceManaged).value, version.value, scalaVersion.value) ++ 
        GenSafeStyles.genFunSpecTest((Compile / sourceManaged).value / "org" / "scalatest" / "funspec", version.value, scalaVersion.value, true).filter { f =>
          f.getName != "FixtureFunSpecSpec.scala" && 
          f.getName != "FunSpecSpec.scala"
        }
      }.taskValue,
    ).dependsOn(commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFunSpecTestDottyNative = project.in(file("dotty/funspec-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative)
    .settings(
      projectTitle := "ScalaTest FunSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genFunSpecTestNative((Test / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)  

  lazy val scalatestFunSuiteTestDotty = project.in(file("dotty/funsuite-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest FunSuite Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genFunSuiteTest((Test / sourceManaged).value, version.value, scalaVersion.value) ++ 
        GenSafeStyles.genFunSuiteTest((Compile / sourceManaged).value / "org" / "scalatest" / "funsuite", version.value, scalaVersion.value, false).filter { f =>
          f.getName != "FixtureFunSuiteSpec.scala" && 
          f.getName != "FunSuiteSpec.scala"
        }
      }.taskValue,
    ).dependsOn(commonTestDotty % "test")

  lazy val scalatestFunSuiteTestDottyJS = project.in(file("dotty/funsuite-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest FunSuite Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genFunSuiteTestJS((Test / sourceManaged).value, version.value, scalaVersion.value) ++ 
        GenSafeStyles.genFunSuiteTest((Compile / sourceManaged).value / "org" / "scalatest" / "funsuite", version.value, scalaVersion.value, true).filter { f =>
          f.getName != "FixtureFunSuiteSpec.scala" && 
          f.getName != "FunSuiteSpec.scala"
        }
      }.taskValue,
    ).dependsOn(commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFunSuiteTestDottyNative = project.in(file("dotty/funsuite-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative)
    .settings(
      projectTitle := "ScalaTest FunSuite Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genFunSuiteTestNative((Test / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)  

  lazy val scalatestPropSpecTestDotty = project.in(file("dotty/propspec-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest PropSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genPropSpecTest((Test / sourceManaged).value, version.value, scalaVersion.value) ++ 
        GenSafeStyles.genPropSpecTest((Compile / sourceManaged).value / "org" / "scalatest" / "propspec", version.value, scalaVersion.value, false).filter { f =>
          f.getName != "FixturePropSpecSpec.scala" && 
          f.getName != "PropSpecSpec.scala"
        }
      }.taskValue,
    ).dependsOn(commonTestDotty % "test")

  lazy val scalatestPropSpecTestDottyJS = project.in(file("dotty/propspec-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest PropSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genPropSpecTestJS((Test / sourceManaged).value, version.value, scalaVersion.value) ++ 
        GenSafeStyles.genPropSpecTest((Compile / sourceManaged).value / "org" / "scalatest" / "propspec", version.value, scalaVersion.value, true).filter { f =>
          f.getName != "FixturePropSpecSpec.scala" && 
          f.getName != "PropSpecSpec.scala"
        }
      }.taskValue,
    ).dependsOn(commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestPropSpecTestDottyNative = project.in(file("dotty/propspec-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative)
    .settings(
      projectTitle := "ScalaTest PropSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genPropSpecTestNative((Test / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)               

  lazy val scalatestWordSpecTestDotty = project.in(file("dotty/wordspec-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest WordSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genWordSpecTest((Test / sourceManaged).value, version.value, scalaVersion.value) ++ 
        GenSafeStyles.genWordSpecTest((Compile / sourceManaged).value / "org" / "scalatest" / "wordspec", version.value, scalaVersion.value, false).filter { f =>
          f.getName != "FixtureWordSpecSpec.scala" && 
          f.getName != "WordSpecSpec.scala"
        }
      }.taskValue,
    ).dependsOn(commonTestDotty % "test")

  lazy val scalatestWordSpecTestDottyJS = project.in(file("dotty/wordspec-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest WordSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genWordSpecTestJS((Test / sourceManaged).value, version.value, scalaVersion.value) ++ 
        GenSafeStyles.genWordSpecTest((Compile / sourceManaged).value / "org" / "scalatest" / "wordspec", version.value, scalaVersion.value, true).filter { f =>
          f.getName != "FixtureWordSpecSpec.scala" && 
          f.getName != "WordSpecSpec.scala"
        }
      }.taskValue,
    ).dependsOn(commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestWordSpecTestDottyNative = project.in(file("dotty/wordspec-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative)
    .settings(
      projectTitle := "ScalaTest WordSpec Test",
      Test / sourceGenerators += Def.task {
        GenScalaTestDotty.genWordSpecTestNative((Test / sourceManaged).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)   

}
