import dotty.tools.sbtplugin.DottyPlugin.autoImport._
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
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.{scalaJSLinkerConfig, jsEnv}

import scalanative.sbtplugin.ScalaNativePlugin

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

trait DottyBuild { this: BuildCommons =>

  // List of available night build at https://repo1.maven.org/maven2/ch/epfl/lamp/dotty-compiler_0.27/
  // lazy val dottyVersion = dottyLatestNightlyBuild.get
  lazy val dottyVersion = System.getProperty("scalatest.dottyVersion", "3.1.3")
  lazy val dottySettings = List(
    scalaVersion := dottyVersion,
    scalacOptions ++= List("-language:implicitConversions", "-noindent", "-Xprint-suspension")
  )

  // https://github.com/sbt/sbt/issues/2205#issuecomment-144375501
  private lazy val packageManagedSources =
    mappings in (Compile, packageSrc) ++= { // publish generated sources
      val srcs = (managedSources in Compile).value
      val sdirs = (managedSourceDirectories in Compile).value
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
      initialCommands in console := "import org.scalactic._",
      packageManagedSources,
      sourceGenerators in Compile += {
        Def.task {
          // From scalactic-macro
          GenScalacticDotty.genMacroScala((sourceManaged in Compile).value, version.value, scalaVersion.value) ++
          ScalacticGenResourcesJVM.genResources((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenAnyVals.genMain((sourceManaged in Compile).value / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value, true) ++
          GenEvery.genMain((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenColCompatHelper.genMain((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          // end from scalactic-macro
          GenScalacticDotty.genScala((sourceManaged in Compile).value, version.value, scalaVersion.value) ++
          GenVersions.genScalacticVersions((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          ScalacticGenResourcesJVM.genFailureMessages((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenArrayHelper.genMain((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value)
        }.taskValue
      },
      resourceGenerators in Compile += Def.task {
        GenScalacticDotty.genResource((resourceManaged in Compile).value)
      }.taskValue,
      //scalacticDocSourcesSetting,
      //docTaskSetting,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
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
      initialCommands in console := "import org.scalactic._",
      packageManagedSources,
      sourceGenerators in Compile += {
        Def.task {
          // From scalactic-macro
          GenScalacticDotty.genMacroScala((sourceManaged in Compile).value, version.value, scalaVersion.value) ++
          ScalacticGenResourcesJSVM.genResources((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenAnyVals.genMain((sourceManaged in Compile).value / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value, true) ++
          GenEvery.genMain((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenColCompatHelper.genMain((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          // end from scalactic-macro
          GenScalacticDotty.genScalaJS((sourceManaged in Compile).value, version.value, scalaVersion.value) ++
          GenVersions.genScalacticVersions((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          ScalacticGenResourcesJSVM.genFailureMessages((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenArrayHelper.genMain((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value)
        }.taskValue
      },
      resourceGenerators in Compile += Def.task {
        GenScalacticDotty.genResource((resourceManaged in Compile).value)
      }.taskValue,
      //scalacticDocSourcesSetting,
      //docTaskSetting,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
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
      initialCommands in console := "import org.scalactic._",
      packageManagedSources,
      sourceGenerators in Compile += {
        Def.task {
          // From scalactic-macro
          GenScalacticDotty.genMacroScala((sourceManaged in Compile).value, version.value, scalaVersion.value) ++
          ScalacticGenResourcesJSVM.genResources((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenAnyVals.genMain((sourceManaged in Compile).value / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value, true) ++
          GenEvery.genMain((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenColCompatHelper.genMain((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          // end from scalactic-macro
          GenScalacticDotty.genScalaNative((sourceManaged in Compile).value, version.value, scalaVersion.value) ++ 
          GenVersions.genScalacticVersions((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          ScalacticGenResourcesJSVM.genFailureMessages((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value) ++
          GenArrayHelper.genMain((sourceManaged in Compile).value / "org" / "scalactic", version.value, scalaVersion.value)
        }.taskValue
      },
      resourceGenerators in Compile += Def.task {
        GenScalacticDotty.genResource((resourceManaged in Compile).value)
      }.taskValue,
      //scalacticDocSourcesSetting,
      //docTaskSetting,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar")
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
      initialCommands in console := """|import org.scalatest._
                                       |import org.scalactic._
                                       |import Matchers._""".stripMargin,
      libraryDependencies ++= scalaXmlDependency(scalaVersion.value),
      libraryDependencies ++= scalatestLibraryDependencies,
      packageManagedSources,
      sourceGenerators in Compile += Def.task {
        GenModulesDotty.genScalaTestCore((sourceManaged in Compile).value, version.value, scalaVersion.value) ++
        GenVersions.genScalaTestVersions((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
        ScalaTestGenResourcesJVM.genResources((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
        ScalaTestGenResourcesJVM.genFailureMessages((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)  ++
        GenConfigMap.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
      }.taskValue,
      javaSourceManaged := target.value / "java",
      managedSourceDirectories in Compile += javaSourceManaged.value,
      sourceGenerators in Compile += Def.task {
        GenScalaTestDotty.genJava((javaSourceManaged in Compile).value, version.value, scalaVersion.value)
      }.taskValue,
      resourceGenerators in Compile += Def.task {
          GenScalaTestDotty.genHtml((resourceManaged in Compile).value, version.value, scalaVersion.value)
      }.taskValue,
      sourceGenerators in Compile += Def.task {
        GenTable.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
        GenCompatibleClasses.genMain((sourceManaged in Compile).value / "org" / "scalatest" / "tools", version.value, scalaVersion.value)
        //GenSafeStyles.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
      }.taskValue,
      //scalatestJSDocTaskSetting,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar"),
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
      initialCommands in console := """|import org.scalatest._
                                       |import org.scalactic._
                                       |import Matchers._""".stripMargin,
      libraryDependencies += "org.scala-lang.modules" %%% "scala-xml" % "2.3.0",
      libraryDependencies += ("org.scala-js" %% "scalajs-test-interface" % scalaJSVersion).withDottyCompat(dottyVersion), 
      packageManagedSources,
      sourceGenerators in Compile += Def.task {
        GenModulesDotty.genScalaTestCoreJS((sourceManaged in Compile).value, version.value, scalaVersion.value) ++
        GenScalaTestDotty.genScalaJS((sourceManaged in Compile).value, version.value, scalaVersion.value) ++
        GenVersions.genScalaTestVersions((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
        ScalaTestGenResourcesJSVM.genResources((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
        ScalaTestGenResourcesJSVM.genFailureMessages((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)  ++
        GenConfigMap.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
      }.taskValue,
      javaSourceManaged := target.value / "java",
      managedSourceDirectories in Compile += javaSourceManaged.value,
      sourceGenerators in Compile += Def.task {
        GenScalaTestDotty.genJava((javaSourceManaged in Compile).value, version.value, scalaVersion.value)
      }.taskValue,
      resourceGenerators in Compile += Def.task {
          GenScalaTestDotty.genHtml((resourceManaged in Compile).value, version.value, scalaVersion.value)
      }.taskValue,
      sourceGenerators in Compile += Def.task {
        GenTable.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
        //GenSafeStyles.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
      }.taskValue,
      //scalatestJSDocTaskSetting,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar"),
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
      initialCommands in console := """|import org.scalatest._
                                       |import org.scalactic._
                                       |import Matchers._""".stripMargin,
      libraryDependencies += "org.scala-lang.modules" %%% "scala-xml" % "2.3.0",
      libraryDependencies += ("org.scala-native" %%% "test-interface" % nativeVersion),
      packageManagedSources,
      sourceGenerators in Compile += Def.task {
        GenModulesDotty.genScalaTestCoreNative((sourceManaged in Compile).value, version.value, scalaVersion.value) ++
        GenScalaTestDotty.genScalaNative((sourceManaged in Compile).value, version.value, scalaVersion.value) ++
        GenVersions.genScalaTestVersions((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
        ScalaTestGenResourcesJSVM.genResources((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
        ScalaTestGenResourcesJSVM.genFailureMessages((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)  ++
        GenConfigMap.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
      }.taskValue,
      javaSourceManaged := target.value / "java",
      managedSourceDirectories in Compile += javaSourceManaged.value,
      sourceGenerators in Compile += Def.task {
        GenScalaTestDotty.genJava((javaSourceManaged in Compile).value, version.value, scalaVersion.value)
      }.taskValue,
      resourceGenerators in Compile += Def.task {
          GenScalaTestDotty.genHtml((resourceManaged in Compile).value, version.value, scalaVersion.value)
      }.taskValue,
      sourceGenerators in Compile += Def.task {
        GenTable.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
        //GenSafeStyles.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
      }.taskValue,
      //scalatestJSDocTaskSetting,
      mimaPreviousArtifacts := Set(organization.value %% name.value % previousReleaseVersion),
      mimaCurrentClassfiles := (classDirectory in Compile).value.getParentFile / (name.value + "_" + scalaBinaryVersion.value + "-" + releaseVersion + ".jar"),
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
    def scalatestModule(name: String, title: String): Project = p
      .enablePlugins(SbtOsgi)
      .settings(sharedSettings: _*)
      .settings(dottySettings: _*)
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

    /** common settings for all scalatest sub modules (all modules, except the `scalatest` module) */
    def scalatestSubModule(name: String, title: String, gen: GenModulesDotty.GenFn): Project =
      scalatestModule(name, title).settings(
        sourceGenerators in Compile += Def.task {
          gen((sourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue,
        OsgiKeys.importPackage := Seq(
          "org.scalatest.*",
          "*;resolution:=optional"
        ),
      )

    /** common settings for all scalatest `style` modules such as `featurespec`, `funsuite`,.. */
    def scalatestStyleModule(style: String, title: String): Project =
      scalatestSubModule(s"scalatest-$style", title, GenModulesDotty(style))
        .settings(
          OsgiKeys.exportPackage := Seq(s"org.scalatest.$style"),
        ).dependsOn(scalatestCoreDotty)

    /** common settings for all scalatest js `style` modules such as `featurespec`, `funsuite`,.. */
    def scalatestStyleModuleJS(style: String, title: String): Project =
      scalatestSubModule(s"scalatest-$style", title, GenModulesDotty.applyJS(style))
        .settings(
          OsgiKeys.exportPackage := Seq(s"org.scalatest.$style"),
        ).dependsOn(scalatestCoreDottyJS).enablePlugins(ScalaJSPlugin)

    /** common settings for all scalatest native `style` modules such as `featurespec`, `funsuite`,.. */
    def scalatestStyleModuleNative(style: String, title: String): Project =
      scalatestSubModule(s"scalatest-$style", title, GenModulesDotty.applyNative(style))
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
      (targetDir, version, scalaVersion) =>
        GenScalaTestDotty.genDiagramsScalaNative(targetDir / "org" / "scalatest", version, scalaVersion)
    ).settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest", 
        "org.scalatest.diagrams"
      ),
    ).dependsOn(scalatestCoreDottyNative).enablePlugins(ScalaNativePlugin)  

  lazy val scalatestMatchersCoreDotty = project.in(file("dotty/matchers-core"))
    .scalatestSubModule(
      "scalatest-matchers-core",
      "ScalaTest Matchers Core Dotty",
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
      GenModulesDotty.genScalaTestShouldMatchers
    ).settings(
      OsgiKeys.exportPackage := Seq("org.scalatest.matchers.should"),
    ).dependsOn(scalatestMatchersCoreDotty)

  lazy val scalatestShouldMatchersDottyJS = project.in(file("dotty/shouldmatchers.js"))
    .scalatestSubModule(
      "scalatest-shouldmatchers",
      "ScalaTest Should Matchers Dotty JS",
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
      (targetDir, version, scalaVersion) =>
        GenMatchers.genMainForDotty(targetDir / "org" / "scalatest", version, scalaVersion)
    ).settings(
    OsgiKeys.exportPackage := Seq("org.scalatest.matchers.must"),
  ).dependsOn(scalatestMatchersCoreDotty)

  lazy val scalatestMustMatchersDottyJS = project.in(file("dotty/mustmatchers.js"))
    .scalatestSubModule(
      "scalatest-mustmatchers",
      "ScalaTest Must Matchers Dotty JS ",
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
      scalacOptions in (Compile, doc) := List.empty
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
      scalatestMatchersCoreDotty, 
      scalatestShouldMatchersDotty, 
      scalatestMustMatchersDotty
    )

  lazy val scalatestDotty = project.in(file("dotty/scalatest"))
    .scalatestModule("scalatest", "ScalaTest Dotty")
    .settings(
      // Little trick to get rid of bnd error when publish.
      sourceGenerators in Compile += Def.task {
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
      scalatestMatchersCoreDotty, 
      scalatestShouldMatchersDotty, 
      scalatestMustMatchersDotty
    )

  lazy val scalatestDottyJS = project.in(file("dotty/scalatest.js"))
    .scalatestModule("scalatest", "ScalaTest Dotty JS")
    .settings(
      // Little trick to get rid of bnd error when publish.
      sourceGenerators in Compile += Def.task {
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
      scalatestMatchersCoreDottyJS, 
      scalatestShouldMatchersDottyJS, 
      scalatestMustMatchersDottyJS
    ).enablePlugins(ScalaJSPlugin)

  lazy val scalatestDottyNative = project.in(file("dotty/scalatest.native"))
    .scalatestModule("scalatest", "ScalaTest Dotty Native")
    .settings(
      // Little trick to get rid of bnd error when publish.
      sourceGenerators in Compile += Def.task {
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
      mappings in (Compile, packageBin) ++= mappings.in(scalacticDotty, Compile, packageBin).value,
      // include the scalactic sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalacticDotty, Compile, packageSrc).value,
      // include the scalatestCompatible classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestCompatible, Compile, packageBin).value,
      // include the scalatestCompatible sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestCompatible, Compile, packageSrc).value,
      // include the scalatest classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestCoreDotty, Compile, packageBin).value,
      // include the scalatest sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestCoreDotty, Compile, packageSrc).value,
      // include the scalatestFeatureSpecDotty classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestFeatureSpecDotty, Compile, packageBin).value,
      // include the scalatestFeatureSpecDotty sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestFeatureSpecDotty, Compile, packageSrc).value,
      // include the scalatestFlatSpecDotty classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestFlatSpecDotty, Compile, packageBin).value,
      // include the scalatestFlatSpecDotty sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestFlatSpecDotty, Compile, packageSrc).value,
      // include the scalatestFreeSpecDotty classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestFreeSpecDotty, Compile, packageBin).value,
      // include the scalatestFreeSpecDotty sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestFreeSpecDotty, Compile, packageSrc).value,
      // include the scalatestFunSuiteDotty classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestFunSuiteDotty, Compile, packageBin).value,
      // include the scalatestFunSuiteDotty sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestFunSuiteDotty, Compile, packageSrc).value,
      // include the scalatestFunSpecDotty classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestFunSpecDotty, Compile, packageBin).value,
      // include the scalatestFunSpecDotty sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestFunSpecDotty, Compile, packageSrc).value,
      // include the scalatestPropSpecDotty classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestPropSpecDotty, Compile, packageBin).value,
      // include the scalatestPropSpecDotty sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestPropSpecDotty, Compile, packageSrc).value,
      // include the scalatestWordSpecDotty classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestWordSpecDotty, Compile, packageBin).value,
      // include the scalatestWordSpecDotty sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestWordSpecDotty, Compile, packageSrc).value,
      // include the scalatestDiagramsDotty classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestDiagramsDotty, Compile, packageBin).value,
      // include the scalatestDiagramsDotty sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestDiagramsDotty, Compile, packageSrc).value,
      // include the scalatestMatchersCoreDotty classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestMatchersCoreDotty, Compile, packageBin).value,
      // include the scalatestMatchersCoreDotty sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestMatchersCoreDotty, Compile, packageSrc).value,
      // include the scalatestShouldMatchersDotty classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestShouldMatchersDotty, Compile, packageBin).value,
      // include the scalatestShouldMatchersDotty sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestShouldMatchersDotty, Compile, packageSrc).value,
      // include the scalatestMustMatchersDotty classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestMustMatchersDotty, Compile, packageBin).value,
      // include the scalatestMustMatchersDotty sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestMustMatchersDotty, Compile, packageSrc).value, 
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
      libraryDependencies += ("org.scala-js" %% "scalajs-test-interface" % scalaJSVersion).withDottyCompat(dottyVersion), 
      // include the scalactic classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalacticDottyJS, Compile, packageBin).value,
      // include the scalactic sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalacticDottyJS, Compile, packageSrc).value,
      // include the scalatest classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestCoreDottyJS, Compile, packageBin).value,
      // include the scalatest sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestCoreDottyJS, Compile, packageSrc).value,
      // include the scalatestFeatureSpecDottyJS classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestFeatureSpecDottyJS, Compile, packageBin).value,
      // include the scalatestFeatureSpecDottyJS sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestFeatureSpecDottyJS, Compile, packageSrc).value,
      // include the scalatestFlatSpecDottyJS classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestFlatSpecDottyJS, Compile, packageBin).value,
      // include the scalatestFlatSpecDottyJS sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestFlatSpecDottyJS, Compile, packageSrc).value,
      // include the scalatestFreeSpecDottyJS classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestFreeSpecDottyJS, Compile, packageBin).value,
      // include the scalatestFreeSpecDottyJS sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestFreeSpecDottyJS, Compile, packageSrc).value,
      // include the scalatestFunSuiteDottyJS classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestFunSuiteDottyJS, Compile, packageBin).value,
      // include the scalatestFunSuiteDottyJS sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestFunSuiteDottyJS, Compile, packageSrc).value,
      // include the scalatestFunSpecDottyJS classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestFunSpecDottyJS, Compile, packageBin).value,
      // include the scalatestFunSpecDottyJS sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestFunSpecDottyJS, Compile, packageSrc).value,
      // include the scalatestPropSpecDottyJS classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestPropSpecDottyJS, Compile, packageBin).value,
      // include the scalatestPropSpecDottyJS sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestPropSpecDottyJS, Compile, packageSrc).value,
      // include the scalatestWordSpecDottyJS classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestWordSpecDottyJS, Compile, packageBin).value,
      // include the scalatestWordSpecDottyJS sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestWordSpecDottyJS, Compile, packageSrc).value,
      // include the scalatestDiagramsDottyJS classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestDiagramsDottyJS, Compile, packageBin).value,
      // include the scalatestDiagramsDottyJS sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestDiagramsDottyJS, Compile, packageSrc).value,
      // include the scalatestMatchersCoreDottyJS classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestMatchersCoreDottyJS, Compile, packageBin).value,
      // include the scalatestMatchersCoreDottyJS sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestMatchersCoreDottyJS, Compile, packageSrc).value,
      // include the scalatestShouldMatchersDottyJS classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestShouldMatchersDottyJS, Compile, packageBin).value,
      // include the scalatestShouldMatchersDottyJS sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestShouldMatchersDottyJS, Compile, packageSrc).value,
      // include the scalatestMustMatchersDottyJS classes and resources in the jar
      mappings in (Compile, packageBin) ++= mappings.in(scalatestMustMatchersDottyJS, Compile, packageBin).value,
      // include the scalatestMustMatchersDottyJS sources in the source jar
      mappings in (Compile, packageSrc) ++= mappings.in(scalatestMustMatchersDottyJS, Compile, packageSrc).value,
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
        mappings in (Compile, packageBin) ++= mappings.in(scalacticDottyNative, Compile, packageBin).value,
        // include the scalacticDottyNative sources in the source jar
        mappings in (Compile, packageSrc) ++= mappings.in(scalacticDottyNative, Compile, packageSrc).value,
        // include the scalacticDottyNative classes and resources in the jar
        mappings in (Compile, packageBin) ++= mappings.in(scalatestCoreDottyNative, Compile, packageBin).value,
        // include the scalacticDottyNative sources in the source jar
        mappings in (Compile, packageSrc) ++= mappings.in(scalatestCoreDottyNative, Compile, packageSrc).value,
        // include the scalatestFeatureSpecDottyNative classes and resources in the jar
        mappings in (Compile, packageBin) ++= mappings.in(scalatestFeatureSpecDottyNative, Compile, packageBin).value,
        // include the scalatestFeatureSpecDottyNative sources in the source jar
        mappings in (Compile, packageSrc) ++= mappings.in(scalatestFeatureSpecDottyNative, Compile, packageSrc).value,
        // include the scalatestFlatSpecDottyNative classes and resources in the jar
        mappings in (Compile, packageBin) ++= mappings.in(scalatestFlatSpecDottyNative, Compile, packageBin).value,
        // include the scalatestFlatSpecDottyNative sources in the source jar
        mappings in (Compile, packageSrc) ++= mappings.in(scalatestFlatSpecDottyNative, Compile, packageSrc).value,
        // include the scalatestFreeSpecDottyNative classes and resources in the jar
        mappings in (Compile, packageBin) ++= mappings.in(scalatestFreeSpecDottyNative, Compile, packageBin).value,
        // include the scalatestFreeSpecDottyNative sources in the source jar
        mappings in (Compile, packageSrc) ++= mappings.in(scalatestFreeSpecDottyNative, Compile, packageSrc).value,
        // include the scalatestFunSuiteDottyNative classes and resources in the jar
        mappings in (Compile, packageBin) ++= mappings.in(scalatestFunSuiteDottyNative, Compile, packageBin).value,
        // include the scalatestFunSuiteDottyNative sources in the source jar
        mappings in (Compile, packageSrc) ++= mappings.in(scalatestFunSuiteDottyNative, Compile, packageSrc).value,
        // include the scalatestFunSpecDottyNative classes and resources in the jar
        mappings in (Compile, packageBin) ++= mappings.in(scalatestFunSpecDottyNative, Compile, packageBin).value,
        // include the scalatestFunSpecDottyNative sources in the source jar
        mappings in (Compile, packageSrc) ++= mappings.in(scalatestFunSpecDottyNative, Compile, packageSrc).value,
        // include the scalatestPropSpecDottyNative classes and resources in the jar
        mappings in (Compile, packageBin) ++= mappings.in(scalatestPropSpecDottyNative, Compile, packageBin).value,
        // include the scalatestPropSpecDottyNative sources in the source jar
        mappings in (Compile, packageSrc) ++= mappings.in(scalatestPropSpecDottyNative, Compile, packageSrc).value,
        // include the scalatestWordSpecDottyNative classes and resources in the jar
        mappings in (Compile, packageBin) ++= mappings.in(scalatestWordSpecDottyNative, Compile, packageBin).value,
        // include the scalatestWordSpecDottyNative sources in the source jar
        mappings in (Compile, packageSrc) ++= mappings.in(scalatestWordSpecDottyNative, Compile, packageSrc).value,
        // include the scalatestDiagramsDottyNative classes and resources in the jar
        mappings in (Compile, packageBin) ++= mappings.in(scalatestDiagramsDottyNative, Compile, packageBin).value,
        // include the scalatestDiagramsDottyNative sources in the source jar
        mappings in (Compile, packageSrc) ++= mappings.in(scalatestDiagramsDottyNative, Compile, packageSrc).value,
        // include the scalatestMatchersCoreDottyNative classes and resources in the jar
        mappings in (Compile, packageBin) ++= mappings.in(scalatestMatchersCoreDottyNative, Compile, packageBin).value,
        // include the scalatestMatchersCoreDottyNative sources in the source jar
        mappings in (Compile, packageSrc) ++= mappings.in(scalatestMatchersCoreDottyNative, Compile, packageSrc).value,
        // include the scalatestShouldMatchersDottyNative classes and resources in the jar
        mappings in (Compile, packageBin) ++= mappings.in(scalatestShouldMatchersDottyNative, Compile, packageBin).value,
        // include the scalatestShouldMatchersDottyNative sources in the source jar
        mappings in (Compile, packageSrc) ++= mappings.in(scalatestShouldMatchersDottyNative, Compile, packageSrc).value,
        // include the scalatestMustMatchersDottyNative classes and resources in the jar
        mappings in (Compile, packageBin) ++= mappings.in(scalatestMustMatchersDottyNative, Compile, packageBin).value,
        // include the scalatestMustMatchersDottyNative sources in the source jar
        mappings in (Compile, packageSrc) ++= mappings.in(scalatestMustMatchersDottyNative, Compile, packageSrc).value,
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
      sourceGenerators in Compile += Def.task {
        GenCommonTestDotty.genMain((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value) ++
        GenGen.genMain((sourceManaged in Compile).value / "scala" / "org" / "scalatest" / "prop", version.value, scalaVersion.value) ++
        GenCompatibleClasses.genTest((sourceManaged in Compile).value, version.value, scalaVersion.value)
      }.taskValue,
      noPublishSettings,
    ).dependsOn(scalacticDotty, LocalProject("scalatestDotty"))

  lazy val commonTestDottyJS = project.in(file("dotty/common-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "Common test classes used by scalactic and scalatest",
      libraryDependencies ++= crossBuildTestLibraryDependencies.value,
      sourceGenerators in Compile += Def.task {
        GenCommonTestDotty.genMainJS((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value) ++
        GenGen.genMain((sourceManaged in Compile).value / "scala" / "org" / "scalatest" / "prop", version.value, scalaVersion.value) ++
        GenCompatibleClasses.genTest((sourceManaged in Compile).value, version.value, scalaVersion.value)
      }.taskValue,
      noPublishSettings,
    ).dependsOn(scalacticDottyJS, LocalProject("scalatestDottyJS")).enablePlugins(ScalaJSPlugin)

  lazy val commonTestDottyNative = project.in(file("dotty/common-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "Common test classes used by scalactic and scalatest",
      libraryDependencies ++= crossBuildTestLibraryDependencies.value,
      sourceGenerators in Compile += Def.task {
        GenCommonTestDotty.genMainJS((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value) ++
        GenGen.genMain((sourceManaged in Compile).value / "scala" / "org" / "scalatest" / "prop", version.value, scalaVersion.value) ++
        GenCompatibleClasses.genTest((sourceManaged in Compile).value, version.value, scalaVersion.value)
      }.taskValue,
      noPublishSettings,
    ).dependsOn(scalacticDottyNative, LocalProject("scalatestDottyNative")).enablePlugins(ScalaNativePlugin)  

  lazy val scalacticTestDotty = project.in(file("dotty/scalactic-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "Scalactic Test",
      organization := "org.scalactic",
      testOptions in Test ++=
        Seq(Tests.Argument(TestFrameworks.ScalaTest,
          "-oDIF",
          "-W", "120", "60")),    
      logBuffered in Test := false,
      noPublishSettings,
      sourceGenerators in Test += Def.task {
        GenScalacticDotty.genTest((sourceManaged in Test).value, version.value, scalaVersion.value) /*++
        GenAnyVals.genTest((sourceManaged in Test).value / "scala" / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value)*/
      }.taskValue
    ).dependsOn(scalacticDotty, scalatestDotty % "test", commonTestDotty % "test")

  lazy val scalacticTestDottyJS = project.in(file("dotty/scalactic-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "Scalactic Test JS",
      organization := "org.scalactic",
      scalaJSLinkerConfig ~= { _.withOptimizer(false) },
      testOptions in Test ++=
        Seq(Tests.Argument(TestFrameworks.ScalaTest, "-oDIF")),
      jsEnv := {
        import org.scalajs.jsenv.nodejs.NodeJSEnv
        new NodeJSEnv(
          NodeJSEnv.Config()
            .withArgs(List("--max_old_space_size=3000")))
      }, 
      parallelExecution in Test := false,
      fork in Test := false,
      logBuffered in Test := false,
      noPublishSettings,
      sourceGenerators in Test += Def.task {
        GenScalacticDotty.genTestJS((sourceManaged in Test).value, version.value, scalaVersion.value) /*++
        GenAnyVals.genTest((sourceManaged in Test).value / "scala" / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value)*/
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
      sourceGenerators in Test += Def.task {
        GenScalacticDotty.genTestNative((sourceManaged in Test).value, version.value, scalaVersion.value) /*++
        GenAnyVals.genTest((sourceManaged in Test).value / "scala" / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value)*/
      }.taskValue
    ).dependsOn(scalacticDottyNative, scalatestDottyNative % "test", commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)  

  def sharedTestSettingsDotty: Seq[Setting[_]] = 
    Seq(
      organization := "org.scalatest",
      libraryDependencies ++= scalatestLibraryDependencies,
      libraryDependencies ++= 
        Seq(
          "org.scalatestplus" %% "testng-6-7" % plusTestNGVersion % "test",
          "org.scalatestplus" %% "junit-4-13" % plusJUnitVersion % "test"
        ),
      testOptions in Test := scalatestTestOptions,
      logBuffered in Test := false,
      //fork in Test := true,
      //parallelExecution in Test := true,
      //testForkedParallel in Test := true,
      baseDirectory in Test := file("./"),
    ) ++ noPublishSettings

  lazy val scalatestTestDotty = project.in(file("dotty/scalatest-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest Test",
      javaSourceManaged := target.value / "java",
      sourceGenerators in Test += Def.task {
        GenRegularTests4.genJava((javaSourceManaged in Compile).value) ++
        GenScalaTestDotty.genTest((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDotty % "test").aggregate(
      scalatestDiagramsTestDotty, 
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
      testOptions in Test := scalatestTestJSNativeOptions,
      parallelExecution in Test := false,
      fork in Test := false,
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
      scalaJSLinkerConfig ~= { _.withOptimizer(false).withSemantics(_.withStrictFloats(true)) },
      sourceGenerators in Test += Def.task {
        //GenRegularTests4.genJava((javaSourceManaged in Compile).value) ++
        GenScalaTestDotty.genTestJS((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(scalacticDottyJS, scalatestDottyJS % "test", commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)
     .aggregate(
       scalatestDiagramsTestDottyJS, 
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
      sourceGenerators in Test += Def.task {
        //GenRegularTests4.genJava((javaSourceManaged in Compile).value) ++
        GenScalaTestDotty.genTestNative((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(scalacticDottyNative, scalatestDottyNative % "test", commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)
     .aggregate(
       scalatestDiagramsTestDottyNative, 
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
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genDiagramsTest((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDotty % "test")

  lazy val scalatestDiagramsTestDottyJS = project.in(file("dotty/diagrams-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest Diagrams Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genDiagramsTestJS((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestDiagramsTestDottyNative = project.in(file("dotty/diagrams-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative)
    .settings(
      projectTitle := "ScalaTest Diagrams Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genDiagramsTestNative((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)  

  lazy val scalatestFeatureSpecTestDotty = project.in(file("dotty/featurespec-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest FeatureSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genFeatureSpecTest((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDotty % "test")

  lazy val scalatestFeatureSpecTestDottyJS = project.in(file("dotty/featurespec-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest FeatureSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genFeatureSpecTestJS((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFeatureSpecTestDottyNative = project.in(file("dotty/featurespec-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative)
    .settings(
      projectTitle := "ScalaTest FeatureSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genFeatureSpecTestNative((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)  

  lazy val scalatestFlatSpecTestDotty = project.in(file("dotty/flatspec-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest FlatSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genFlatSpecTest((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDotty % "test")

  lazy val scalatestFlatSpecTestDottyJS = project.in(file("dotty/flatspec-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest FlatSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genFlatSpecTestJS((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFlatSpecTestDottyNative = project.in(file("dotty/flatspec-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative)
    .settings(
      projectTitle := "ScalaTest FlatSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genFlatSpecTestNative((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)  

  lazy val scalatestFreeSpecTestDotty = project.in(file("dotty/freespec-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest FreeSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genFreeSpecTest((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDotty % "test")

  lazy val scalatestFreeSpecTestDottyJS = project.in(file("dotty/freespec-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest FreeSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genFreeSpecTestJS((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFreeSpecTestDottyNative = project.in(file("dotty/freespec-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative)
    .settings(
      projectTitle := "ScalaTest FreeSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genFreeSpecTestNative((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)  

  lazy val scalatestFunSpecTestDotty = project.in(file("dotty/funspec-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest FunSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genFunSpecTest((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDotty % "test")

  lazy val scalatestFunSpecTestDottyJS = project.in(file("dotty/funspec-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest FunSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genFunSpecTestJS((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFunSpecTestDottyNative = project.in(file("dotty/funspec-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative)
    .settings(
      projectTitle := "ScalaTest FunSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genFunSpecTestNative((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)  

  lazy val scalatestFunSuiteTestDotty = project.in(file("dotty/funsuite-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest FunSuite Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genFunSuiteTest((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDotty % "test")

  lazy val scalatestFunSuiteTestDottyJS = project.in(file("dotty/funsuite-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest FunSuite Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genFunSuiteTestJS((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestFunSuiteTestDottyNative = project.in(file("dotty/funsuite-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative)
    .settings(
      projectTitle := "ScalaTest FunSuite Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genFunSuiteTestNative((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)  

  lazy val scalatestPropSpecTestDotty = project.in(file("dotty/propspec-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest PropSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genPropSpecTest((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDotty % "test")

  lazy val scalatestPropSpecTestDottyJS = project.in(file("dotty/propspec-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest PropSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genPropSpecTestJS((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestPropSpecTestDottyNative = project.in(file("dotty/propspec-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative)
    .settings(
      projectTitle := "ScalaTest PropSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genPropSpecTestNative((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)               

  lazy val scalatestWordSpecTestDotty = project.in(file("dotty/wordspec-test"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDotty)
    .settings(
      projectTitle := "ScalaTest WordSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genWordSpecTest((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDotty % "test")

  lazy val scalatestWordSpecTestDottyJS = project.in(file("dotty/wordspec-test.js"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsDottyJS)
    .settings(
      projectTitle := "ScalaTest WordSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genWordSpecTestJS((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestWordSpecTestDottyNative = project.in(file("dotty/wordspec-test.native"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(sharedTestSettingsNative)
    .settings(
      projectTitle := "ScalaTest WordSpec Test",
      sourceGenerators in Test += Def.task {
        GenScalaTestDotty.genWordSpecTestNative((sourceManaged in Test).value, version.value, scalaVersion.value)
      }.taskValue,
    ).dependsOn(commonTestDottyNative % "test").enablePlugins(ScalaNativePlugin)   

}
