import dotty.tools.sbtplugin.DottyPlugin.autoImport._
import sbt._
import Keys._
import com.typesafe.sbt.osgi.SbtOsgi
import com.typesafe.tools.mima.plugin.MimaKeys.{mimaPreviousArtifacts, mimaCurrentClassfiles, mimaBinaryIssueFilters}
import com.typesafe.tools.mima.core._
import com.typesafe.tools.mima.core.ProblemFilters._
import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.sbt.osgi.SbtOsgi
import com.typesafe.sbt.osgi.SbtOsgi.autoImport._

trait DottyBuild { this: BuildCommons =>

  // List of available night build at https://repo1.maven.org/maven2/ch/epfl/lamp/dotty-compiler_0.14/
  // lazy val dottyVersion = dottyLatestNightlyBuild.get
  lazy val dottyVersion = "0.24.0-RC1"
  lazy val dottySettings = List(
    scalaVersion := dottyVersion,
    libraryDependencies := libraryDependencies.value.map(_.withDottyCompat(scalaVersion.value)),
    scalacOptions ++= List("-language:implicitConversions", "-noindent", "-Xprint-suspension")
  )

  lazy val scalacticDotty = Project("scalacticDotty", file("scalactic.dotty"))
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
        Def.task{
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
      resourceGenerators in Compile += {
        Def.task {
          GenScalacticDotty.genResource((resourceManaged in Compile).value)
        }.taskValue
      },
      //scalacticDocSourcesSetting,
      //docTaskSetting,
      publishArtifact in (Compile, packageDoc) := false, // Temporary disable publishing of doc, can't get it to build.
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

  // https://github.com/sbt/sbt/issues/2205#issuecomment-144375501
  private lazy val packageManagedSources =
    mappings in (Compile, packageSrc) ++= { // publish generated sources
      val srcs = (managedSources in Compile).value
      val sdirs = (managedSourceDirectories in Compile).value
      val base = baseDirectory.value
      import Path._
      (srcs --- sdirs --- base) pair (relativeTo(sdirs) | relativeTo(base) | flat)
    }

  lazy val scalatestDotty = Project("scalatestDotty", file("scalatest.dotty"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest",
      organization := "org.scalatest",
      moduleName := "scalatest",
      initialCommands in console := """|import org.scalatest._
                                       |import org.scalactic._
                                       |import Matchers._""".stripMargin,
      libraryDependencies ++= scalaXmlDependency(scalaVersion.value),
      libraryDependencies ++= scalatestLibraryDependencies,
      packageManagedSources,
      sourceGenerators in Compile += {
        Def.task {
          GenScalaTestDotty.genScala((sourceManaged in Compile).value, version.value, scalaVersion.value) ++
          GenVersions.genScalaTestVersions((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
          ScalaTestGenResourcesJVM.genResources((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
          ScalaTestGenResourcesJVM.genFailureMessages((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)  ++
          GenConfigMap.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      },
      javaSourceManaged := target.value / "java",
      managedSourceDirectories in Compile += javaSourceManaged.value,
      sourceGenerators in Compile += {
        Def.task{
          GenScalaTestDotty.genJava((javaSourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      resourceGenerators in Compile += {
        Def.task {
          GenScalaTestDotty.genHtml((resourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      sourceGenerators in Compile += {
        Def.task{
          GenTable.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
          GenMatchers.genMainForDotty((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
          GenFactoriesDotty.genMain((sourceManaged in Compile).value / "org" / "scalatest" / "matchers" / "dsl", version.value, scalaVersion.value) ++
          GenCompatibleClasses.genMain((sourceManaged in Compile).value / "org" / "scalatest" / "tools", version.value, scalaVersion.value)
          //GenSafeStyles.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      },
      //scalatestJSDocTaskSetting,
      publishArtifact in (Compile, packageDoc) := false, // Temporary disable publishing of doc, can't get it to build.
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
      "org.scalatest.refspec",
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
      "Bundle-Description" -> "ScalaTest is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc.",
      "Main-Class" -> "org.scalatest.tools.Runner"
    )
  ).dependsOn(scalacticDotty)

  lazy val scalatestCoreDotty = Project("scalatestCoreDotty", file("modules/dotty/scalatest-core"))
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
      sourceGenerators in Compile += {
        Def.task {
          GenModulesDotty.genScalaTestCore((sourceManaged in Compile).value, version.value, scalaVersion.value) ++
          GenVersions.genScalaTestVersions((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
          ScalaTestGenResourcesJVM.genResources((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
          ScalaTestGenResourcesJVM.genFailureMessages((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)  ++
          GenConfigMap.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      },
      javaSourceManaged := target.value / "java",
      managedSourceDirectories in Compile += javaSourceManaged.value,
      sourceGenerators in Compile += {
        Def.task{
          GenScalaTestDotty.genJava((javaSourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      resourceGenerators in Compile += {
        Def.task {
          GenScalaTestDotty.genHtml((resourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      sourceGenerators in Compile += {
        Def.task{
          GenTable.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value) ++
          GenCompatibleClasses.genMain((sourceManaged in Compile).value / "org" / "scalatest" / "tools", version.value, scalaVersion.value)
          //GenSafeStyles.genMain((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      },
      //scalatestJSDocTaskSetting,
      publishArtifact in (Compile, packageDoc) := false, // Temporary disable publishing of doc, can't get it to build.
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
  ).dependsOn(scalacticDotty)

  lazy val scalatestFeatureSpecDotty = Project("scalatestFeatureSpecDotty", file("modules/dotty/scalatest-featurespec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest FeatureSpec Dotty",
      organization := "org.scalatest",
      moduleName := "scalatest-featurespec",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesDotty.genScalaTestFeatureSpec((sourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact in (Compile, packageDoc) := false, // Temporary disable publishing of doc, can't get it to build.
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.featurespec"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest FeatureSpec Dotty",
      "Bundle-Description" -> "ScalaTest is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestCoreDotty)

  lazy val scalatestFlatSpecDotty = Project("scalatestFlatSpecDotty", file("modules/dotty/scalatest-flatspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest FlatSpec Dotty",
      organization := "org.scalatest",
      moduleName := "scalatest-flatspec",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesDotty.genScalaTestFlatSpec((sourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact in (Compile, packageDoc) := false, // Temporary disable publishing of doc, can't get it to build.
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.flatspec"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest FlatSpec Dotty",
      "Bundle-Description" -> "ScalaTest is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestCoreDotty)

  lazy val scalatestFreeSpecDotty = Project("scalatestFreeSpecDotty", file("modules/dotty/scalatest-freespec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest FreeSpec Dotty",
      organization := "org.scalatest",
      moduleName := "scalatest-freespec",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesDotty.genScalaTestFreeSpec((sourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact in (Compile, packageDoc) := false, // Temporary disable publishing of doc, can't get it to build.
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.freespec"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest FreeSpec Dotty",
      "Bundle-Description" -> "ScalaTest is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestCoreDotty)

  lazy val scalatestFunSuiteDotty = Project("scalatestFunSuiteDotty", file("modules/dotty/scalatest-funsuite"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest FunSuite Dotty",
      organization := "org.scalatest",
      moduleName := "scalatest-funsuite",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesDotty.genScalaTestFunSuite((sourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact in (Compile, packageDoc) := false, // Temporary disable publishing of doc, can't get it to build.
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.funsuite"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest FunSuite Dotty",
      "Bundle-Description" -> "ScalaTest is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestCoreDotty)

  lazy val scalatestFunSpecDotty = Project("scalatestFunSpecDotty", file("modules/dotty/scalatest-funspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest FunSpec Dotty",
      organization := "org.scalatest",
      moduleName := "scalatest-funspec",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesDotty.genScalaTestFunSpec((sourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact in (Compile, packageDoc) := false, // Temporary disable publishing of doc, can't get it to build.
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.funspec"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest FunSpec Dotty",
      "Bundle-Description" -> "ScalaTest is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestCoreDotty)

  lazy val scalatestPropSpecDotty = Project("scalatestPropSpecDotty", file("modules/dotty/scalatest-propspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest PropSpec Dotty",
      organization := "org.scalatest",
      moduleName := "scalatest-propspec",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesDotty.genScalaTestPropSpec((sourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact in (Compile, packageDoc) := false, // Temporary disable publishing of doc, can't get it to build.
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.propspec"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest PropSpec Dotty",
      "Bundle-Description" -> "ScalaTest is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestCoreDotty)

  lazy val scalatestRefSpecDotty = Project("scalatestRefSpecDotty", file("modules/dotty/scalatest-refspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest RefSpec Dotty",
      organization := "org.scalatest",
      moduleName := "scalatest-refspec",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesDotty.genScalaTestRefSpec((sourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact in (Compile, packageDoc) := false, // Temporary disable publishing of doc, can't get it to build.
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.refspec"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest RefSpec Dotty",
      "Bundle-Description" -> "ScalaTest is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestCoreDotty)

  lazy val scalatestWordSpecDotty = Project("scalatestWordSpecDotty", file("modules/dotty/scalatest-wordspec"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest WordSpec Dotty",
      organization := "org.scalatest",
      moduleName := "scalatest-wordspec",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesDotty.genScalaTestWordSpec((sourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact in (Compile, packageDoc) := false, // Temporary disable publishing of doc, can't get it to build.
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.wordspec"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest WordSpec Dotty",
      "Bundle-Description" -> "ScalaTest is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestCoreDotty)

  lazy val scalatestDiagramsDotty = Project("scalatestDiagramsDotty", file("modules/dotty/scalatest-diagrams"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest Diagrams Dotty",
      organization := "org.scalatest",
      moduleName := "scalatest-diagrams",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesDotty.genScalaTestDiagrams((sourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact in (Compile, packageDoc) := false, // Temporary disable publishing of doc, can't get it to build.
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.diagrams"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest Diagrams Dotty",
      "Bundle-Description" -> "ScalaTest is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestCoreDotty)

  lazy val scalatestMatchersCoreDotty = Project("scalatestMatchersCoreDotty", file("modules/dotty/scalatest-matchers-core"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest Matchers Core Dotty",
      organization := "org.scalatest",
      moduleName := "scalatest-matchers-core",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesDotty.genScalaTestMatchersCore((sourceManaged in Compile).value, version.value, scalaVersion.value) ++ 
          GenFactoriesDotty.genMain((sourceManaged in Compile).value / "org" / "scalatest" / "matchers" / "dsl", version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact in (Compile, packageDoc) := false, // Temporary disable publishing of doc, can't get it to build.
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
      "Bundle-Name" -> "ScalaTest Matchers Core Dotty",
      "Bundle-Description" -> "ScalaTest is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestCoreDotty)

  lazy val scalatestShouldMatchersDotty = Project("scalatestShouldMatchersDotty", file("modules/dotty/scalatest-shouldmatchers"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest Should Matchers Dotty",
      organization := "org.scalatest",
      moduleName := "scalatest-shouldmatchers",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesDotty.genScalaTestShouldMatchers((sourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact in (Compile, packageDoc) := false, // Temporary disable publishing of doc, can't get it to build.
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.matchers.should"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest Should Matchers Dotty",
      "Bundle-Description" -> "ScalaTest is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestMatchersCoreDotty)

  lazy val scalatestMustMatchersDotty = Project("scalatestMustMatchersDotty", file("modules/dotty/scalatest-mustmatchers"))
    .enablePlugins(SbtOsgi)
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest Must Matchers Dotty",
      organization := "org.scalatest",
      moduleName := "scalatest-mustmatchers",
      sourceGenerators in Compile += {
        Def.task {
          GenModulesDotty.genScalaTestMustMatchers((sourceManaged in Compile).value, version.value, scalaVersion.value) ++ 
          GenMatchers.genMainForDotty((sourceManaged in Compile).value / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact in (Compile, packageDoc) := false, // Temporary disable publishing of doc, can't get it to build.
    ).settings(osgiSettings: _*).settings(
    OsgiKeys.exportPackage := Seq(
      "org.scalatest.matchers.must"
    ),
    OsgiKeys.importPackage := Seq(
      "org.scalatest.*",
      "*;resolution:=optional"
    ),
    OsgiKeys.additionalHeaders:= Map(
      "Bundle-Name" -> "ScalaTest Must Matchers Dotty",
      "Bundle-Description" -> "ScalaTest is an open-source test framework for the Javascript Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
      "Bundle-DocURL" -> "http://www.scalatest.org/",
      "Bundle-Vendor" -> "Artima, Inc."
    )
  ).dependsOn(scalatestMatchersCoreDotty)

  lazy val scalatestModulesDotty = (project in file("modules/dotty/modules-aggregation"))
    .settings(sharedSettings: _*)
    .settings(
      publishArtifact := false,
      publish := {},
      publishLocal := {},
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

  lazy val commonTestDotty = Project("commonTestDotty", file("common-test.dotty"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "Common test classes used by scalactic and scalatest",
      libraryDependencies ++= crossBuildTestLibraryDependencies.value,
      sourceGenerators in Compile += {
        Def.task{
          GenCommonTestDotty.genMain((sourceManaged in Compile).value, version.value, scalaVersion.value) ++
          GenGen.genMain((sourceManaged in Compile).value / "scala" / "org" / "scalatest" / "prop", version.value, scalaVersion.value) ++
          GenCompatibleClasses.genTest((sourceManaged in Compile).value, version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    ).dependsOn(scalacticDotty, LocalProject("scalatestDotty"))

  lazy val scalacticTestDotty = Project("scalacticTestDotty", file("scalactic-test.dotty"))
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
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      sourceGenerators in Test += Def.task {
        GenScalacticDotty.genTest((sourceManaged in Test).value, version.value, scalaVersion.value) /*++
        GenAnyVals.genTest((sourceManaged in Test).value / "scala" / "org" / "scalactic" / "anyvals", version.value, scalaVersion.value)*/
      }.taskValue
    ).dependsOn(scalacticDotty, scalatestDotty % "test", commonTestDotty % "test")

  lazy val scalatestTestDotty = Project("scalatestTestDotty", file("scalatest-test.dotty"))
    .settings(sharedSettings: _*)
    .settings(dottySettings: _*)
    .settings(
      projectTitle := "ScalaTest Test",
      organization := "org.scalatest",
      libraryDependencies ++= scalatestLibraryDependencies,
      //libraryDependencies ++= scalatestTestLibraryDependencies(scalaVersion.value),
      testOptions in Test := scalatestTestOptions,
      logBuffered in Test := false,
      //fork in Test := true,
      //parallelExecution in Test := true,
      //testForkedParallel in Test := true,
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestDotty.genTest((sourceManaged in Test).value, version.value, scalaVersion.value)
        }.taskValue
      },
      baseDirectory in Test := file("./"),
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    ).dependsOn(scalatestDotty % "test", commonTestDotty % "test")  

}
