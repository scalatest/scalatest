import dotty.tools.sbtplugin.DottyPlugin.autoImport._
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

import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._

trait DottyBuild { this: BuildCommons =>

  // List of available night build at https://repo1.maven.org/maven2/ch/epfl/lamp/dotty-compiler_0.27/
  // lazy val dottyVersion = dottyLatestNightlyBuild.get
  lazy val dottyVersion = System.getProperty("scalatest.dottyVersion", "3.0.0")
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
  ).enablePlugins(ScalaJSPlugin)

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
      libraryDependencies += "org.scala-lang.modules" %%% "scala-xml" % "2.0.0", 
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
        publishArtifact in (Compile, packageDoc) := false, // Temporary disable publishing of doc, can't get it to build.
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
        
  }
  
  lazy val scalatestFeatureSpecDotty = project.in(file("dotty/featurespec"))
    .scalatestStyleModule("featurespec", "ScalaTest FeatureSpec Dotty")

  lazy val scalatestFeatureSpecDottyJS = project.in(file("dotty/featurespec.js"))
    .scalatestStyleModuleJS("featurespec", "ScalaTest FeatureSpec Dotty JS")  

  lazy val scalatestFlatSpecDotty = project.in(file("dotty/flatspec"))
    .scalatestStyleModule("flatspec", "ScalaTest FlatSpec Dotty")

  lazy val scalatestFlatSpecDottyJS = project.in(file("dotty/flatspec.js"))
    .scalatestStyleModuleJS("flatspec", "ScalaTest FlatSpec Dotty JS")  

  lazy val scalatestFreeSpecDotty = project.in(file("dotty/freespec"))
    .scalatestStyleModule("freespec", "ScalaTest FreeSpec Dotty")

  lazy val scalatestFreeSpecDottyJS = project.in(file("dotty/freespec.js"))
    .scalatestStyleModuleJS("freespec", "ScalaTest FreeSpec Dotty JS")  

  lazy val scalatestFunSuiteDotty = project.in(file("dotty/funsuite"))
    .scalatestStyleModule("funsuite", "ScalaTest FunSuite Dotty")

  lazy val scalatestFunSuiteDottyJS = project.in(file("dotty/funsuite.js"))
    .scalatestStyleModuleJS("funsuite", "ScalaTest FunSuite Dotty JS")  

  lazy val scalatestFunSpecDotty = project.in(file("dotty/funspec"))
    .scalatestStyleModule("funspec", "ScalaTest FunSpec Dotty")

  lazy val scalatestFunSpecDottyJS = project.in(file("dotty/funspec.js"))
    .scalatestStyleModuleJS("funspec", "ScalaTest FunSpec Dotty JS")  

  lazy val scalatestPropSpecDotty = project.in(file("dotty/propspec"))
    .scalatestStyleModule("propspec", "ScalaTest PropSpec Dotty")

  lazy val scalatestPropSpecDottyJS = project.in(file("dotty/propspec.js"))
    .scalatestStyleModuleJS("propspec", "ScalaTest PropSpec Dotty JS")  

  lazy val scalatestRefSpecDotty = project.in(file("dotty/refspec"))
    .scalatestStyleModule("refspec", "ScalaTest RefSpec Dotty")

  lazy val scalatestRefSpecDottyJS = project.in(file("dotty/refspec.js"))
    .scalatestStyleModuleJS("refspec", "ScalaTest RefSpec Dotty JS")  

  lazy val scalatestWordSpecDotty = project.in(file("dotty/wordspec"))
    .scalatestStyleModule("wordspec", "ScalaTest WordSpec Dotty")

  lazy val scalatestWordSpecDottyJS = project.in(file("dotty/wordspec.js"))
    .scalatestStyleModuleJS("wordspec", "ScalaTest WordSpec Dotty JS")  

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
      "ScalaTest Must Matchers DottyJS ",
      (targetDir, version, scalaVersion) =>
        GenMatchers.genMainForDottyJS(targetDir / "org" / "scalatest", version, scalaVersion) ++ 
        GenScalaTestDotty.genMustMatchersScalaJS(targetDir, version, scalaVersion)
    ).settings(
    OsgiKeys.exportPackage := Seq("org.scalatest.matchers.must"),
  ).dependsOn(scalatestMatchersCoreDottyJS).enablePlugins(ScalaJSPlugin)

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

  def scalatestTestDottyJSOptions =
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
      "-m", "org.scalatest.funspec",
      "-m", "org.scalatest.funsuite",
      "-m", "org.scalatest.propspec",
      "-m", "org.scalatest.wordspec",
      "-oDIF"))    

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
      testOptions in Test := scalatestTestDottyJSOptions,
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

}
