import sbt._
import Keys._
import java.net.{URL, URLClassLoader}

object ScalatestBuild extends Build {

  val scalaVersionToUse = "2.10.3"
    
  val releaseVersion = "2.1.3-SNAPSHOT"

  lazy val scalatest = Project("scalatest", file("."))
   .settings(
     organization := "org.scalatest",
     version := releaseVersion,
     scalaVersion := scalaVersionToUse,
     scalacOptions ++= Seq("-no-specialization", "-feature", "-target:jvm-1.5"),
     initialCommands in console := """|import org.scalatest._
                                      |import org.scalautils._
                                      |import Matchers._""".stripMargin,
     ivyXML := 
       <dependency org="org.eclipse.jetty.orbit" name="javax.servlet" rev="3.0.0.v201112011016">
         <artifact name="javax.servlet" type="orbit" ext="jar"/>
       </dependency>, 
     libraryDependencies ++= simpledependencies,
     libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersionToUse % "provided",
     libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersionToUse, // this is needed to compile macro
     resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public",
     genMustMatchersTask, 
     genGenTask, 
     genTablesTask, 
     genCodeTask, 
     genFactoriesTask,
     genCompatibleClassesTask,
     sourceGenerators in Compile <+= 
         (baseDirectory, sourceManaged in Compile) map genFiles("gengen", "GenGen.scala")(GenGen.genMain),
     sourceGenerators in Compile <+= 
         (baseDirectory, sourceManaged in Compile) map genFiles("gentables", "GenTable.scala")(GenTable.genMain),
     sourceGenerators in Compile <+=
         (baseDirectory, sourceManaged in Compile) map genFiles("genmatchers", "MustMatchers.scala")(GenMatchers.genMain),
     sourceGenerators in Compile <+= 
         (baseDirectory, sourceManaged in Compile) map genFiles("genfactories", "GenFactories.scala")(GenFactories.genMain),
     sourceGenerators in Compile <+=
         (baseDirectory, sourceManaged in Compile) map genFiles("gencompcls", "GenCompatibleClasses.scala")(GenCompatibleClasses.genMain),
     testOptions in Test := Seq(Tests.Argument("-l", "org.scalatest.tags.Slow", 
                                               "-m", "org.scalatest", 
                                               "-m", "org.scalautils",
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
                                               "-oDI", 
                                               "-h", "target/html", 
                                               "-u", "target/junit", 
                                               "-fW", "target/result.txt"))
   )
   
  lazy val gentests = Project("gentests", file("gentests"))
   .settings(
     organization := "org.scalatest",
     version := releaseVersion,
     scalaVersion := scalaVersionToUse,
     scalacOptions ++= Seq("-no-specialization", "-feature"),
     libraryDependencies ++= simpledependencies,
     resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public",
     genMustMatchersTask, 
     genGenTask, 
     genTablesTask, 
     genInspectorsTask,
     genInspectorsShorthandsTask,
    genTheyWordTask,
     genContainTask, 
     genSortedTask, 
     genLoneElementTask, 
     genEmptyTask, 
     sourceGenerators in Test <+= 
         (baseDirectory, sourceManaged in Test) map genFiles("gengen", "GenGen.scala")(GenGen.genTest),
     sourceGenerators in Test <+= 
         (baseDirectory, sourceManaged in Test) map genFiles("gentables", "GenTable.scala")(GenTable.genTest),
     sourceGenerators in Test <+=
           (baseDirectory, sourceManaged in Test) map genFiles("genmatchers", "GenMatchers.scala")(GenMatchers.genTest),
     sourceGenerators in Test <+= 
         (baseDirectory, sourceManaged in Test) map genFiles("genthey", "GenTheyWord.scala")(GenTheyWord.genTest),
     sourceGenerators in Test <+= 
         (baseDirectory, sourceManaged in Test) map genFiles("geninspectors", "GenInspectors.scala")(GenInspectors.genTest),
     sourceGenerators in Test <+=
         (baseDirectory, sourceManaged in Test) map genFiles("geninspectorsshorthands", "GenInspectorsShorthands.scala")(GenInspectorsShorthands.genTest),
     sourceGenerators in Test <+= 
         (baseDirectory, sourceManaged in Test) map genFiles("gencontain", "GenContain.scala")(GenContain.genTest), 
     sourceGenerators in Test <+= 
         (baseDirectory, sourceManaged in Test) map genFiles("gensorted", "GenSorted.scala")(GenSorted.genTest),
     sourceGenerators in Test <+= 
         (baseDirectory, sourceManaged in Test) map genFiles("genloneelement", "GenLoneElement.scala")(GenLoneElement.genTest),
     sourceGenerators in Test <+= 
         (baseDirectory, sourceManaged in Test) map genFiles("genempty", "GenEmpty.scala")(GenEmpty.genTest),
     testOptions in Test := Seq(Tests.Argument("-l", "org.scalatest.tags.Slow", 
                                               "-oDI", 
                                               "-h", "gentests/target/html", 
                                               "-u", "gentests/target/junit", 
                                               "-fW", "target/result.txt"))
   ).dependsOn(scalatest  % "test->test")

   def simpledependencies = Seq(
     "org.scala-sbt" % "test-interface" % "1.0" % "optional",
     "org.scalacheck" %% "scalacheck" % "1.11.0" % "optional",
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

  def genFiles(name: String, generatorSource: String)(gen: (File, String) => Unit)(basedir: File, outDir: File): Seq[File] = {
    val tdir = outDir / "scala" / name
    val genSource = basedir / "project" / generatorSource
    def results = (tdir ** "*.scala").get
    if (results.isEmpty || results.exists(_.lastModified < genSource.lastModified)) {
      tdir.mkdirs()
      gen(tdir, scalaVersionToUse)
    }
    results
  }
  
  val genMustMatchers = TaskKey[Unit]("genmatchers", "Generate Must Matchers")
  val genMustMatchersTask = genMustMatchers <<= (sourceManaged in Compile, sourceManaged in Test, name) map { (mainTargetDir: File, testTargetDir: File, projName: String) =>
    projName match {
      case "scalatest" =>
        GenMatchers.genMain(new File(mainTargetDir, "scala/genmatchers"), scalaVersionToUse)
      case "gentests" =>
        GenMatchers.genTest(new File(testTargetDir, "scala/genmatchers"), scalaVersionToUse)
    }
  }
  
  val genGen = TaskKey[Unit]("gengen", "Generate Property Checks")
  val genGenTask = genGen <<= (sourceManaged in Compile, sourceManaged in Test, name) map { (mainTargetDir: File, testTargetDir: File, projName: String) =>
    projName match {
      case "scalatest" => 
        GenGen.genMain(new File(mainTargetDir, "scala/gengen"), scalaVersionToUse)
      case "gentests" =>
        GenGen.genTest(new File(testTargetDir, "scala/gengen"), scalaVersionToUse)
    }
  }
  
  val genTables = TaskKey[Unit]("gentables", "Generate Tables")
  val genTablesTask = genTables <<= (sourceManaged in Compile, sourceManaged in Test, name) map { (mainTargetDir: File, testTargetDir: File, projName: String) =>
    projName match {
      case "scalatest" => 
        GenTable.genMain(new File(mainTargetDir, "scala/gentables"), scalaVersionToUse)
      case "gentests" => 
        GenTable.genTest(new File(testTargetDir, "scala/gentables"), scalaVersionToUse)
    }
  }
  
  val genTheyWord = TaskKey[Unit]("genthey", "Generate They Word tests")
  val genTheyWordTask = genTheyWord <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenTheyWord.genTest(new File(testTargetDir, "scala/genthey"), scalaVersionToUse)
  }
  
  val genInspectors = TaskKey[Unit]("geninspectors", "Generate Inspectors tests")
  val genInspectorsTask = genInspectors <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenInspectors.genTest(new File(testTargetDir, "scala/geninspectors"), scalaVersionToUse)
  }

  val genInspectorsShorthands = TaskKey[Unit]("geninspectorsshorthands", "Generate Inspectors Shorthands tests")
  val genInspectorsShorthandsTask = genInspectorsShorthands <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenInspectorsShorthands.genTest(new File(testTargetDir, "scala/geninspectorsshorthands"), scalaVersionToUse)
  }
  
  val genFactories = TaskKey[Unit]("genfactories", "Generate Matcher Factories")
  val genFactoriesTask = genFactories <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenFactories.genMain(new File(mainTargetDir, "scala/genfactories"), scalaVersionToUse)
  }

  val genCompatibleClasses = TaskKey[Unit]("gencompcls", "Generate Compatible Classes for Java 6 & 7")
  val genCompatibleClassesTask = genCompatibleClasses <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenCompatibleClasses.genMain(new File(mainTargetDir, "scala/gencompclass"), scalaVersionToUse)
  }
  
  val genContain = TaskKey[Unit]("gencontain", "Generate contain matcher tests")
  val genContainTask = genContain <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenContain.genTest(new File(testTargetDir, "scala/gencontain"), scalaVersionToUse)
  }
  
  val genSorted = TaskKey[Unit]("gensorted", "Generate sorted matcher tests")
  val genSortedTask = genSorted <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenSorted.genTest(new File(testTargetDir, "scala/gensorted"), scalaVersionToUse)
  }
  
  val genLoneElement = TaskKey[Unit]("genloneelement", "Generate lone element matcher tests")
  val genLoneElementTask = genLoneElement <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenLoneElement.genTest(new File(testTargetDir, "scala/genloneelement"), scalaVersionToUse)
  }
  
  val genEmpty = TaskKey[Unit]("genempty", "Generate empty matcher tests")
  val genEmptyTask = genEmpty <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenEmpty.genTest(new File(testTargetDir, "scala/genempty"), scalaVersionToUse)
  }

  val genCode = TaskKey[Unit]("gencode", "Generate Code, includes Must Matchers and They Word tests.")
  val genCodeTask = genCode <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenGen.genMain(new File(mainTargetDir, "scala/gengen"), scalaVersionToUse)
    GenTable.genMain(new File(mainTargetDir, "scala/gentables"), scalaVersionToUse)
    GenMatchers.genMain(new File(mainTargetDir, "scala/genmatchers"), scalaVersionToUse)
    GenFactories.genMain(new File(mainTargetDir, "scala/genfactories"), scalaVersionToUse)
  }
}
