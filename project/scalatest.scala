import sbt._
import Keys._
import java.net.{URL, URLClassLoader}

object ScalatestBuild extends Build {

  val scalaVersionToUse = "2.10.0"
    
  val releaseVersion = "2.0.M6-SNAP27"
    
  val sbtVersionToUse = "0.13.0-RC1"
  
  val includeTestPackageSet = Set("org.scalatest", 
                                  "org.scalatest.fixture", 
                                  "org.scalatest.concurrent", 
                                  "org.scalatest.testng", 
                                  "org.scalatest.junit", 
                                  "org.scalatest.events", 
                                  "org.scalatest.prop", 
                                  "org.scalatest.tools", 
                                  "org.scalatest.matchers", 
                                  "org.scalatest.suiteprop", 
                                  "org.scalatest.mock", 
                                  "org.scalatest.path", 
                                  "org.scalatest.selenium")
                                                     
  def isIncludedPackage(className: String) = {
    try {
      val packageName = className.substring(0, className.lastIndexOf("."))
      includeTestPackageSet.contains(packageName)
    }
    catch {
      case e: Exception => 
        e.printStackTrace()
        false
    }
  }
                              
  lazy val scalatest = Project("scalatest", file("."))
   .settings(
     organization := "org.scalatest",
     version := releaseVersion,
     scalaVersion := scalaVersionToUse,
     sbtVersion := sbtVersionToUse, 
     ivyXML := 
       <dependency org="org.eclipse.jetty.orbit" name="javax.servlet" rev="3.0.0.v201112011016">
         <artifact name="javax.servlet" type="orbit" ext="jar"/>
       </dependency>, 
     libraryDependencies ++= simpledependencies,
     resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public",
     genMustMatchersTask, 
     genGenTask, 
     genTablesTask, 
     genCodeTask, 
     genFactoriesTask, 
     sourceGenerators in Compile <+= 
         (baseDirectory, sourceManaged in Compile) map genFiles("gengen", "GenGen.scala")(GenGen.genMain),
     sourceGenerators in Compile <+= 
         (baseDirectory, sourceManaged in Compile) map genFiles("gentables", "GenTable.scala")(GenTable.genMain),
     sourceGenerators in Compile <+=
         (baseDirectory, sourceManaged in Compile) map genFiles("genmatchers", "MustMatchers.scala")(GenMatchers.genMain),
     sourceGenerators in Compile <+= 
         (baseDirectory, sourceManaged in Compile) map genFiles("genfactories", "GenFactories.scala")(GenFactories.genMain),
     testOptions in Test := Seq(Tests.Filter(s => isIncludedPackage(s)), 
                                Tests.Argument("-l", "org.scalatest.tags.Slow", 
                                               "-oDI", 
                                               "-h", "target/html", 
                                               "-u", "target/junit"))
   )
   
  lazy val gentests = Project("gentests", file("gentests"))
   .settings(
     organization := "org.scalatest",
     version := releaseVersion,
     scalaVersion := scalaVersionToUse,
     sbtVersion := sbtVersionToUse, 
     libraryDependencies ++= simpledependencies,
     resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public",
     genMustMatchersTask, 
     genGenTask, 
     genTablesTask, 
     genInspectorsTask, 
     genTheyWordTask, 
     genContainTask, 
     genSortedTask, 
     genLoneElementTask, 
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
         (baseDirectory, sourceManaged in Test) map genFiles("gencontain", "GenContain.scala")(GenContain.genTest), 
     sourceGenerators in Test <+= 
         (baseDirectory, sourceManaged in Test) map genFiles("gensorted", "GenSorted.scala")(GenSorted.genTest),
     sourceGenerators in Test <+= 
         (baseDirectory, sourceManaged in Test) map genFiles("genloneelement", "GenLoneElement.scala")(GenLoneElement.genTest),
     testOptions in Test := Seq(Tests.Argument("-l", "org.scalatest.tags.Slow", 
                                               "-oDI", 
                                               "-h", "gentests/target/html", 
                                               "-u", "gentests/target/junit"))
   ).dependsOn(scalatest  % "test->test")

   def simpledependencies = Seq(
     "org.scala-sbt" % "test-interface" % "1.0" % "optional", 
     "org.scalacheck" % ("scalacheck_" + scalaVersionToUse) % "1.10.0" % "optional", 
     "org.easymock" % "easymockclassextension" % "3.1" % "optional", 
     "org.jmock" % "jmock-legacy" % "2.5.1" % "optional", 
     "org.mockito" % "mockito-all" % "1.9.0" % "optional", 
     "org.testng" % "testng" % "6.3.1" % "optional", 
     "com.google.inject" % "guice" % "3.0" % "optional", 
     "junit" % "junit" % "4.10" % "optional", 
     "org.seleniumhq.selenium" % "selenium-java" % "2.31.0" % "optional",  
     "org.apache.ant" % "ant" % "1.7.1" % "optional", 
     "net.sourceforge.cobertura" % "cobertura" % "1.9.1" % "test",
     "commons-io" % "commons-io" % "1.3.2" % "test", 
     "org.eclipse.jetty" % "jetty-server" % "8.1.8.v20121106" % "test", 
     "org.eclipse.jetty" % "jetty-webapp" % "8.1.8.v20121106" % "test", 
     "asm" % "asm" % "3.3.1" % "optional", 
     "org.pegdown" % "pegdown" % "1.1.0" % "optional" 
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
  
  val genFactories = TaskKey[Unit]("genfactories", "Generate Matcher Factories")
  val genFactoriesTask = genFactories <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenFactories.genMain(new File(mainTargetDir, "scala/genfactories"), scalaVersionToUse)
  }
  
  val genContain = TaskKey[Unit]("gencontain", "Generate contain matcher tests")
  val genContainTask = genContain <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenContain.genTest(new File(testTargetDir, "scala/contain"), scalaVersionToUse)
  }
  
  val genSorted = TaskKey[Unit]("gensorted", "Generate sorted matcher tests")
  val genSortedTask = genSorted <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenSorted.genTest(new File(testTargetDir, "scala/gensorted"), scalaVersionToUse)
  }
  
  val genLoneElement = TaskKey[Unit]("genloneelement", "Generate lone element matcher tests")
  val genLoneElementTask = genLoneElement <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenLoneElement.genTest(new File(testTargetDir, "scala/genloneelement"), scalaVersionToUse)
  }

  val genCode = TaskKey[Unit]("gencode", "Generate Code, includes Must Matchers and They Word tests.")
  val genCodeTask = genCode <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenGen.genMain(new File(mainTargetDir, "scala/gengen"), scalaVersionToUse)
    GenTable.genMain(new File(mainTargetDir, "scala/gentables"), scalaVersionToUse)
    GenMatchers.genMain(new File(mainTargetDir, "scala/genmatchers"), scalaVersionToUse)
    GenFactories.genMain(new File(mainTargetDir, "scala/genfactories"), scalaVersionToUse)
  }
}
