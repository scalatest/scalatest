import sbt._
import Keys._
import java.net.{URL, URLClassLoader}

object ScalatestBuild extends Build {

  val scalaVersionToUse = "2.9.2"
  
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
   
  def isRunnable(className: String, testLoader: ClassLoader): Boolean = {
    val clazz = testLoader.loadClass(className)
    clazz.getAnnotations.find(a => a.annotationType.getName == "org.scalatest.WrapWith") match {
      case Some(wrapWithAnnotation) => 
        val wrapperSuiteClazz = wrapWithAnnotation.getClass.getMethod("value").invoke(wrapWithAnnotation).asInstanceOf[Class[_]]
        val constructorList = wrapperSuiteClazz.getDeclaredConstructors()
        constructorList.exists { c => 
          val types = c.getParameterTypes
          types.length == 1 && types(0) == classOf[java.lang.Class[_]]
        }
      case None =>
        false
    }
  }
  
  def isDiscoverableSuite(className: String, testLoader: ClassLoader): Boolean = {
    val clazz = testLoader.loadClass(className)
    !clazz.getAnnotations.exists(_.annotationType.getName == "org.scalatest.DoNotDiscover")
  }
  
  val emptyClassArray = new Array[java.lang.Class[T] forSome { type T }](0)
  
  def isAccessibleSuite(className: String, testLoader: ClassLoader): Boolean = {
    import java.lang.reflect.Modifier
    try {
      val clazz = testLoader.loadClass(className)
      Modifier.isPublic(clazz.getModifiers) &&
      !Modifier.isAbstract(clazz.getModifiers) &&
      Modifier.isPublic(clazz.getConstructor(emptyClassArray: _*).getModifiers)
    }
    catch {
      case nsme: NoSuchMethodException => false
      case se: SecurityException => false
    }
  }
                              
  def isScalaTestSuite(className: String, testLoader: ClassLoader): Boolean = {
    isIncludedPackage(className) &&
    isDiscoverableSuite(className, testLoader) && 
    (isAccessibleSuite(className, testLoader) || isRunnable(className, testLoader))
  }
                              
   lazy val scalatest = Project("scalatest", file("."))
   .settings(
     organization := "org.scalatest",
     version := "2.0.M5-SNAPSHOT",
     scalaVersion := scalaVersionToUse,
     libraryDependencies ++= simpledependencies,
     resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public",
     genMustMatchersTask, 
     genGenTask, 
     genTablesTask, 
     genTheyWordTask, 
     genCodeTask, 
     genInspectorsTask, 
     sourceGenerators in Compile <+= 
         (baseDirectory, sourceManaged in Compile) map genFiles("gen", "GenGen.scala")(GenGen.genMain),
     sourceGenerators in Compile <+= 
         (baseDirectory, sourceManaged in Compile) map genFiles("gentables", "GenTable.scala")(GenTable.genMain),
     sourceGenerators in Compile <+=
           (baseDirectory, sourceManaged in Compile) map genFiles("matchers", "MustMatchers.scala")(GenMatchers.genMain),
     testOptions in Test <<= 
         (fullClasspath in Test) map { classPathList =>
           val urlList:Seq[URL] = classPathList map {attr => new File(attr.data.getAbsolutePath()).toURL }
           val testLoader = new URLClassLoader(urlList.toArray)
           Seq(Tests.Filter(className => isScalaTestSuite(className, testLoader)))
         }
   ).aggregate("gentests")
   
   lazy val gentests = Project("gentests", file("gen"))
   .settings(
     organization := "org.scalatest",
     version := "2.0.M5-SNAPSHOT",
     scalaVersion := scalaVersionToUse,
     libraryDependencies ++= simpledependencies,
     resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public",
     sourceGenerators in Test <+= 
         (baseDirectory, sourceManaged in Test) map genFiles("gen", "GenGen.scala")(GenGen.genTest),
     sourceGenerators in Test <+= 
         (baseDirectory, sourceManaged in Test) map genFiles("gentables", "GenTable.scala")(GenTable.genTest),
     sourceGenerators in Test <+=
           (baseDirectory, sourceManaged in Test) map genFiles("matchers", "GenMatchers.scala")(GenMatchers.genTest),
     sourceGenerators in Test <+= 
         (baseDirectory, sourceManaged in Test) map genFiles("genthey", "GenTheyWord.scala")(GenTheyWord.genTest),
     sourceGenerators in Test <+= 
         (baseDirectory, sourceManaged in Test) map genFiles("geninspectors", "GenInspectors.scala")(GenInspectors.genTest),
     testOptions in Test <<= 
         (fullClasspath in Test) map { classPathList =>
           val urlList:Seq[URL] = classPathList map {attr => new File(attr.data.getAbsolutePath()).toURL }
           val testLoader = new URLClassLoader(urlList.toArray)
           Seq(Tests.Filter(className => isScalaTestSuite(className, testLoader)))
         }
   ).dependsOn(scalatest  % "test->test")

   def simpledependencies = Seq(
     "org.scala-tools.testing" % "test-interface" % "0.5",  // TODO optional
     "org.scalacheck" % ("scalacheck_" + scalaVersionToUse) % "1.10.0",   // TODO optional
     "org.easymock" % "easymockclassextension" % "3.1",   // TODO optional
     "org.jmock" % "jmock-legacy" % "2.5.1", // TODO optional
     "org.mockito" % "mockito-all" % "1.9.0", // TODO optional
     "org.testng" % "testng" % "6.3.1",  // TODO optional
     "com.google.inject" % "guice" % "3.0", // TODO optional
     "junit" % "junit" % "4.10", // TODO optional
     "org.seleniumhq.selenium" % "selenium-java" % "2.25.0", // TODO optional 
     "com.typesafe.akka" % "akka-actor" % "2.0.2", // TODO optional
     "org.apache.ant" % "ant" % "1.7.1", // TODO optional
     "net.sourceforge.cobertura" % "cobertura" % "1.9.1" % "test",
     "commons-io" % "commons-io" % "1.3.2" % "test", 
     "org.eclipse.jetty" % "jetty-server" % "8.0.1.v20110908" % "test", 
     "org.eclipse.jetty" % "jetty-webapp" % "8.0.1.v20110908" % "test", 
     "asm" % "asm" % "3.3.1", // TODO optional
     "org.pegdown" % "pegdown" % "1.1.0" // TODO optional
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
  
  val genMustMatchers = TaskKey[Unit]("genmustmatchers", "Generate Must Matchers")
  val genMustMatchersTask = genMustMatchers <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenMatchers.genMain(new File("gen/target/scala-" + scalaVersionToUse + "/src_managed/main/matchers"), scalaVersionToUse)
    GenMatchers.genTest(new File("gen/target/scala-" + scalaVersionToUse + "/src_managed/test/matchers"), scalaVersionToUse)
  }
  
  val genGen = TaskKey[Unit]("gengen", "Generate Property Checks")
  val genGenTask = genGen <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenGen.genMain(new File("gen/target/scala-" + scalaVersionToUse + "/src_managed/main/gen"), scalaVersionToUse)
    GenGen.genTest(new File("gen/target/scala-" + scalaVersionToUse + "/src_managed/test/gen"), scalaVersionToUse)
  }
  
  val genTables = TaskKey[Unit]("gentables", "Generate Tables")
  val genTablesTask = genTables <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenTable.genMain(new File("gen/target/scala-" + scalaVersionToUse + "/src_managed/main/gentables"), scalaVersionToUse)
    GenTable.genTest(new File("gen/target/scala-" + scalaVersionToUse + "/src_managed/test/gentables"), scalaVersionToUse)
  }
  
  val genTheyWord = TaskKey[Unit]("genthey", "Generate They Word tests")
  val genTheyWordTask = genTheyWord <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenTheyWord.genTest(new File("gen/target/scala-" + scalaVersionToUse + "/src_managed/test/genthey"), scalaVersionToUse)
  }
  
  val genInspectors = TaskKey[Unit]("geninspectors", "Generate Inspectors tests")
  val genInspectorsTask = genInspectors <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenInspectors.genTest(new File("gen/target/scala-" + scalaVersionToUse + "/src_managed/test/geninspectors"), scalaVersionToUse)
  }

  val genCode = TaskKey[Unit]("gencode", "Generate Code, includes Must Matchers and They Word tests.")
  val genCodeTask = genCode <<= (sourceManaged in Compile, sourceManaged in Test) map { (mainTargetDir: File, testTargetDir: File) =>
    GenMatchers.genMain(new File("target/scala-" + scalaVersionToUse + "/src_managed/main/matchers"), scalaVersionToUse)
    GenMatchers.genTest(new File("gen/target/scala-" + scalaVersionToUse + "/src_managed/test/matchers"), scalaVersionToUse)
    GenTheyWord.genTest(new File("gen/target/scala-" + scalaVersionToUse + "/src_managed/test/genthey"), scalaVersionToUse)
    GenInspectors.genTest(new File("gen/target/scala-" + scalaVersionToUse + "/src_managed/test/geninspectors"), scalaVersionToUse)
  }
}
