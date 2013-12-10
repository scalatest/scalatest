/*
 * Copyright 2001-2013 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest.tools

import org.scalatest._
import scala.collection.mutable
import scala.sys.process._
import java.io.File
import java.util.regex.Pattern
import java.util.regex.Matcher.quoteReplacement
import SuiteDiscoveryHelper.discoverTests
import SuiteDiscoveryHelper.discoverSuiteNames
import SuiteDiscoveryHelper.discoverJUnitClassNames
import org.apache.commons.io.FileUtils

class SuiteDiscoveryHelperFriend(sdt: SuiteDiscoveryHelper.type) {

  def transformToClassName(fileName: String, fileSeparator: Char): Option[String] = {
    val m = Class.forName("org.scalatest.tools.SuiteDiscoveryHelper$").getDeclaredMethod("org$scalatest$tools$SuiteDiscoveryHelper$$transformToClassName",
      Array(classOf[String], classOf[Char]): _*)
    m.setAccessible(true)
    m.invoke(sdt, Array[Object](fileName, new java.lang.Character(fileSeparator)): _*).asInstanceOf[Option[String]]
  }

  def extractClassNames(fileNames: Iterator[String], fileSeparator: Char): Iterator[String] = {
    val m = Class.forName("org.scalatest.tools.SuiteDiscoveryHelper$").getDeclaredMethod("org$scalatest$tools$SuiteDiscoveryHelper$$extractClassNames",
      Array(classOf[Iterator[String]], classOf[Char]): _*)
    m.setAccessible(true)
    m.invoke(sdt, Array[Object](fileNames, new java.lang.Character(fileSeparator)): _*).asInstanceOf[Iterator[String]]
  }

  def isAccessibleSuite(clazz: java.lang.Class[_]): Boolean = {
    val m = Class.forName("org.scalatest.tools.SuiteDiscoveryHelper$").getDeclaredMethod("isAccessibleSuite",
      Array(classOf[Class[_]]): _*) // This one works in 2.7
      // Array(classOf[Class])) // This one works in 2.6
    m.setAccessible(true)
    m.invoke(sdt, Array[Object](clazz): _*).asInstanceOf[Boolean]
  }
  
  def isRunnable(clazz: java.lang.Class[_]): Boolean = {
    val m = Class.forName("org.scalatest.tools.SuiteDiscoveryHelper$").getDeclaredMethod("isRunnable",
      Array(classOf[Class[_]]): _*) // This one works in 2.7
    m.setAccessible(true)
    m.invoke(sdt, Array[Object](clazz): _*).asInstanceOf[Boolean]
  }

  def getFileNamesSetFromFile(file: File, fileSeparator: Char): Set[String] = {
    val m = Class.forName("org.scalatest.tools.SuiteDiscoveryHelper$").getDeclaredMethod("org$scalatest$tools$SuiteDiscoveryHelper$$getFileNamesSetFromFile",
      Array(classOf[File], classOf[Char]): _*)
    m.setAccessible(true)
    m.invoke(sdt, Array[Object](file, new java.lang.Character(fileSeparator)): _*).asInstanceOf[Set[String]]
  }
  
  def isDiscoverableSuite(clazz: java.lang.Class[_]): Boolean = {
    val m = Class.forName("org.scalatest.tools.SuiteDiscoveryHelper$").getDeclaredMethod("isDiscoverableSuite",
      Array(classOf[Class[_]]): _*)
    m.setAccessible(true)
    m.invoke(sdt, Array[Object](clazz): _*).asInstanceOf[Boolean]
  }
}

class SuiteDiscoveryHelperSuite extends Suite {

  val sdtf = new SuiteDiscoveryHelperFriend(SuiteDiscoveryHelper)
  val loader = getClass.getClassLoader
  val accessibleSuites =
    Set(
      "org.scalatest.tools.RunnerSpec",
      "org.scalatest.tools.SuiteDiscoveryHelperSuite",
      "org.scalatest.tools.SuiteDiscoveryHelperSuite2")

  //
  // Given this Suite's name and one of its test names,
  // discoverTests should return a SuiteParam object for this
  // Suite and the specified test.
  //
  def testDiscoverTests1() {
    val testSpecs = List(TestSpec("testDiscoverTests1", false))

    val suiteParams = discoverTests(testSpecs, accessibleSuites, loader)

    assert(suiteParams.length === 1)

    val suiteParam = suiteParams(0)

    assert(suiteParam.className === "org.scalatest.tools.SuiteDiscoveryHelperSuite")
    assert(suiteParam.testNames.length === 1)
    assert(suiteParam.testNames(0) === "testDiscoverTests1")
    assert(suiteParam.wildcardTestNames.length === 0)
    assert(suiteParam.nestedSuites.length === 0)
  }

  //
  // Given two test names, where only one is found, discoverTests should
  // return a SuiteParam with just the one test name.
  //
  def testDiscoverTests2() {
    val testSpecs =
      List(
        TestSpec("testDiscoverTests2", false),
        TestSpec("testDiscoverTestsX", false)
      )

    val suiteParams =
      discoverTests(testSpecs, accessibleSuites, loader)

    assert(suiteParams.length === 1)

    val suiteParam = suiteParams(0)

    assert(suiteParam.className === "org.scalatest.tools.SuiteDiscoveryHelperSuite")
    assert(suiteParam.testNames.length === 1)
    assert(suiteParam.testNames(0) === "testDiscoverTests2")
    assert(suiteParam.wildcardTestNames.length === 0)
    assert(suiteParam.nestedSuites.length === 0)
  }

  //
  // Given two test names, where both are found, discoverTests should
  // return a SuiteParam with both test names.
  //
  def testDiscoverTests3() {
    val testSpecs =
      List(
        TestSpec("testDiscoverTests2", false),
        TestSpec("testDiscoverTests1", false)
      )

    val suiteParams =
      discoverTests(testSpecs, accessibleSuites, loader)

    assert(suiteParams.length === 1)

    val suiteParam = suiteParams(0)

    assert(suiteParam.className === "org.scalatest.tools.SuiteDiscoveryHelperSuite")
    assert(suiteParam.testNames.length === 2)
    assert(suiteParam.testNames(0) === "testDiscoverTests1")
    assert(suiteParam.testNames(1) === "testDiscoverTests2")
    assert(suiteParam.wildcardTestNames.length === 0)
    assert(suiteParam.nestedSuites.length === 0)
  }

  //
  // Two test names, where both are in one Suite and one is in
  // two Suites.
  //
  def testDiscoverTests4() {
    val testSpecs =
      List(
        TestSpec("testDiscoverTests4", false),
        TestSpec("testDiscoverTests1", false)
      )

    val suiteParams =
      discoverTests(testSpecs, accessibleSuites, loader)

    assert(suiteParams.length === 2)

    val suiteParam0 = suiteParams(0)

    assert(suiteParam0.className === "org.scalatest.tools.SuiteDiscoveryHelperSuite")
    assert(suiteParam0.testNames.length === 2)
    assert(suiteParam0.testNames(0) === "testDiscoverTests1")
    assert(suiteParam0.testNames(1) === "testDiscoverTests4")
    assert(suiteParam0.wildcardTestNames.length === 0)
    assert(suiteParam0.nestedSuites.length === 0)

    val suiteParam1 = suiteParams(1)

    assert(suiteParam1.className === "org.scalatest.tools.SuiteDiscoveryHelperSuite2")
    assert(suiteParam1.testNames.length === 1)
    assert(suiteParam1.testNames(0) === "testDiscoverTests4")
    assert(suiteParam1.wildcardTestNames.length === 0)
    assert(suiteParam1.nestedSuites.length === 0)
  }

  //
  // Discover tests using a substring.  This should discover tests in
  // two Suites.
  //
  def testDiscoverTestsA1() {
    val testSpecs =
      List(
        TestSpec("testDiscoverTestsA", true)
      )

    val suiteParams =
      discoverTests(testSpecs, accessibleSuites, loader)

    assert(suiteParams.length === 2)

    val suiteParam0 = suiteParams(0)

    assert(suiteParam0.className ===
           "org.scalatest.tools.SuiteDiscoveryHelperSuite")
    assert(suiteParam0.testNames.length === 0)
    assert(suiteParam0.wildcardTestNames.length === 1)
    assert(suiteParam0.wildcardTestNames(0) === "testDiscoverTestsA")
    assert(suiteParam0.nestedSuites.length === 0)

    val suiteParam1 = suiteParams(1)

    assert(suiteParam1.className ===
           "org.scalatest.tools.SuiteDiscoveryHelperSuite2")
    assert(suiteParam1.testNames.length === 0)
    assert(suiteParam1.wildcardTestNames.length === 1)
    assert(suiteParam1.wildcardTestNames(0) === "testDiscoverTestsA")
    assert(suiteParam1.nestedSuites.length === 0)
  }

  def testTransformToClassName() {
    assert(sdtf.transformToClassName("bob.class", '/') === Some("bob"))
    assert(sdtf.transformToClassName("a.b.c.bob.class", '/') === Some("a.b.c.bob"))
    assert(sdtf.transformToClassName("a.b.c.bob", '/') === None)
    assert(sdtf.transformToClassName("", '/') === None)
    assert(sdtf.transformToClassName("notdotclass", '/') === None)
    assert(sdtf.transformToClassName(".class", '/') === None)
    assert(sdtf.transformToClassName("a/b/c/bob.class", '/') === Some("a.b.c.bob"))
    assert(sdtf.transformToClassName("a/b/c/bob", '/') === None)
    assert(sdtf.transformToClassName("/.class", '/') === None)
    assert(sdtf.transformToClassName("..class", '/') === Some("."))
    assert(sdtf.transformToClassName("a\\b\\c\\bob.class", '\\') === Some("a.b.c.bob"))
    assert(sdtf.transformToClassName("a\\b\\c\\bob", '\\') === None)
    assert(sdtf.transformToClassName("\\.class", '\\') === None)
  }

  def testIsAccessibleSuite() {
    assert(sdtf.isAccessibleSuite(classOf[SuiteDiscoveryHelperSuite])) 
    assert(!sdtf.isAccessibleSuite(classOf[PackageAccessSuite]))
    assert(!sdtf.isAccessibleSuite(classOf[PackageAccessConstructorSuite]))
    assert(!sdtf.isAccessibleSuite(classOf[Suite]))
    assert(!sdtf.isAccessibleSuite(classOf[Object]))
  }

  def testExtractClassNames() {
    assert(sdtf.extractClassNames(List("bob.class").iterator, '/').toList === List("bob"))
    assert(sdtf.extractClassNames(List("bob.class", "manifest.txt", "a/b/c/bob.class").iterator, '/').toList === List("bob", "a.b.c.bob"))
    assert(sdtf.extractClassNames(List("bob.class", "manifest.txt", "a\\b\\c\\bob.class").iterator, '\\').toList === List("bob", "a.b.c.bob"))
    assert(sdtf.extractClassNames(List("bob.class", "manifest.txt", "/a/b/c/bob.class").iterator, '/').toList === List("bob", "a.b.c.bob"))
  }

  def testGetFileNamesSetFromFile() {
    
    assert(sdtf.getFileNamesSetFromFile(new File("harness/fnIteratorTest/empty.txt"), '/') === Set("empty.txt"))
    /*
    This one doesn't work now that I've checked the harness into subversion, because it finds the svn files.
    So I need to first copy just the files I want somewhere, then run this.
    assert(sdtf.getFileNamesSetFromFile(new File("harness/fnIteratorTest"), '/') === Set("subDir2/inSubDir2.class",
      "subDir2/subSubDir/inSubSubDir.class", "empty.txt", "empty.class", "subDir1/inSubDir1.class"))
    */
  }

  def testIsDiscoverableSuite() {
    assert(sdtf.isDiscoverableSuite(classOf[SuiteDiscoveryHelperSuite])) 
    @DoNotDiscover class NotDiscoverable {}
    assert(!sdtf.isDiscoverableSuite(classOf[NotDiscoverable]))
  }
  
  def testIsRunnable {
    class NormalClass {}
    class SuiteClass extends Suite
    @WrapWith(classOf[SuiteClass])
    class AnnotateDefaultConstructor
    class WrongSuiteClass(testValue: String) extends Suite
    @WrapWith(classOf[WrongSuiteClass])
    class AnnotateWrongConstructor
    assert(!sdtf.isRunnable(classOf[NormalClass]))
    assert(!sdtf.isRunnable(classOf[SuiteClass]))
    assert(!sdtf.isRunnable(classOf[AnnotateDefaultConstructor]))
    assert(!sdtf.isRunnable(classOf[AnnotateWrongConstructor]))
    assert(sdtf.isRunnable(classOf[SomeApiClass]))
    assert(sdtf.isRunnable(classOf[SomeApiSubClass]))
  }

  private def copyClassFilesIntoDirTree(classFiles: Set[String], dirTop: String)
  {
    val testsDir = "target/tests/"

    FileUtils.deleteDirectory(new File(dirTop))
  
    classFiles.foreach { classFile =>
      FileUtils.copyFile(new File(testsDir + classFile),
                         new File(dirTop + classFile))
    }
  }

  def testDiscoverSuiteNames() {
    val loader      = getClass.getClassLoader
    val sep         = System.getProperty("file.separator")
    val runpath1Dir = "target/runpath1/"
    val runpath2Dir = "target/runpath2/"
    val runpath1Jar = "target"+ sep +"runpath1.jar"

    //
    // No files in runpath.
    //
    FileUtils.deleteDirectory(new File(runpath1Dir))
    new File(runpath1Dir).mkdir

    val discoveredSet0 = discoverSuiteNames(List(runpath1Dir), loader, None)
    assert(discoveredSet0.size === 0)

    //
    // Discover one Suite. (FastAsLight isn't a Suite.)
    //
    val fileSet1 = Set("org/scalatest/EasySuite.class",
                       "org/scalatest/FastAsLight.class")

    val expectedSet1 = Set("org.scalatest.EasySuite")

    copyClassFilesIntoDirTree(fileSet1, runpath1Dir)

    val discoveredSet1 =
      discoverSuiteNames(List(runpath1Dir, "noSuchDir"), loader, None)

    assert(discoveredSet1 === expectedSet1)

    //
    // Discover multiple Suites in multiple directory trees.
    //
    val fileSet2 = Set("org/scalatest/tools/RunnerSpec.class",
                       "org/scalatest/SlowAsMolasses.class",
                       "org/scalatest/SuiteSuite.class");

    val expectedSet2 = Set("org.scalatest.EasySuite",
                           "org.scalatest.tools.RunnerSpec",
                           "org.scalatest.SuiteSuite")

    copyClassFilesIntoDirTree(fileSet2, runpath2Dir)

    val discoveredSet2 =
      discoverSuiteNames(List(runpath1Dir, runpath2Dir, "noSuch.jar"),
                         loader, None)

    assert(discoveredSet2 === expectedSet2)

    //
    // Discover Suites in a jar.
    //
    ("jar cf "+ runpath1Jar +" -C "+ runpath1Dir +" .").!

    val discoveredSet2j =
      discoverSuiteNames(List(runpath1Jar, runpath2Dir), loader, None)

    assert(discoveredSet2j === expectedSet2)

    //
    // Filter by suffix.
    //
    val expectedSet2s = Set("org.scalatest.tools.RunnerSpec")
    
    val discoveredSet2s =
      discoverSuiteNames(List(runpath1Dir, runpath2Dir), loader,
                         Some(Pattern.compile(".*(Spec)$")))

    assert(discoveredSet2s === expectedSet2s)

    //
    // Filter by suffixes.
    //
    val discoveredSet2ss =
      discoverSuiteNames(List(runpath1Dir, runpath2Dir), loader,
                         Some(Pattern.compile(".*(Spec|Suite)$")))

    assert(discoveredSet2ss === expectedSet2)
  }

  def testDiscoverJUnitClassNames() {
    val loader      = getClass.getClassLoader
    val sep         = System.getProperty("file.separator")
    val runpath1Dir = "target/jrunpath1/"
    val runpath2Dir = "target/jrunpath2/"
    val runpath1Jar = "target"+ sep +"jrunpath1.jar"

    //
    // Discover one class.
    //
    val fileSet1 = Set("org/scalatest/junit/JHappySuite.class",
                       "org/scalatest/FastAsLight.class")

    val expectedSet1 = Set("org.scalatest.junit.JHappySuite")

    copyClassFilesIntoDirTree(fileSet1, runpath1Dir)

    val discoveredSet1 =
      discoverJUnitClassNames(List(runpath1Dir, "noSuchDir"), loader).toSet

    assert(discoveredSet1 === expectedSet1)

    //
    // Discover multiple classes in multiple directory trees.
    //
    val fileSet2 = Set("org/scalatest/junit/JBitterSuite.class",
                       "org/scalatest/SlowAsMolasses.class",
                       "org/scalatest/SuiteSuite.class");

    val expectedSet2 = Set("org.scalatest.junit.JHappySuite",
                           "org.scalatest.junit.JBitterSuite")

    copyClassFilesIntoDirTree(fileSet2, runpath2Dir)

    val discoveredSet2 =
      discoverJUnitClassNames(
        List(runpath1Dir, runpath2Dir, "noSuch.jar"), loader).toSet

    assert(discoveredSet2 === expectedSet2)

    //
    // Discover classes in a jar.
    //
    ("jar cf "+ runpath1Jar +" -C "+ runpath1Dir +" .").!

    val discoveredSet2j =
      discoverJUnitClassNames(List(runpath1Jar, runpath2Dir), loader).toSet

    assert(discoveredSet2j === expectedSet2)
  }
}

//
// This class is just used by tests in SuiteDiscoveryHelperSuite
// for testing Suite discovery by test name.
//
class SuiteDiscoveryHelperSuite2 extends Suite {

  def testDiscoverTests4() {
  }

  def testDiscoverTestsA2() {
  }

  def testDiscoverTestsA3() {
  }
}
