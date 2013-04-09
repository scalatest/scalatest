/*
 * Copyright 2001-2008 Artima, Inc.
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
import java.io.File
import java.util.regex.Pattern

class SuiteDiscoveryHelperFriend(sdt: SuiteDiscoveryHelper.type) {

  def transformToClassName(fileName: String, fileSeparator: Char): Option[String] = {
    val m = Class.forName("org.scalatest.tools.SuiteDiscoveryHelper$").getDeclaredMethod("org$scalatest$tools$SuiteDiscoveryHelper$$transformToClassName",
      Array(classOf[String], classOf[Char]): _*)
    m.setAccessible(true)
    m.invoke(sdt, Array[Object](fileName, new java.lang.Character(fileSeparator)): _*).asInstanceOf[Option[String]]
  }

  def extractClassNames(fileNames: Iterator[String], fileSeparator: Char): Iterator[String] = {
    val m = Class.forName("org.scalatest.tools.SuiteDiscoveryHelper$").getDeclaredMethod("extractClassNames",
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

  def processFileNames(fileNames: Iterator[String], fileSeparator: Char, loader: ClassLoader, suffixes: Option[Pattern]):
  Set[String] =
  {
    val m = Class.forName("org.scalatest.tools.SuiteDiscoveryHelper$").getDeclaredMethod("org$scalatest$tools$SuiteDiscoveryHelper$$processFileNames",
      Array(classOf[Iterator[String]], classOf[Char], classOf[ClassLoader], classOf[Option[Pattern]]): _*)
    m.setAccessible(true)
    m.invoke(sdt, Array[Object](fileNames, new java.lang.Character(fileSeparator), loader, suffixes): _*).asInstanceOf[Set[String]]
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

  def testProcessFileNames() {

    val loader = getClass.getClassLoader
    val discoveredSet1 = sdtf.processFileNames(List("doesNotExist.txt", "noSuchfile.class").iterator, '/', loader, None)
    assert(discoveredSet1.isEmpty)

    val discoveredSet2 = sdtf.processFileNames(List("org/scalatest/EasySuite.class", "noSuchfile.class", "org/scalatest/FastAsLight.class").iterator, '/', loader, None)
    assert(discoveredSet2 === Set("org.scalatest.EasySuite"))

    val fileNames3 =
      List(
        "org/scalatest/EasySuite.class",
        "org/scalatest/RunnerSuite.class",
        "org/scalatest/SlowAsMolasses.class",
        "org/scalatest/SuiteSuite.class",
        "noSuchfile.class",
        "org/scalatest/FastAsLight.class"
      )
    val classNames3 =
      Set(
        "org.scalatest.EasySuite",
        // "org.scalatest.RunnerSuite", dropped this when moved RunnerSuite to tools
        "org.scalatest.SuiteSuite"
      )
    val discoveredSet3 = sdtf.processFileNames(fileNames3.iterator, '/', loader, None)
    assert(discoveredSet3 === classNames3)

    // Test with backslashes
    val fileNames4 =
      List(
        "org\\scalatest\\EasySuite.class",
        "org\\scalatest\\RunnerSuite.class",
        "org\\scalatest\\SlowAsMolasses.class",
        "org\\scalatest\\SuiteSuite.class",
        "noSuchfile.class",
        "org\\scalatest\\FastAsLight.class"
      )
    val discoveredSet4 = sdtf.processFileNames(fileNames4.iterator, '\\', loader, None)
    assert(discoveredSet4 === classNames3)

    // Test with leading slashes
    val fileNames5 =
      List(
        "/org/scalatest/EasySuite.class",
        "/org/scalatest/RunnerSuite.class",
        "/org/scalatest/SlowAsMolasses.class",
        "/org/scalatest/SuiteSuite.class",
        "/noSuchfile.class",
        "/org/scalatest/FastAsLight.class"
      )
    val discoveredSet5 = sdtf.processFileNames(fileNames5.iterator, '/', loader, None)
    assert(discoveredSet5 === classNames3)

    // Test for specified suffixes only
    val fileNames6 =
      List(
        "/org/scalatest/EasySuite.class",
        "/org/scalatest/RunnerSuite.class",
        "/org/scalatest/SlowAsMolasses.class",
        "/org/scalatest/SuiteSuite.class",
        "/org/scalatest/FilterSpec.class",
        "/noSuchfile.class",
        "/org/scalatest/FastAsLight.class"
      )

    val classNames4 =
      Set(
        "org.scalatest.EasySuite",
        "org.scalatest.SuiteSuite",
        "org.scalatest.FilterSpec"
      )

    val discoveredSet6 = sdtf.processFileNames(fileNames6.iterator, '/', loader, Some(Pattern.compile(".*(Suite)$")))
    assert(discoveredSet6 === classNames3)

    val discoveredSet7 = sdtf.processFileNames(fileNames6.iterator, '/', loader, Some(Pattern.compile(".*(Spec|Suite)$")))
    assert(discoveredSet7 === classNames4)
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
}
