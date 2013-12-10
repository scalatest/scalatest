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
import java.lang.reflect.Modifier
import java.util.Enumeration
import java.util.jar.JarFile
import java.util.jar.JarEntry
import scala.collection.mutable
import java.io.File
import java.net.URL
import java.net.MalformedURLException
import java.io.IOException
import java.util.regex.Pattern

import scala.collection.mutable.ListBuffer

/**
 * Discovers Suites on the runpath.
 *
 * @author Bill Venners
 */
private[scalatest] object SuiteDiscoveryHelper {

  //
  // Finds Suites containing specified tests.
  //
  // Tests are specified either by name or substring.  This method
  // searches all the accessibleSuites for matching tests, and
  // returns a SuiteParam for each matching Suite found.
  //
  def discoverTests(testSpecs:        List[TestSpec],
                    accessibleSuites: Set[String],
                    loader:           ClassLoader): List[SuiteParam] =
  {
    val buf = new ListBuffer[SuiteParam]

    if (!testSpecs.isEmpty) {
      val names: Set[String] =
        testSpecs.filter(_.isSubstring == false).map(_.spec).toSet
  
      val substrings: Set[String] =
        testSpecs.filter(_.isSubstring == true).map(_.spec).toSet
  
      for (suiteName <- accessibleSuites) {
        val suiteInstance: Suite =
          DiscoverySuite.getSuiteInstance(suiteName, loader)
  
        val nameMatches: Set[String] =
          names.intersect(suiteInstance.testNames)
  
        val substringMatches: Set[String] =
          substrings.filter(substring =>
            suiteInstance.testNames.exists(_.contains(substring)))
  
        if ((nameMatches.size > 0) || (substringMatches.size > 0))
          buf += SuiteParam(suiteName,
                            nameMatches.toList.sortWith(_<_).toArray,
                            substringMatches.toList.sortWith(_<_).toArray,
                            Array.empty)
      }
    }

    buf.toList.sortWith(_.className<_.className)
  }

  //
  // Given a file name composed using specified separator, converts name to
  // corresponding class name.  E.g., for separator '/':
  //
  //    org/scalatest/fixture/FixtureFunSuiteSpec.class
  //
  // -> org.scalatest.fixture.FixtureFunSuiteSpec
  //
  // Returns None if file name doesn't end in '.class'.
  //
  // (Typically we compose file names using ':' instead of '/', but
  // that's probably just a mistake where path.separator got used instead
  // of file.separator and doesn't affect how things turn out.)
  // 
  private def transformToClassName(fileName: String, fileSeparator: Char): Option[String] = {

    // If the fileName starts with a file separator char, lop that off
    val fn =
      if (!fileName.isEmpty && fileName(0) == fileSeparator)
        fileName.substring(1)
      else
        fileName

    if (fn.endsWith(".class") && fn != ".class")
      Some(fn.substring(0, fn.length - 6).replace(fileSeparator, '.'))
    else
      None
  }

  private val emptyClassArray = new Array[java.lang.Class[T] forSome { type T }](0)

  private[scalatest] def isAccessibleSuite(clazz: java.lang.Class[_]): Boolean = {
      try {
        classOf[Suite].isAssignableFrom(clazz) && 
          Modifier.isPublic(clazz.getModifiers) &&
          !Modifier.isAbstract(clazz.getModifiers) &&
          Modifier.isPublic(clazz.getConstructor(emptyClassArray: _*).getModifiers)
      }
      catch {
        case nsme: NoSuchMethodException => false
        case se: SecurityException => false
      }
  }

  private[scalatest] def isAccessibleSuite(className: String, loader: ClassLoader): Boolean = {
    try {
      isAccessibleSuite(loader.loadClass(className)) 
    }
    catch {
      case e: ClassNotFoundException => false
      case e: NoClassDefFoundError => false
    }
  }
  
  private[scalatest] def isDiscoverableSuite(clazz: java.lang.Class[_]): Boolean = {
    !clazz.isAnnotationPresent(classOf[DoNotDiscover])
  }
  
  private def isDiscoverableSuite(className: String, loader: ClassLoader): Boolean = {
    try {
      isDiscoverableSuite(loader.loadClass(className))
    }
    catch {
      case e: ClassNotFoundException => false
      case e: NoClassDefFoundError => false
    }
  }
  
  private[scalatest] def isRunnable(clazz: java.lang.Class[_]): Boolean = {
    val wrapWithAnnotation = clazz.getAnnotation(classOf[WrapWith])
    if (wrapWithAnnotation != null) {
      val wrapperSuiteClazz = wrapWithAnnotation.value
      val constructorList = wrapperSuiteClazz.getDeclaredConstructors()
      constructorList.exists { c => 
        val types = c.getParameterTypes
        types.length == 1 && types(0) == classOf[java.lang.Class[_]]
      }
    }
    else
      false
  }
  
  private[scalatest] def isRunnable(className: String, loader: ClassLoader): Boolean = {
    try {
      isRunnable(loader.loadClass(className)) 
    }
    catch {
      case e: ClassNotFoundException => false
      case e: NoClassDefFoundError => false
    }
  }

  //
  // Determines whether class should be included in test based
  // on whether its class name matches one of the suffixes
  // specified by user.
  //
  // Users may specify that only classes whose names end with
  // specified suffixes be included in test.
  //
  private def classNameSuffixOkay(className: String,
                                  suffixes: Option[Pattern]): Boolean =
  {
    (suffixes == None) ||
    suffixes.get.matcher(className).matches
  }

  private def getFileNamesSetFromFile(file: File, fileSeparator: Char): Set[String] = {

    def prependPrevName(prevName: String, fileName: String) = prevName + fileSeparator + fileName

    def listFilesInDir(dir: File, prevName: String): List[String] = {

      if (!dir.isDirectory)
        throw new IllegalArgumentException

      val subDirs = for (entry <- dir.listFiles.toList; if entry.isDirectory) yield entry
      val fileLists: List[List[String]] = 
        for (subDir <- subDirs) 
          yield listFilesInDir(subDir, prependPrevName(prevName, subDir.getName))

      val files: List[String] =
        for (entry <- dir.listFiles.toList; if !entry.isDirectory)
          yield prependPrevName(prevName, entry.getName)

      files ::: fileLists.flatMap(e => e)
    }

    val allFiles = if (file.isDirectory)
      listFilesInDir(file, "")
    else
      List(file.getName)

    Set() ++ allFiles.map(fn => if (!fn.isEmpty && fn(0) == fileSeparator) fn.substring(1) else fn)
  }

  private def getFileNamesIteratorFromJar(file: JarFile): Iterator[String] = {

    class EnumerationWrapper[T](e: Enumeration[T]) extends Iterator[T] {
      def next: T = e.nextElement
      def hasNext: Boolean = e.hasMoreElements
    }

    new EnumerationWrapper[JarEntry](file.entries).map(_.getName)
  }

  //
  // Given a fileNames iterator, returns an iterator of class names
  // corresponding to .class files found.
  //
  private def extractClassNames(fileNames: Iterator[String], fileSeparator: Char): Iterator[String] = {
    val options =
      for (fileName <- fileNames) yield
        transformToClassName(fileName, fileSeparator)

    for (Some(className) <- options) yield
      className
  }

  def discoverSuiteNames(runpath: List[String], loader: ClassLoader,
                         suffixes: Option[Pattern]):
  Set[String] =
  {
    val classNames = getRunpathClassNames(runpath)

    classNames.filter(isIncludeableSuite(_, loader, suffixes)).toSet
  }

  //
  // Finds the names of all classes in the runpath.
  //
  private def getRunpathClassNames(runpath: List[String]): List[String] =
  {
    val (jarIterators, fileIterators) = getFileNameIterators(runpath)

    val classNamesFromJars =
      jarIterators.map(
        (f: Iterator[String]) => extractClassNames(f, '/')).flatten

    val classNamesFromFileSystem =
      fileIterators.map(extractClassNames(_, getFileSeparator)).flatten

    classNamesFromJars ::: classNamesFromFileSystem
  }

  private def isIncludeableSuite(className: String, loader: ClassLoader,
                                 suffixes: Option[Pattern]): Boolean =
  {
    classNameSuffixOkay(className, suffixes) &&
    isDiscoverableSuite(className, loader) &&
    (isAccessibleSuite(className, loader) || isRunnable(className, loader))
  }

  //
  // Returns two lists of iterators for extracting file names
  // from runpath elements:
  //  1) iterators for files within jars
  //  2) iterators for files within the file system
  //
  private def getFileNameIterators(runpath: List[String]):
  (List[Iterator[String]], List[Iterator[String]]) =
  {
    val (runpathJars, runpathFiles) = runpath.partition(_.endsWith(".jar"))

    (getJarIterators(runpathJars), getFileIterators(runpathFiles))
  }

  //
  // Returns a list of iterators for retrieving file names from
  // directory elements in the runpath, one iterator for each
  // element.
  //
  private def getFileIterators(runpathFiles: List[String]):
  List[Iterator[String]] =
    for (path <- runpathFiles) yield
      getFileNamesSetFromFile(new File(path), getFileSeparator).iterator

  //
  // Returns a list of iterators for retrieving file names from
  // jar elements in the runpath, one iterator for each jar.
  //
  private def getJarIterators(runpathJars: List[String]):
  List[Iterator[String]] =
    for (path <- runpathJars)
      yield {
        getJarFile(path) match {
          case Some(jf) => getFileNamesIteratorFromJar(jf)
          case None => Set[String]().iterator
        }
      }

  private def getJarFile(path: String): Option[JarFile] = {
    def getJarFileFromURL(url: URL): Option[JarFile] = {
      val o = url.openConnection().getContent()
      if (o != null) {
        try {
          Some(o.asInstanceOf[JarFile])
        }
        catch {
          case e: ClassCastException => None
        }
      }
      else {
        None
      }
    }

    def getJarFileFromFileSystem(path: String): Option[JarFile] = {
      try {
        Some(new JarFile(path))
      }
      catch {
        case e: IOException => None
      }
    }

    try {
      getJarFileFromURL(new URL(path))
    }
    catch {
      case e: MalformedURLException => getJarFileFromFileSystem(path)
    }
  }

  //
  // Returns the names of all classes in the runpath that
  // contain JUnit tests (as marked by JUnit @Test annotation).
  //
  def discoverJUnitClassNames(runpath: List[String],
                              loader: ClassLoader): List[String] =
  {
    val classNames = getRunpathClassNames(runpath)

    classNames.filter(hasJUnitTests(_, loader))
  }

  //
  // Returns a list of iterators for retrieving file names from
  // jar elements in the runpath.
  //
  private def hasJUnitTests(className: String, loader: ClassLoader): Boolean = {
    try {
      val clazz = loader.loadClass(className)
      clazz.getMethods.exists(_.isAnnotationPresent(classOf[org.junit.Test]))
    }
    catch {
      case e: ClassNotFoundException => false
      case e: NoClassDefFoundError => false
    }
  }

  private def getFileSeparator: Char = {
    val fileSeparatorString = System.getProperty("path.separator")

    if (!fileSeparatorString.isEmpty) fileSeparatorString(0)
    else ':'
  }
}
