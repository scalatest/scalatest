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

// TODO: Make this an object. To do so need to figure out how
// to invoke private method with reflection on an object, because
// that's how I'm testing the private methods here.
/**
 * Discovers Suites on the runpath.
 *
 * @author Bill Venners
 */
private[scalatest] object SuiteDiscoveryHelper {

  def discoverSuiteNames(runpath: List[String], loader: ClassLoader,
                         suffixes: Option[Pattern]): Set[String] =
  {
    val fileSeparatorString = System.getProperty("path.separator")
    val fileSeparator = if (!fileSeparatorString.isEmpty) fileSeparatorString(0) else ':'

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

    val listOfSets: List[Set[String]] = 
      for (path <- runpath)
        yield {
          val urlOption =
            try {
              Some(new URL(path))
            }
            catch {
              case e: MalformedURLException => None
            }
    
          val endsWithDotJar = path.endsWith(".jar")
    
          if (endsWithDotJar) {
            val jarFileOption =
              urlOption match {
                case Some(url) => getJarFileFromURL(url)
                case None => getJarFileFromFileSystem(path)
              }
    
            jarFileOption match {
              case Some(jf) => processFileNames(getFileNamesIteratorFromJar(jf), '/', loader, suffixes)
              case None => Set[String]()
            }
          }
          else {
            processFileNames(getFileNamesSetFromFile(new File(path), fileSeparator).iterator, fileSeparator, loader, suffixes)
          }
        }

    Set() ++ listOfSets.flatMap(_.iterator.toList)
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
  // Determines whether specified class is to be included in
  // test run.
  //
  // Returns Some(<class name>) if processed, else None
  private def processClassName(className: String, loader: ClassLoader, suffixes: Option[Pattern]): Option[String] = {

    if (classNameSuffixOkay(className, suffixes) && isDiscoverableSuite(className, loader)
        && 
        (isAccessibleSuite(className, loader) || isRunnable(className, loader))) 
      Some(className)
    else 
      None 
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

  //
  // Scans specified files and returns names of classes to
  // be included in test run.
  //
  // Extracts class names from the file names of .class files
  // specified by the passed-in iterator, and returns those
  // classes found that are to be included in run.
  //
  private def processFileNames(fileNames: Iterator[String], fileSeparator: Char, loader: ClassLoader,
                               suffixes: Option[Pattern]): Set[String] =
  {
    val classNameOptions = // elements are Some(<class name>) if processed, else None
      for (className <- extractClassNames(fileNames, fileSeparator))
        yield processClassName(className, loader, suffixes)

    val classNames = 
      for (Some(className) <- classNameOptions)
        yield className

    Set[String]() ++ classNames
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
}
