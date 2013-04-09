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
import java.util.UUID

/**
 * A Suite that contains as nested suites accessible suites on the runpath whose fully qualified
 * names start with the specified path. If wildcard is true, then any accessible suite whose fully
 * qualified name starts with the path will be included in nestedSuites. Else, only accessible suites that
 * are direct members of the path are included in nesteSuites.
 *
 * @author Bill Venners
 */
private[scalatest] class DiscoverySuite(path: String, accessibleSuites: Set[String], wildcard: Boolean, runpathClassLoader: ClassLoader)  extends Suite {

  if (path == null || accessibleSuites == null || runpathClassLoader == null)
    throw new NullPointerException

  override val suiteId = getClass.getName + "-" + UUID.randomUUID.toString
    
  override val nestedSuites: collection.immutable.IndexedSeq[Suite] =
    for (suiteClassName <- DiscoverySuite.nestedSuiteNames(path, accessibleSuites, wildcard))
      yield {
        try {
          val clazz = runpathClassLoader.loadClass(suiteClassName)
          val wrapWithAnnotation = clazz.getAnnotation(classOf[WrapWith])
          if (wrapWithAnnotation == null)
            clazz.newInstance.asInstanceOf[Suite]
          else {
            val suiteClazz = wrapWithAnnotation.value
            val constructorList = suiteClazz.getDeclaredConstructors()
            val constructor = constructorList.find { c => 
              val types = c.getParameterTypes
              types.length == 1 && types(0) == classOf[java.lang.Class[_]]
            }
            constructor.get.newInstance(clazz).asInstanceOf[Suite]
          }
        }
        catch {
          case e: Exception => {
            val msg = Resources("cannotLoadDiscoveredSuite", suiteClassName)
            throw new RuntimeException(msg, e)
          }
        }
      }
     // TODO: probably override run to just call runNestedSuites
  override protected def runTests(testName: Option[String], args: Args): Status = {
    if (testName == null)
      throw new NullPointerException("testName was null")
    if (args == null)
      throw new NullPointerException("args was null")
    SucceededStatus
  }
}

private[scalatest] object DiscoverySuite {

  private def wildcardList(path: String, accessibleSuites: Set[String]): Set[String] = accessibleSuites.filter(_.startsWith(path))

  private def narrowList(path: String, accessibleSuites: Set[String]): Set[String] = // filter out all but ones that are direct members of the path
    for (name <- wildcardList(path, accessibleSuites); if name.length > path.length && !name.substring(path.length + 1).contains('.'))
      yield name

  private[scalatest] def nestedSuiteNames(path: String, accessibleSuites: Set[String], wildcard: Boolean): collection.immutable.IndexedSeq[String] =
    if (wildcard)
      Vector.empty ++ wildcardList(path, accessibleSuites)
    else
      Vector.empty ++ narrowList(path, accessibleSuites)
}


