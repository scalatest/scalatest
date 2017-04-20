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
import java.util.UUID
import org.scalactic.Requirements._

/**
 * A Suite that contains as nested suites accessible suites on the runpath whose fully qualified
 * names start with the specified path. If wildcard is true, then any accessible suite whose fully
 * qualified name starts with the path will be included in nestedSuites. Else, only accessible suites that
 * are direct members of the path are included in nesteSuites.
 *
 * @author Bill Venners
 */
private[scalatest] class DiscoverySuite(path: String, accessibleSuites: Set[String], wildcard: Boolean, runpathClassLoader: ClassLoader)  extends Suite {

  requireNonNull(path, accessibleSuites, runpathClassLoader)

  override val suiteId = getClass.getName + "-" + UUID.randomUUID.toString
    
  override val nestedSuites: collection.immutable.IndexedSeq[Suite] =
    for (suiteClassName <- DiscoverySuite.nestedSuiteNames(path, accessibleSuites, wildcard))
      yield DiscoverySuite.getSuiteInstance(suiteClassName, runpathClassLoader)
     // TODO: probably override run to just call runNestedSuites
  override protected def runTests(testName: Option[String], args: Args): Status = {
    requireNonNull(testName, args)
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

  private[scalatest] def getSuiteInstance(suiteClassName: String, loader: ClassLoader): Suite = {
    try {
      val clazz = loader.loadClass(suiteClassName)
      AnnotationHelper.findWrapWith(clazz).map(wrapWithAnnotation => {
        val suiteClazz = wrapWithAnnotation.value
        val constructorList = suiteClazz.getDeclaredConstructors()
        val constructor = constructorList.find { c => 
          val types = c.getParameterTypes
          types.length == 1 && types(0) == classOf[java.lang.Class[_]]
        }
        constructor.get.newInstance(clazz).asInstanceOf[Suite]
      }).getOrElse(clazz.newInstance.asInstanceOf[Suite])
    }
    catch {
      case t: Throwable => { // TODO: Maybe include the e.getClass.getName and the message for e in the message cannotLoadDiscoveredSuite, because Jess had the problem
                             // That gradle cut off the stack trace so she couldn't see the cause.
        val msg = Resources.cannotLoadDiscoveredSuite(suiteClassName)
        new DeferredAbortedSuite(suiteClassName, new RuntimeException(msg, t))
      }
    }
  }
}


