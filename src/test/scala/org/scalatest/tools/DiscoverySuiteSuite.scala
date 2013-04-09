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
import org.scalatest.events._

/*
I can't get this to work and have no more time. Will make nestedSuiteNames package access.
object DiscoverySuiteCompanionFriend {

  val dsc = DiscoverySuite.getClass.getField("MODULE$").get(null)

  def nestedSuiteNames(path: String, accessibleSuites: List[String], wildcard: Boolean, runpathClassLoader: ClassLoader): List[String] = {
    val m = Class.forName("org.scalatest.DiscoverySuite").getDeclaredMethod("org$scalatest$DiscoverySuite$$nestedSuiteNames", Array(classOf[String], classOf[List[String]], classOf[Boolean]))
    m.setAccessible(true)
    m.invoke(dsc, Array[Object](path, accessibleSuites, new java.lang.Boolean(wildcard))).asInstanceOf[List[String]]
  }
}
*/

private[scalatest] class DiscoverySuiteSuite extends Suite {
  
  val loader = DiscoverySuite.getClass.getClassLoader

  def testConstructor() {
    intercept[NullPointerException] {
      new DiscoverySuite(null, Set(), false, loader)
    }
    intercept[NullPointerException] {
      new DiscoverySuite("hi", null, false, loader)
    }
    intercept[NullPointerException] {
      new DiscoverySuite(null, Set(), false, null)
    }
    assertResult(Nil) {
      DiscoverySuite.nestedSuiteNames("a.b.c", Set(), false)
    }
    assertResult(List("a.b.c.Hi")) {
      DiscoverySuite.nestedSuiteNames("a.b.c", Set("a.b.c.Hi"), true)
    }
    assertResult(List("a.b.c.d.Hi")) {
      DiscoverySuite.nestedSuiteNames("a.b.c", Set("a.b.c.d.Hi"), true)
    }
    assertResult(List("a.b.c.Hi")) {
      DiscoverySuite.nestedSuiteNames("a.b.c", Set("a.b.c.Hi"), false)
    }
    assertResult(Nil) {
      DiscoverySuite.nestedSuiteNames("a.b.c", Set("a.b.c"), false)
    }
    assertResult(Nil) {
      DiscoverySuite.nestedSuiteNames("a.b.c", Set("a.b.c.d.Hi"), false)
    }
  }
}
