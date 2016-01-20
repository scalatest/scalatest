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
package org.scalatest.fixture

import scala.collection.immutable.ListSet
import org.scalatest.Suite.autoTagClassAnnotations
import org.scalatest._
import Spec._
import Suite._
import org.scalatest.events.{TopOfClass, TopOfMethod}
import scala.reflect.NameTransformer._
import java.lang.reflect.{Method, Modifier, InvocationTargetException}

/**
 * <strong>Class <code>fixture.Spec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
 * <code>org.scalatest.fixture.FunSpec</code> instead.</strong>
 *
 * <p>
 * Because this style uses reflection at runtime to discover scopes and tests, it can only be supported on the JVM, not Scala.js.
 * Thus in ScalaTest 3.0.0, class <code>org.scalatest.Spec</code> was moved to the <code>org.scalatest.refspec</code> package and renamed
 * <code>RefSpec</code>, with the intention of later moving it to a separate module available only on the JVM. If the 
 * <code>org.scalatest.refspec._</code> package contained a <code>fixture</code> subpackage, then importing <code>org.scalatest.refspec._</code>
 * would import the name <code>fixture</code> as <code>org.scalatest.refspec.fixture</code>. This would likely be confusing for users,
 * who expect <code>fixture</code> to mean <code>org.scalatest.fixture</code>.
 * </p>
 *
 * <p>
 * As a result this class has been deprecated and will <em>not</em>
 * be moved to package <code>org.scalatest.refspec</code>. Instead we recommend you rewrite any test classes that currently extend
 * <code>org.scalatest.fixture.Spec</code> to extend <a href="FunSpec.html"><code>org.scalatest.fixture.FunSpec</code></a> instead,
 * replacing any scope <code>object</code>
 * with a <code>describe</code> clause, and any test method with an <code>it</code> clause.
 * </p>
 *
 * @author Bill Venners
 */
@Finders(Array("org.scalatest.finders.SpecFinder"))
@deprecated("fixture.Spec has been deprecated and will be removed in a future version of ScalaTest. Please use org.scalatest.fixture.FunSpec instead.")
abstract class Spec extends SpecLike {

  /**
   * Returns a user friendly string for this suite, composed of the
   * simple name of the class (possibly simplified further by removing dollar signs if added by the Scala interpeter) and, if this suite
   * contains nested suites, the result of invoking <code>toString</code> on each
   * of the nested suites, separated by commas and surrounded by parentheses.
   *
   * @return a user-friendly string for this suite
   */
  override def toString: String = Suite.suiteToString(None, this)
}

private[scalatest] object Spec {
  
  def isTestMethod(m: Method): Boolean = {

    val isInstanceMethod = !Modifier.isStatic(m.getModifiers())

    val paramTypes = m.getParameterTypes
    val hasNoParamOrFixtureParam = paramTypes.isEmpty || paramTypes.length == 1

    // name must have at least one encoded space: "$u0220"
    val includesEncodedSpace = m.getName.indexOf("$u0020") >= 0
    
    val isOuterMethod = m.getName.endsWith("$$outer")
    
    val isNestedMethod = m.getName.matches(".+\\$\\$.+\\$[1-9]+")

    // def maybe(b: Boolean) = if (b) "" else "!"
    // println("m.getName: " + m.getName + ": " + maybe(isInstanceMethod) + "isInstanceMethod, " + maybe(hasNoParams) + "hasNoParams, " + maybe(includesEncodedSpace) + "includesEncodedSpace")
    isInstanceMethod && hasNoParamOrFixtureParam && includesEncodedSpace && !isOuterMethod && !isNestedMethod
  }
  
  import java.security.MessageDigest
  import scala.io.Codec
  
  // The following compactify code is written based on scala compiler source code at:-
  // https://github.com/scala/scala/blob/master/src/reflect/scala/reflect/internal/StdNames.scala#L47
  
  private val compactifiedMarker = "$$$$"
  
  def equalIfRequiredCompactify(value: String, compactified: String): Boolean = {
    if (compactified.matches(".+\\$\\$\\$\\$.+\\$\\$\\$\\$.+")) {
      val firstDolarIdx = compactified.indexOf("$$$$")
      val lastDolarIdx = compactified.lastIndexOf("$$$$")
      val prefix = compactified.substring(0, firstDolarIdx)
      val suffix = compactified.substring(lastDolarIdx + 4)
      val lastIndexOfDot = value.lastIndexOf(".")
      val toHash = 
        if (lastIndexOfDot >= 0) 
          value.substring(0, value.length - 1).substring(value.lastIndexOf(".") + 1)
        else
          value
          
      val bytes = Codec.toUTF8(toHash.toArray)
      val md5 = MessageDigest.getInstance("MD5")
      md5.update(bytes)
      val md5chars = (md5.digest() map (b => (b & 0xFF).toHexString)).mkString
      (prefix + compactifiedMarker + md5chars + compactifiedMarker + suffix) == compactified
    }
    else
      value == compactified
  }
}
