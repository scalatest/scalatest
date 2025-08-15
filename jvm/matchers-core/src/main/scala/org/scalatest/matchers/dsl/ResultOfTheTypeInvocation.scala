/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalatest.matchers.dsl

import org.scalactic._
import org.scalatest.Resources
import org.scalatest.matchers.MatchersHelper.checkExpectedException

import scala.reflect.ClassTag

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class ResultOfTheTypeInvocation[T](clazzTag: ClassTag[T], pos: source.Position) {

  val clazz: Class[T] = clazzTag.runtimeClass.asInstanceOf[Class[T]]

  def this(c: Class[_], pos: source.Position) = this(ClassTag(c).asInstanceOf[ClassTag[T]], pos)

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * val e = the [FileNotFoundException] thrownBy { ... }
   *                                     ^
   * </pre>
   **/
  //DOTTY-ONLY infix def thrownBy(fun: => Any): T = {
  // SKIP-DOTTY-START 
  def thrownBy(fun: => Any): T = {
  // SKIP-DOTTY-END  
    checkExpectedException(fun, clazz, Resources.wrongException _, Resources.exceptionExpected _, pos)
  }
  
  override def toString: String = "the [" + clazz.getName + "]"
}
