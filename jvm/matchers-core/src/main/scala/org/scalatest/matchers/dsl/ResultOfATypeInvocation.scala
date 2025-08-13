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

import org.scalatest.matchers.MatchersHelper.checkExpectedException
import org.scalatest.Resources
import org.scalatest.matchers.MatchersHelper.indicateSuccess
import org.scalatest.matchers.MatchersHelper.indicateFailure
import org.scalatest.matchers.MatchersHelper.checkThrownBy
import org.scalatest.matchers.MatchersHelper.checkBeThrownBy
import org.scalactic._

import scala.reflect.ClassTag

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class ResultOfATypeInvocation[T](val clazzTag: ClassTag[T]) {

  val clazz: Class[T] = clazzTag.runtimeClass.asInstanceOf[Class[T]]

  def this(c: Class[_]) = this(ClassTag(c).asInstanceOf[ClassTag[T]])

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * a [RuntimeException] should be thrownBy { ... }
   *                      ^
   * </pre>
   **/
  //DOTTY-ONLY infix def should(beWord: BeWord)(implicit prettifier: Prettifier, pos: source.Position): ResultOfBeWordForAType[T] =
  // SKIP-DOTTY-START 
  def should(beWord: BeWord)(implicit prettifier: Prettifier, pos: source.Position): ResultOfBeWordForAType[T] =
  // SKIP-DOTTY-END
    new ResultOfBeWordForAType[T](clazz, prettifier, pos)  

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * a [RuntimeException] should not
   *                      ^
   * </pre>
   *
   * This method is here to direct people trying to use the above syntax to use <code>noException</code> instead.
   */
  //DOTTY-ONLY infix def should(notWord: NotWord): PleaseUseNoExceptionShouldSyntaxInstead =
  // SKIP-DOTTY-START 
  def should(notWord: NotWord): PleaseUseNoExceptionShouldSyntaxInstead =
  // SKIP-DOTTY-END
    new PleaseUseNoExceptionShouldSyntaxInstead
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * a [RuntimeException] shouldBe thrownBy { ... }
   *                      ^
   * </pre>
   **/
  // SKIP-DOTTY-START 
  def shouldBe(thrownBy: ResultOfThrownByApplication)(implicit prettifier: Prettifier, pos: source.Position): org.scalatest.Assertion = 
    checkThrownBy(clazz, thrownBy, pos)
  // SKIP-DOTTY-END
  //DOTTY-ONLY infix inline def shouldBe(thrownBy: ResultOfThrownByApplication)(implicit prettifier: Prettifier): org.scalatest.Assertion =   
  //DOTTY-ONLY   ${ org.scalatest.matchers.MatchersHelper.checkThrownByMacro('{clazz}, '{thrownBy}) }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * a [IllegalArgumentException] should (be thrownBy { ... })
   *                              ^
   * </pre>
   **/
  // SKIP-DOTTY-START 
  def should(beThrownBy: ResultOfBeThrownBy)(implicit prettifier: Prettifier, pos: source.Position): org.scalatest.Assertion = 
    checkBeThrownBy(clazz, beThrownBy, pos)
  // SKIP-DOTTY-END  
  //DOTTY-ONLY infix inline def should(beThrownBy: ResultOfBeThrownBy)(implicit prettifier: Prettifier): org.scalatest.Assertion = 
  //DOTTY-ONLY   ${ org.scalatest.matchers.MatchersHelper.checkBeThrownByMacro('{clazz}, '{beThrownBy}) }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * a [RuntimeException] must be thrownBy { ... }
   *                      ^
   * </pre>
   **/
  //DOTTY-ONLY infix def must(beWord: BeWord)(implicit prettifier: Prettifier, pos: source.Position): ResultOfBeWordForAType[T] =
  // SKIP-DOTTY-START 
  def must(beWord: BeWord)(implicit prettifier: Prettifier, pos: source.Position): ResultOfBeWordForAType[T] =
  // SKIP-DOTTY-END
    new ResultOfBeWordForAType[T](clazz, prettifier, pos)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * a [RuntimeException] must not
   *                      ^
   * </pre>
   *
   * This method is here to direct people trying to use the above syntax to use <code>noException</code> instead.
   */
  //DOTTY-ONLY infix def must(notWord: NotWord): PleaseUseNoExceptionShouldSyntaxInstead =
  // SKIP-DOTTY-START 
  def must(notWord: NotWord): PleaseUseNoExceptionShouldSyntaxInstead =
  // SKIP-DOTTY-END
    new PleaseUseNoExceptionShouldSyntaxInstead

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * a [RuntimeException] mustBe thrownBy { ... }
   *                      ^
   * </pre>
   **/
  // SKIP-DOTTY-START 
  def mustBe(thrownBy: ResultOfThrownByApplication)(implicit prettifier: Prettifier, pos: source.Position): org.scalatest.Assertion = 
    checkThrownBy(clazz, thrownBy, pos)
  // SKIP-DOTTY-END
  //DOTTY-ONLY infix inline def mustBe(thrownBy: ResultOfThrownByApplication)(implicit prettifier: Prettifier): org.scalatest.Assertion = 
  //DOTTY-ONLY   ${ org.scalatest.matchers.MatchersHelper.checkThrownByMacro('{clazz}, '{thrownBy}) }  

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * a [IllegalArgumentException] must (be thrownBy { ... })
   *                              ^
   * </pre>
   **/
  // SKIP-DOTTY-START 
  def must(beThrownBy: ResultOfBeThrownBy)(implicit prettifier: Prettifier, pos: source.Position): org.scalatest.Assertion = 
    checkBeThrownBy(clazz, beThrownBy, pos)
  // SKIP-DOTTY-END
  //DOTTY-ONLY infix inline def must(beThrownBy: ResultOfBeThrownBy)(implicit prettifier: Prettifier): org.scalatest.Assertion = 
  //DOTTY-ONLY   ${ org.scalatest.matchers.MatchersHelper.checkBeThrownByMacro('{clazz}, '{beThrownBy}) }
  
  override def toString: String = "a [" + clazz.getName + "]"
}
