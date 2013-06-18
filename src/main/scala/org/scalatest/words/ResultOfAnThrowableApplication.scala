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
package org.scalatest.words

import org.scalatest.Resources
import org.scalatest.Assertions.newAssertionFailedException

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class ResultOfAnThrowableApplication[T <: Throwable] {
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * an [Exception] should be thrownBy { ... }
   *                ^
   * </pre>
   */
  def should(beWord: BeWord): ResultOfBeWordForAnThrowable[T] = 
    new ResultOfBeWordForAnThrowable[T](true)
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * an [RuntimeException] shouldBe thrownBy { ... }
   *                       ^
   * </pre>
   */
  def shouldBe(throwBy: ResultOfThrownByApplication)(implicit manifest: Manifest[T]) {
    
    val clazz = manifest.erasure.asInstanceOf[Class[T]]
    val caught = try {
      throwBy.apply()
      None
    }
    catch {
      case u: Throwable => {
        if (!clazz.isAssignableFrom(u.getClass)) {
          val s = Resources("wrongException", clazz.getName, u.getClass.getName)
          throw newAssertionFailedException(Some(s), Some(u), 4)
        }
        else {
          Some(u)
        }
      }
    }
    caught match {
      case None =>
        val message = Resources("exceptionExpected", clazz.getName)
        throw newAssertionFailedException(Some(message), None, 4)
      case Some(e) => e.asInstanceOf[T] // I know this cast will succeed, becuase iSAssignableFrom succeeded above
    }
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * an [RuntimeException] shouldNot be thrownBy { ... }
   *                       ^
   * </pre>
   */
  def shouldNot(beWord: BeWord): ResultOfBeWordForAnThrowable[T] = 
    new ResultOfBeWordForAnThrowable[T](false)
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * an [RuntimeException] should not be thrownBy { ... }
   *                       ^
   * </pre>
   */
  def should(notWord: NotWord): ResultOfNotWordForAnThrowable[T] = 
    new ResultOfNotWordForAnThrowable[T]
}