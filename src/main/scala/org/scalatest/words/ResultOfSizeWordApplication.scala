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

import org.scalatest.matchers._
import org.scalatest.Matchers.newTestFailedException
import org.scalatest.Helper.accessProperty
import org.scalatest.Resources


/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class ResultOfSizeWordApplication(val expectedSize: Long) extends HavePropertyMatcher[AnyRef, Long] {

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * set should not have (size (3))
   *                     ^
   * </pre>
   *
   * <p>
   * This reason <code>ResultOfSizeWordApplication</code> is a <code>HavePropertyMatcher[AnyRef, Long]</code> is
   * so that you don't have to remember whether <code>size</code> needs to be surrounded by parentheses when following
   * <code>have</code>. Only <code>length</code> and <code>size</code> can be used without parentheses: everything else
   * needs the parentheses. So this approach means that if you use the unneeded parentheses with <code>length</code> and
   * <code>size</code>, it will still work. This <code>apply</code> method uses reflection to find and access the <code>size</code>
   * property on the passed <code>objectWithProperty</code>. Therefore if the object does not have the appropriate structure, the expression
   * will compile, but at will produce a <code>TestFailedException</code> at runtime.
   * </p>
   */
  def apply(objectWithProperty: AnyRef): HavePropertyMatchResult[Long] = {

    accessProperty(objectWithProperty, 'size, false) match {

      case None =>

        throw newTestFailedException(Resources("propertyNotFound", "size", expectedSize.toString, "getSize"))

      case Some(result) =>

        new HavePropertyMatchResult[Long](
          result == expectedSize,
          "size",
          expectedSize,
          result match {
            case value: Byte => value.toLong
            case value: Short => value.toLong
            case value: Int => value.toLong
            case value: Long => value
            case _ => throw newTestFailedException(Resources("sizePropertyNotAnInteger"))
          }
        )
    }
  }
}
