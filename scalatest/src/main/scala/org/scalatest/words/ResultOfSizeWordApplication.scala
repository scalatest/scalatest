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

import org.scalatest.enablers.Size
import org.scalatest.matchers.HavePropertyMatcher
import org.scalatest.matchers.HavePropertyMatchResult
import org.scalactic.Prettifier

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class ResultOfSizeWordApplication(val expectedSize: Long) {

  // TODO: SCALADOC
  def apply[T : Size](resultOfOfTypeInvocation: ResultOfOfTypeInvocation[T]): HavePropertyMatcher[T, Long] = {
    new HavePropertyMatcher[T, Long] {
      def apply(t: T): HavePropertyMatchResult[Long] = {
        val sz = implicitly[Size[T]]
        val sizeOfT = sz.sizeOf(t)
        new HavePropertyMatchResult(
          sizeOfT == expectedSize,
          "size",
           expectedSize,
           sizeOfT
        )
      }
    }
  }
  
  /**
   * Overrides toString to return "size (X)", where X is expectedSize
   */
  override def toString: String = "size (" + Prettifier.default(expectedSize) + ")"
}
