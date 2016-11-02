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
package org.scalatest.prop

import org.scalatest.Assertion

trait PropertyTestResultChecker[T] {

  def succeed(v: T): Boolean

}

object PropertyTestResultChecker {

  implicit def checkerForAny[T]: PropertyTestResultChecker[T] =
    new PropertyTestResultChecker[T] {
      def succeed(v: T): Boolean = true
    }

  implicit def checkerForBoolean[BOOLEAN <: Boolean]: PropertyTestResultChecker[BOOLEAN] =
    new PropertyTestResultChecker[BOOLEAN] {
      def succeed(v: BOOLEAN): Boolean = v
    }

  val default = checkerForAny

}