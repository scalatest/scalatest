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
package org.scalactic

/**
 * Trait providing an implicit class that adds a <code>toOr</code> method to
 * <code>Either</code>, which converts <code>Left</code> to <code>Bad</code>,
 * and <code>Right</code> to <code>Good</code>.
 */
trait EitherSugar {

  /**
   * Implicit class that adds a <code>toOr</code> method to
   * <code>Either</code>, which converts <code>Left</code> to <code>Bad</code>,
   * and <code>Right</code> to <code>Good</code>.
   */
  implicit class Eitherizer[L, R](either: Either[L, R]) {
    def toOr: R Or L = Or.from(either)
  }
  /**
   * Implicit class that adds a <code>toOr</code> method to
   * <code>Either</code>, which converts <code>Left</code> to <code>Bad</code>,
   * and <code>Right</code> to <code>Good</code>.
   */
  implicit class NothingLeftEitherizer[R](either: Either[Nothing, R]) {
    def toOr: R Or Nothing = Or.from(either)
  }
  /**
   * Implicit class that adds a <code>toOr</code> method to
   * <code>Either</code>, which converts <code>Success</code> to <code>Good</code>,
   * and <code>Failure</code> to <code>Bad</code>.
   */
  implicit class NothingRightEitherizer[L](either: Either[L, Nothing]) {
    def toOr: Nothing Or L = Or.from(either)
  }
} 

/**
 * Companion object for <code>EitherSugar</code> enabling its members to be
 * imported as an alternative to mixing them in.
 */
object EitherSugar extends EitherSugar

