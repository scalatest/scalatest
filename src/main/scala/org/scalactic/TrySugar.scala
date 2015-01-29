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

import scala.util.Try
/**
 * Trait providing an implicit class that adds a <code>toOr</code> method to
 * <code>Try</code>, which converts <code>Success</code> to <code>Good</code>,
 * and <code>Failure</code> to <code>Bad</code>.
 */
trait TrySugar {

  /**
   * Implicit class that adds a <code>toOr</code> method to
   * <code>Try</code>, which converts <code>Success</code> to <code>Good</code>,
   * and <code>Failure</code> to <code>Bad</code>.
   */
  implicit class Tryizer[G](theTry: Try[G]) {
    def toOr: G Or Throwable = Or.from(theTry)
  }

  /**
   * Implicit class that adds a <code>toOr</code> method to
   * <code>Try</code>, which converts <code>Success</code> to <code>Good</code>,
   * and <code>Failure</code> to <code>Bad</code>.
   */
  implicit class NothingSuccessTryizer(theTry: Try[Nothing]) {
    def toOr: Nothing Or Throwable = Or.from(theTry)
  } 
} 

/**
 * Companion object for <code>TrySugar</code> enabling its members to be
 * imported as an alternative to mixing them in.
 */
object TrySugar extends TrySugar

