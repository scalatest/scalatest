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
import scala.util.Failure
import annotation.tailrec

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
    def validating(hd: G => Validation[ErrorMessage], tl: (G => Validation[ErrorMessage])*): Try[G] = {
      theTry.flatMap { (g: G) =>
        TrySugar.passOrFirstFail(g, hd :: tl.toList) match {
          case Pass => theTry
          case Fail(errMsg) => Failure(ValidationFailedException(errMsg))
        }
      }
    }
  }

  /**
   * Implicit class that adds a <code>toOr</code> method to
   * <code>Try</code>, which converts <code>Success</code> to <code>Good</code>,
   * and <code>Failure</code> to <code>Bad</code>.
   */
  implicit class NothingSuccessTryizer(theTry: Try[Nothing]) {// TODO: Challenge this one. vaidating works on Try[Nothing] already. Does toOr?
    def toOr: Nothing Or Throwable = Or.from(theTry)
  } 
} 

/**
 * Companion object for <code>TrySugar</code> enabling its members to be
 * imported as an alternative to mixing them in.
 */
object TrySugar extends TrySugar {
  @tailrec
  private def passOrFirstFail[G](g: G, fs: List[G => Validation[ErrorMessage]]): Validation[ErrorMessage] = {
    fs match {
      case Nil => Pass
      case head :: tail => 
        head(g) match {
          case Pass => passOrFirstFail(g, tail)
          case firstFail => firstFail
        }
    }
  }
}
