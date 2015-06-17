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
import exceptions.ValidationFailedException

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
  implicit class Tryizer[T](theTry: Try[T]) {
    def toOr: T Or Throwable = Or.from(theTry)
    def validating(hd: T => Validation[ErrorMessage], tl: (T => Validation[ErrorMessage])*): Try[T] = {
      theTry.flatMap { (o: T) =>
        TrySugar.passOrFirstFail(o, hd :: tl.toList) match {
          case Pass => theTry
          case Fail(errorMessage) => Failure(ValidationFailedException(errorMessage))
        }
      }
    }
  }
} 

/**
 * Companion object for <code>TrySugar</code> enabling its members to be
 * imported as an alternative to mixing them in.
 */
object TrySugar extends TrySugar {
  @tailrec
  private[scalactic] def passOrFirstFail[T, E](o: T, fs: List[T => Validation[E]]): Validation[E] = {
    fs match {
      case Nil => Pass
      case head :: tail => 
        head(o) match {
          case Pass => passOrFirstFail(o, tail)
          case firstFail => firstFail
        }
    }
  }
}
