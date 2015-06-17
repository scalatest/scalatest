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

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import exceptions.ValidationFailedException

/**
 * Trait providing an implicit class that adds a <code>toOr</code> method to
 * <code>Try</code>, which converts <code>Success</code> to <code>Good</code>,
 * and <code>Failure</code> to <code>Bad</code>.
 */
trait FutureSugar {

  /**
   * Implicit class that adds a <code>toOr</code> method to
   * <code>Try</code>, which converts <code>Success</code> to <code>Good</code>,
   * and <code>Failure</code> to <code>Bad</code>.
   */
  implicit class Futureizer[T](theFuture: Future[T]) {
    def validating(hd: T => Validation[ErrorMessage], tl: (T => Validation[ErrorMessage])*)(implicit executor: ExecutionContext): Future[T] = {
      theFuture.flatMap { (o: T) =>
        TrySugar.passOrFirstFail(o, hd :: tl.toList) match {
          case Pass => theFuture
          case Fail(errorMessage) => Future.failed(ValidationFailedException(errorMessage))
        }
      }
    }
  }
}

object FutureSugar extends FutureSugar
