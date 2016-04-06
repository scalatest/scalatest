/*
 * Copyright 2001-2016 Artima, Inc.
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
package org.scalatest

import enablers.Futuristic

trait CompleteLastly {

  class ResultOfCompleteInvocation[T](futuristicBlock: => T)(implicit futuristic: Futuristic[T]) {

    def lastly(lastlyBlock: => Unit): T = {
      val result: T =
        try futuristicBlock // evaluate the by-name once
        catch {
          case ex: Throwable =>
            lastlyBlock  // execute the clean up
            throw ex // rethrow the same exception
        }
      futuristic.withCleanup(result) { lastlyBlock }
    }
    override def toString = "ResultOfCompleteInvocation"
  }

  def complete[T](completeBlock: => T)(implicit futuristic: Futuristic[T]): ResultOfCompleteInvocation[T] =
    new ResultOfCompleteInvocation[T](completeBlock)
}

object CompleteLastly extends CompleteLastly

