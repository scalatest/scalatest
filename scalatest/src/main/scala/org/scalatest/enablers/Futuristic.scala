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
package org.scalatest.enablers

import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import org.scalatest.FutureOutcome

trait Futuristic[T] {
  def withCleanup(futuristic: T)(cleanup: => Unit): T
}

object Futuristic {

  implicit def futuristicNatureOfFutureOutcome(implicit executionContext: ExecutionContext): Futuristic[FutureOutcome] =
    new Futuristic[FutureOutcome] {
      def withCleanup(futuristic: FutureOutcome)(cleanup: => Unit): FutureOutcome = {
        futuristic onCompletedThen { _ => cleanup }
      }
    }

  implicit def futuristicNatureOfFutureOf[V](implicit executionContext: ExecutionContext): Futuristic[Future[V]] =
    new Futuristic[Future[V]] {
      def withCleanup(futuristic: Future[V])(cleanup: => Unit): Future[V] = {
        // First deal with Failure, and mimic finally semantics
        // but in future-space. The recoverWith will only execute
        // if this is a Failure, and will only return a Failure,
        // so the subsequent map will only happen if this recoverWith
        // does not happen.
        futuristic recoverWith {
          case firstEx: Throwable => 
            try {
              cleanup
              Future.failed(firstEx)
            }
            catch {
              case secondEx: Throwable =>
                Future.failed(secondEx)
            }
        } map { v => // Ensure cleanup happens for the Success case
          cleanup
          v
        }
      }
    }
}

