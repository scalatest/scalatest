/*
 * Copyright 2001-2015 Artima, Inc.
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

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

trait AsyncRegistrationPolicy {

  implicit def executionContext: ExecutionContext

  // These two made implicit in AsyncCompatibility, non-implicit in AsyncSafety
  def convertAnyToFutureAssertion(o: Any): Future[Assertion]
  def convertFutureTToFutureAssertion[T](o: Future[T]): Future[Assertion]

  def convertExpectationToFutureAssertion(e: Expectation): Future[Assertion]
  def convertFutureExpectationToFutureAssertion(o: Future[Expectation]): Future[Assertion]
}

