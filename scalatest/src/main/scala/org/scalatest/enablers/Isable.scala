/*
 * Copyright 2001-2017 Artima, Inc.
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

import org.scalactic.source
import org.scalatest.exceptions.{StackDepthException, TestFailedException, TestPendingException}
import org.scalatest.{Assertion, PendingStatement, Resources}
import org.scalatest.Tag

import scala.collection.generic.FilterMonadic
import scala.concurrent.{ExecutionContext, Future}

trait Isable[T] {
  def registerPendingTestToRun(f: () => T, verb: String, rest: String, tags: List[Tag], pos: source.Position): Unit
}
