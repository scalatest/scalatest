/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalatest.fixture

import org.scalatest.Outcome
import org.scalatest.PendingStatement
import org.scalatest.OutcomeOf.outcomeOf
import org.scalatest.AsyncTestHolder
import org.scalatest.FutureAsyncTestHolder
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

private[scalatest] case class AsyncPendingTransformer[FixtureParam](exceptionalTestFun: FixtureParam => PendingStatement)(implicit ctx: ExecutionContext) extends (FixtureParam => AsyncTestHolder) {
  def apply(fixture: FixtureParam): AsyncTestHolder = {
    FutureAsyncTestHolder {
      Future { outcomeOf { exceptionalTestFun(fixture) } }
    }
  }
}
