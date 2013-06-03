/*
 * Copyright 2001-2012 Artima, Inc.
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

trait OutcomeOf {

  /**
   *
   */
  def outcomeOf(f: => Unit): Outcome = {
    try {                                         
      f                                           
      Succeeded
    }                                             
    catch {                                       
      case ex: exceptions.TestCanceledException => Canceled(ex)                           
      case exceptions.TestPendingException(reason) => Pending(reason)                           
      case ex: exceptions.TestOmittedException => Omitted                           
      case tfe: exceptions.TestFailedException => Failed(tfe)
      case ex: Throwable if !Suite.anExceptionThatShouldCauseAnAbort(ex) => Failed(ex)                           
    }
  }
}

object OutcomeOf extends OutcomeOf

