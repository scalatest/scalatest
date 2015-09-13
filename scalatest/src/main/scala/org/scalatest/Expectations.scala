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

import scala.reflect.ClassTag
import Fact._

trait Expectations {
 
  def expectThrows[T <: AnyRef](f: => Any)(implicit classTag: ClassTag[T]): Expectation = {
    val clazz = classTag.runtimeClass
    try {
      f
      False(
        rawFailureMessage = Resources.rawExceptionExpected,
        rawNegatedFailureMessage = Resources.rawExpectedExceptionWasThrown,
        rawMidSentenceFailureMessage = Resources.rawMidSentenceExpectedExceptionWasThrown,
        rawMidSentenceNegatedFailureMessage = Resources.rawMidSentenceExpectedExceptionWasThrown,
        failureMessageArgs = Vector(clazz.getName),
        negatedFailureMessageArgs = Vector(clazz.getName),
        midSentenceFailureMessageArgs = Vector(clazz.getName),
        midSentenceNegatedFailureMessageArgs = Vector(clazz.getName)
      ) 
    }
    catch {
      case u: Throwable => {
        if (!clazz.isAssignableFrom(u.getClass))
          False(
            rawFailureMessage = Resources.rawWrongException,
            rawNegatedFailureMessage = Resources.rawExpectedExceptionWasThrown,
            rawMidSentenceFailureMessage = Resources.rawMidSentenceWrongException,
            rawMidSentenceNegatedFailureMessage = Resources.rawMidSentenceExpectedExceptionWasThrown,
            failureMessageArgs = Vector(clazz.getName),
            negatedFailureMessageArgs = Vector(clazz.getName),
            midSentenceFailureMessageArgs = Vector(clazz.getName),
            midSentenceNegatedFailureMessageArgs = Vector(clazz.getName),
            cause = Some(u)
          ) 
        else
          True(
            rawFailureMessage = Resources.rawExceptionExpected,
            rawNegatedFailureMessage = Resources.rawExpectedExceptionWasThrown,
            rawMidSentenceFailureMessage = Resources.rawMidSentenceExpectedExceptionWasThrown,
            rawMidSentenceNegatedFailureMessage = Resources.rawMidSentenceExpectedExceptionWasThrown,
            failureMessageArgs = Vector(clazz.getName),
            negatedFailureMessageArgs = Vector(clazz.getName),
            midSentenceFailureMessageArgs = Vector(clazz.getName),
            midSentenceNegatedFailureMessageArgs = Vector(clazz.getName),
            cause = Some(u)
          )
      }
    }
  }
}

object Expectations extends Expectations

