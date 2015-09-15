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
      No(
        rawFactMessage = Resources.rawExceptionExpected,
        rawNegatedFactMessage = Resources.rawFactNoExceptionWasThrown,
        rawMidSentenceFactMessage = Resources.rawMidSentenceExpectedExceptionWasThrown,
        rawMidSentenceNegatedFactMessage = Resources.rawMidSentenceFactNoExceptionWasThrown,
        factMessageArgs = Vector(clazz.getName),
        negatedFactMessageArgs = Vector.empty,
        midSentenceFactMessageArgs = Vector(clazz.getName),
        midSentenceNegatedFactMessageArgs = Vector.empty
      ) 
    }
    catch {
      case u: Throwable => {
        if (!clazz.isAssignableFrom(u.getClass))
          No(
            rawFactMessage = Resources.rawWrongException,
            rawNegatedFactMessage = Resources.rawFactExceptionWasThrown,
            rawMidSentenceFactMessage = Resources.rawMidSentenceWrongException,
            rawMidSentenceNegatedFactMessage = Resources.rawMidSentenceFactExceptionWasThrown,
            factMessageArgs = Vector(clazz.getName, u.getClass.getName),
            negatedFactMessageArgs = Vector(u.getClass.getName),
            midSentenceFactMessageArgs = Vector(clazz.getName, u.getClass.getName),
            midSentenceNegatedFactMessageArgs = Vector(u.getClass.getName),
            cause = Some(u)
          ) 
        else
          Yes(
            rawFactMessage = Resources.rawExceptionExpected,
            rawNegatedFactMessage = Resources.rawFactExceptionWasThrown,
            rawMidSentenceFactMessage = Resources.rawMidSentenceExpectedExceptionWasThrown,
            rawMidSentenceNegatedFactMessage = Resources.rawMidSentenceFactExceptionWasThrown,
            factMessageArgs = Vector(clazz.getName),
            negatedFactMessageArgs = Vector(clazz.getName),
            midSentenceFactMessageArgs = Vector(clazz.getName),
            midSentenceNegatedFactMessageArgs = Vector(clazz.getName),
            cause = Some(u)
          )
      }
    }
  }
}

object Expectations extends Expectations

