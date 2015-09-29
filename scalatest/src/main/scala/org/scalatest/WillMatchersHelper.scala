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
package org.scalatest

private[scalatest] object WillMatchersHelper {

  def indicateSuccess(message: => String): Fact = Fact.Yes(message)

  def indicateSuccess(shouldBeTrue: Boolean, message: => String, negatedMessage: => String): Fact =
    Fact.Yes(if (shouldBeTrue) message else negatedMessage)

  def indicateFailure(failureMessage: => String): Fact = Fact.No(failureMessage)

  def indicateFailure(shouldBeTrue: Boolean, failureMessage: => String, negatedFailureMessage: => String): Fact =
    Fact.No(if (shouldBeTrue) failureMessage else negatedFailureMessage)

  def indicateFailure(shouldBeTrue: Boolean, failureMessage: => String, negatedFailureMessage: => String, optionalCause: Option[Throwable] = None, stackDepthAdjustment: Int = 0): Fact =
    Fact.No(if (shouldBeTrue) failureMessage else negatedFailureMessage)

  def indicateFailure(failureMessage: => String, optionalCause: Option[Throwable], stackDepthAdjustment: Int): Fact =
    Fact.No(failureMessage)

  def indicateFailure(shouldBeTrue: Boolean, withFriendlyReminder: Boolean, failureMessageWithFriendlyReminder: => String, failureMessageWithoutFriendlyReminder: => String,
                      negatedFailureMessageWithFriendlyReminder: => String, negatedFailureMessageWithoutFriendlyReminder: => String, optionalCause: Option[Throwable],
                      stackDepthAdjustment: Int): Fact =
    Fact.No(
      if (shouldBeTrue)
        if (withFriendlyReminder)
          failureMessageWithFriendlyReminder
        else
          failureMessageWithoutFriendlyReminder
      else
      if (withFriendlyReminder)
        negatedFailureMessageWithFriendlyReminder
      else
        negatedFailureMessageWithoutFriendlyReminder
    )

}