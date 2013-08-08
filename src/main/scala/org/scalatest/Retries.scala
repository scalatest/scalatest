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

import time.Span

trait Retries {

  def withRetry(blk: => Outcome): Outcome = withRetry(Span.Zero)(blk)

  def withRetry(delay: Span)(blk: => Outcome): Outcome = {
    val firstOutcome = blk
    firstOutcome match {
      case Failed(ex) =>
        blk match {
          case Succeeded => Canceled(Resources("testFlickered"), ex)
          case other => firstOutcome
        }
      case Canceled(ex) =>
        blk match {
          case Succeeded => Succeeded
          case failed: Failed => failed // Never hide a failure.
          case other => firstOutcome
        }
      case other => other
    }
  }

  def withRetryOnFailure(blk: => Outcome): Outcome = withRetryOnFailure(Span.Zero)(blk)

  def withRetryOnFailure(delay: Span)(blk: => Outcome): Outcome = {
    val firstOutcome = blk
    firstOutcome match {
      case Failed(ex) =>
        if (delay != Span.Zero)
          Thread.sleep(delay.millisPart)
        blk match {
          case Succeeded => Canceled(Resources("testFlickered"), ex)
          case other => firstOutcome
        }
      case other => other
    }
  }
  def withRetryOnCancel(blk: => Outcome): Outcome = withRetryOnCancel(Span.Zero)(blk)

  def withRetryOnCancel(delay: Span)(blk: => Outcome): Outcome = {
    val firstOutcome = blk
    firstOutcome match {
      case Canceled(ex) =>
        if (delay != Span.Zero)
          Thread.sleep(delay.millisPart)
        Thread.sleep(delay.millisPart)
        blk match {
          case Succeeded => Succeeded
          case failed: Failed => failed // Never hide a failure.
          case other => firstOutcome
        }
      case other => other
    }
  }
}

object Retries extends Retries

