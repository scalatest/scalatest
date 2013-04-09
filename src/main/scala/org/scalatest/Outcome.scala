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

// Note no Ignored outcome, because ignore is done with a tag and is known
// before a test is executed. Outcome is only modeling the outcomes of
// executing a test body.
// trait FailedOrCanceled

sealed abstract class Outcome {
  def isSucceeded: Boolean = false
  def isFailed: Boolean = false
  def isCanceled: Boolean = false
  def isPending: Boolean = false
  def isOmitted: Boolean = false
  def isDefined: Boolean = false
  def isEmpty: Boolean = true
  def toOption: Option[Throwable] = None
  // def map[Z <: Throwable](fn: E => Z): Outcome[Z] 
  private[scalatest] def toUnit {
    this match {
      case Succeeded =>
      case Exceptional(e) => throw e
      case Pending(_) => throw new exceptions.TestPendingException
      case Omitted => throw new exceptions.TestOmittedException
    }
  }
}

abstract class Exceptional(ex: Throwable) extends Outcome {
  override def isDefined: Boolean = true
  override def isEmpty: Boolean = false
  override def toOption: Option[Throwable] = Some(ex)
  // def map[Z <: Throwable](fn: E => Z): Exceptional[Z]
}
case object Succeeded extends Outcome {
  override def isSucceeded: Boolean = true
  // def map[Z <: Throwable](fn: E => Z): Succeeded = this
}
case class Failed(ex: Throwable) extends Exceptional(ex) {
  override def isFailed: Boolean = true
  // def map[Z <: Throwable](fn: E => Z): Failed[Z] = Failed(fn(ex))
}
case class Canceled(ex: exceptions.TestCanceledException) extends Exceptional(ex) {
  override def isCanceled: Boolean = true
  // def map[Z <: Throwable](fn: E => Z): Canceled[Z] = Canceled(fn(ex))
}
object Canceled {
  def apply(message: String): Canceled = {
    if (message == null)
      throw new NullPointerException("message was null")
    val e = new exceptions.TestCanceledException(message, 0) // TODO: Verify this is hte correct stack depth.
    e.fillInStackTrace()
    Canceled(e)
  }
}
case class Pending(ex: Option[String] = None) extends Outcome {
  override def isPending: Boolean = true
  // def map[Z <: Throwable](fn: E => Z): Pending = this
}
case object Omitted extends Outcome {
  override def isOmitted: Boolean = true
}
/*
object FailedOrCanceled {
  def unapply(res: Outcome): Option[Throwable] = 
    res match {
      case Failed(ex) => Some(ex)
      case Canceled(ex) => Some(ex)
      case _ => None
    }
}
*/
object Exceptional {

  def apply(e: Throwable): Exceptional = 
    e match {
      case tce: exceptions.TestCanceledException => Canceled(tce)
      case _ => Failed(e)
    }

  def unapply(res: Outcome): Option[Throwable] = 
    res match {
      case Failed(ex) => Some(ex)
      case Canceled(ex) => Some(ex)
      case _ => None
    }
}

object Outcome {

/*
  implicit def convertOutcomeToIterable(res: Outcome): scala.collection.immutable.Iterable[Throwable] = {
    res match {
      case Exceptional(ex) => Vector(ex)
      case _ => Vector.empty
    }
  }
*/
}

trait OutcomeOf {
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

