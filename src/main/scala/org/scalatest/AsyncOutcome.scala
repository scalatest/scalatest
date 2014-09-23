
package org.scalatest

import scala.util.Try
import scala.util.Success
import scala.concurrent.Future

trait AsyncOutcome {
  def onComplete(f: Try[Outcome] => Unit)
  def toStatus: Status
  def toOutcome: Outcome // may block
}

case class PastOutcome(past: Outcome) extends AsyncOutcome {
  def onComplete(f: Try[Outcome] => Unit) = {
    f(new Success(past))
  }
  def toStatus: Status =
    past match {
      case _: Failed => FailedStatus
      case _ => SucceededStatus
    }
  def toOutcome: Outcome = past
}

case class FutureOutcome(future: Future[Outcome]) extends AsyncOutcome {
  def onComplete(f: Try[Outcome] => Unit) = ???
  def toStatus: Status = ???
  def toOutcome: Outcome = ???
}

