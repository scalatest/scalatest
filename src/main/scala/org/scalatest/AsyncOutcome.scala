
package org.scalatest

import scala.util.{Success, Try, Failure}
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._

trait AsyncOutcome {
  def onComplete(f: Try[Outcome] => Unit)
  def toStatus: Status
  def toOutcome: Outcome // may block
  def toFutureOutcome(implicit ctx: ExecutionContext): Future[Outcome]
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
  def toFutureOutcome(implicit ctx: ExecutionContext): Future[Outcome] = Future { past }
}

case class FutureOutcome(future: Future[Outcome])(implicit ctx: ExecutionContext) extends AsyncOutcome {
  def onComplete(f: Try[Outcome] => Unit) = {
    future.onComplete {
      case Success(result) => f(new Success(result))
      case Failure(ex) => f(new Failure(ex))
    }
  }
  def toStatus: Status = {
    val status = new ScalaTestStatefulStatus
    future.onComplete {
      case Success(result) => status.setCompleted()
      case Failure(ex) =>
        status.setFailed()
        status.setCompleted()
    }
    status
  }
  def toOutcome: Outcome = Await.result(future, Duration(1000, MILLISECONDS))
  def toFutureOutcome(implicit ctx: ExecutionContext): Future[Outcome] = future
}