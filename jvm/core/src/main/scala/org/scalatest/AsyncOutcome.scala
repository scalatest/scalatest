
package org.scalatest

import scala.util.{Success, Try, Failure}
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._

private[scalatest] sealed trait AsyncOutcome {
  def toStatus: Status
  // SKIP-SCALATESTJS-START
  def toOutcome: Outcome // may block
  // SKIP-SCALATESTJS-END
  def toFutureOfOutcome: Future[Outcome]
  def toFutureOutcome: FutureOutcome
}

private[scalatest] case class PastAsyncOutcome(past: Outcome, onCompleteFun: Try[Outcome] => Unit) extends AsyncOutcome {

  onCompleteFun(new Success(past))

  def toStatus: Status =
    past match {
      case _: Failed => FailedStatus
      case _ => SucceededStatus
    }
  // SKIP-SCALATESTJS-START
  def toOutcome: Outcome = past
  // SKIP-SCALATESTJS-END
  def toFutureOfOutcome: Future[Outcome] = Future.successful(past)
  def toFutureOutcome: FutureOutcome = FutureOutcome { Future.successful(past) }
}

private[scalatest] case class FutureAsyncOutcome(future: Future[Outcome], onCompleteFun: Try[Outcome] => Unit)(implicit ctx: ExecutionContext) extends AsyncOutcome {

  private final val status = new ScalaTestStatefulStatus

  // Must use future.onComplete so that we let the execution context
  // decide how to execute it.
  future.onComplete { tri =>
    onCompleteFun(tri)
    tri match {
      case Failure(ex) => status.setFailedWith(ex)
      case _ =>
    }
    status.setCompleted()
  } /* fills in ctx here */

  def toStatus: Status = status
  // SKIP-SCALATESTJS-START
  def toOutcome: Outcome = Await.result(future, Duration.Inf)
  // SKIP-SCALATESTJS-END
  def toFutureOfOutcome: Future[Outcome] = future
  def toFutureOutcome: FutureOutcome = FutureOutcome { future }
}
