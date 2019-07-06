
package org.scalatest

import scala.util.{Success, Try, Failure}
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._

private[scalatest] sealed trait AsyncTestHolder {
  def toStatus: Status
  // SKIP-SCALATESTJS,NATIVE-START
  def toOutcome: Outcome // may block
  // SKIP-SCALATESTJS,NATIVE-END
  def toFutureOfOutcome: Future[Outcome]
  def toFutureOutcome: FutureOutcome
}

private[scalatest] case class PastAsyncTestHolder(past: Outcome) extends AsyncTestHolder {

  def toStatus: Status =
    past match {
      case _: Failed => FailedStatus
      case _ => SucceededStatus
    }
  // SKIP-SCALATESTJS,NATIVE-START
  def toOutcome: Outcome = past
  // SKIP-SCALATESTJS,NATIVE-END
  def toFutureOfOutcome: Future[Outcome] = Future.successful(past)
  def toFutureOutcome: FutureOutcome = FutureOutcome { Future.successful(past) }
}

private[scalatest] case class FutureAsyncTestHolder(future: Future[Outcome])(implicit ctx: ExecutionContext) extends AsyncTestHolder {

  private final val status = new ScalaTestStatefulStatus

  future.onComplete {
    case Success(result) =>
      status.setCompleted()

    case Failure(ex) =>
      status.setFailedWith(ex)
      status.setCompleted()
  } /* fills in ctx here */

  def toStatus: Status = status
  // SKIP-SCALATESTJS,NATIVE-START
  def toOutcome: Outcome = Await.result(future, Duration.Inf)
  // SKIP-SCALATESTJS,NATIVE-END
  def toFutureOfOutcome: Future[Outcome] = future
  def toFutureOutcome: FutureOutcome = FutureOutcome { future }
}
