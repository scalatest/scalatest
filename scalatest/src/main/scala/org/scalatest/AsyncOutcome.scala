
package org.scalatest

import scala.util.{Success, Try, Failure}
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._

private[scalatest] trait AsyncOutcome {
  def onComplete(f: Try[Outcome] => Unit)
  def toStatus: Status
  // SKIP-SCALATESTJS,NATIVE-START
  def toOutcome: Outcome // may block
  // SKIP-SCALATESTJS,NATIVE-END
  def toInternalFutureOutcome: Future[Outcome]
  def toFutureOutcome: FutureOutcome
}

private[scalatest] case class PastOutcome(past: Outcome) extends AsyncOutcome {

  def onComplete(f: Try[Outcome] => Unit): Unit = {
    f(new Success(past))
  }
  def toStatus: Status =
    past match {
      case _: Failed => FailedStatus
      case _ => SucceededStatus
    }
  // SKIP-SCALATESTJS,NATIVE-START
  def toOutcome: Outcome = past
  // SKIP-SCALATESTJS,NATIVE-END
  def toInternalFutureOutcome: Future[Outcome] = Future.successful(past)
  def toFutureOutcome: FutureOutcome = FutureOutcome { Future.successful(past) }
}

private[scalatest] case class InternalFutureOutcome(future: Future[Outcome])(implicit ctx: ExecutionContext) extends AsyncOutcome {

  private final val queue = new ConcurrentLinkedQueue[Try[Outcome] => Unit]
  private final val status = new ScalaTestStatefulStatus

  future.onComplete {
    case Success(result) =>
      executeQueue(Success(result))
      status.setCompleted()

    case Failure(ex) =>
      executeQueue(Failure(ex))
      status.setFailedWith(ex)
      status.setCompleted()
  }

  def executeQueue(result: Try[Outcome]): Unit = {
    while (!queue.isEmpty) {
      val f = queue.poll
      if (f != null)
        f(result)
    }
  }

  def onComplete(f: Try[Outcome] => Unit): Unit = {
    queue.add(f)
    if (future.isCompleted)
      executeQueue(future.value.get)
  }
  def toStatus: Status = status
  // SKIP-SCALATESTJS,NATIVE-START
  def toOutcome: Outcome = Await.result(future, Duration.Inf)
  // SKIP-SCALATESTJS,NATIVE-END
  def toInternalFutureOutcome: Future[Outcome] = future
  def toFutureOutcome: FutureOutcome = FutureOutcome { future }
}
