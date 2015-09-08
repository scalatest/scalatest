
package org.scalatest

import scala.concurrent.duration.Duration.Infinite
import scala.util.{Success, Try, Failure}
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._

private[scalatest] trait AsyncOutcome {
  def onComplete(f: Try[Outcome] => Unit)
  def toStatus: Status
  def toOutcome: Outcome // may block
  def toFutureOutcome: Future[Outcome]
}

private[scalatest] case class PastOutcome(past: Outcome) extends AsyncOutcome {

  def onComplete(f: Try[Outcome] => Unit) = {
    f(new Success(past))
  }
  def toStatus: Status =
    past match {
      case _: Failed => FailedStatus
      case _ => SucceededStatus
    }
  def toOutcome: Outcome = past
  def toFutureOutcome: Future[Outcome] = Future.successful(past)
}

private[scalatest] case class FutureOutcome(future: Future[Outcome])(implicit ctx: ExecutionContext) extends AsyncOutcome {

  private final val queue = new ConcurrentLinkedQueue[Try[Outcome] => Unit]
  private final val status = new ScalaTestStatefulStatus

  future.onComplete {
    case Success(result) =>
      for (f <- queue.iterator)
        f(Success(result))
      status.setCompleted()

    case Failure(ex) =>
      for (f <- queue.iterator)
        f(Failure(ex))
      status.setFailed()
      status.setCompleted()
  } /* fills in ctx here */

  def onComplete(f: Try[Outcome] => Unit) = {
    var executeLocally = false
    synchronized {
      if (!future.isCompleted)
        queue.add(f)
      else
        executeLocally = true
    }
    if (executeLocally) {
      future.value.get match {
        case Success(result) => f(new Success(result))
        case Failure(ex) => f(new Failure(ex))
      }
    }
  }
  def toStatus: Status = status
  // SKIP-SCALATESTJS-START
  def toOutcome: Outcome = Await.result(future, Duration.Inf)
  // SKIP-SCALATESTJS-END
  //SCALATESTJS-ONLY def toOutcome: Outcome = throw new UnsupportedOperationException("Not supported on scalatest-js")
  def toFutureOutcome: Future[Outcome] = future
}
