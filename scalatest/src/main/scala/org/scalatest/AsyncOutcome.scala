
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

  private def executeQueue(result: Try[Outcome]): Unit = {
    while (!queue.isEmpty) {
      val f = queue.poll
      if (f != null)
        f(result)
    }
  }

  private def handleCompletion(tri: Try[Outcome]): Unit = {
    executeQueue(tri)
    if (!status.isCompleted) {
      tri match {
        case Failure(ex) =>
          status.setFailedWith(ex)
        case _ =>
      }
      status.setCompleted()
      executeQueue(tri) // Execute any callbacks that were registered after executeQueue returned above, but before we called setCompleted.
    }
  }

  def onComplete(f: Try[Outcome] => Unit): Unit = {

    queue.add(f)

    future.value match {

      case Some(tri) =>
        // This is the case where the test future competed before ScalaTest AsyncEngine had
        // a chance to call onComplete to register the test completion event to send to the reporter. So
        // go ahead an send the test completion event to the reporter now. (We added it to the
        // queue above at the beginning of this method.
        handleCompletion(tri)

      case None =>
        // Because executeQueue removes a callback from the queue before it invokes it, it doesn't hurt
        // to register the same callback each time. But only want to set completion on the status once.
        // We don't register this in the constructor, because then there is a race condition between
        // a test future that completes before AsyncEngine has a chance to call onComplete to register
        // the sending of the test completion event to the reporter. This race condition can cause ScalaTest
        // to throw a ConcurrentModificationException about the Informer being swapped ut too soon if
        // a thread pool execution context (like global) is used instead of ScalaTest's default execution
        // context in async styles.
        future.onComplete { tri => handleCompletion(tri) }
    }
  }
  def toStatus: Status = status
  // SKIP-SCALATESTJS,NATIVE-START
  def toOutcome: Outcome = Await.result(future, Duration.Inf)
  // SKIP-SCALATESTJS,NATIVE-END
  def toInternalFutureOutcome: Future[Outcome] = future
  def toFutureOutcome: FutureOutcome = FutureOutcome { future }
}
