
package org.scalatest

import scala.util.{Success, Try, Failure}
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._

private[scalatest] sealed trait AsyncTestHolder {
  def toFutureOutcome: FutureOutcome
}

private[scalatest] case class PastAsyncTestHolder(past: Outcome) extends AsyncTestHolder {
  def toFutureOutcome: FutureOutcome = FutureOutcome { Future.successful(past) }
}

private[scalatest] case class FutureAsyncTestHolder(future: Future[Outcome]) extends AsyncTestHolder {
  def toFutureOutcome: FutureOutcome = FutureOutcome { future }
}
