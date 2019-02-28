package org.scalatest

import scala.concurrent.Future
import org.scalatest.compatible

/**
  * This trait is a default implementation for asynchronous testing with <code>scala.concurrent.Future</code> as a
  * Future system (which is as default in ScalaTest).
  * @see <code>FutureAssertionConverter</code> for more information.
  */
trait DefaultFutureAssertionConverter extends FutureAssertionConverter {
  override type FutureSystem = Future[compatible.Assertion]
  override def convertToScalaFuture(f: FutureSystem): Future[compatible.Assertion] = f
}
