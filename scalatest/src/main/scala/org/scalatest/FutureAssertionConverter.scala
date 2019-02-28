package org.scalatest

import scala.concurrent.Future
import org.scalatest.compatible

/**
  * This trait is an extension point to testing asynchronous code transparently both
  * <code>scala.concurrent.Future</code> and other Future systems. To use with <code>scala.concurrent.Future</code>,
  * create a test class in the same way as existing abstract class (e.g. <code>AsyncFlatSpec</code>,
  * <code>AsyncWordSpec</code>, etc.) that is mixed-in <code>DefaultFutureAssertionConverter</code>, otherwise create a
  * custom abstract class that overrides <code>FutureSystem</code> type (the Future system you want to use with) and
  * <code>convertToScalaFuture</code> method (that transforms to <code>scala.concurrent.Future</code>), then use it in
  * your test class.
  *
  * <p>
  * Here's an example:
  * </p>
  *
  * <pre class="stHighlight">
  * package com.example.myawesome.{Future, Return, Throw}
  *
  * import org.scalatest.compatible.Assertion
  * import org.scalatest.{AsyncFlatSpecLike, compatible}
  *
  * abstract class MyAwesomeAsyncFlatSpec extends AsyncFlatSpecLike {
  *   override type FutureSystem = Future[compatible.Assertion]
  *
  *   override def convertToScalaFuture(f: FutureSystem): scala.concurrent.Future[compatible.Assertion] = {
  *     val promise: scala.concurrent.Promise[Assertion] = scala.concurrent.Promise()
  *     f.respond {
  *       case Return(value)    => promise.success(value)
  *       case Throw(exception) => promise.failure(exception)
  *     }
  *     promise.future
  *   }
  * }
  * </pre>
  */
trait FutureAssertionConverter {
  type FutureSystem
  def convertToScalaFuture(f: FutureSystem): Future[compatible.Assertion]
}
