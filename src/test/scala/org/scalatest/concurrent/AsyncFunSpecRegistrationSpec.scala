package org.scalatest.concurrent

import org.scalatest.SharedHelpers.EventRecordingReporter
import org.scalatest._
import scala.concurrent.Future

class AsyncFunSpecRegistrationSpec extends FunSpec {

  describe("AsyncFunSpecRegistrationSpec") {

    it("can be used to support custom test return type") {

      sealed trait ConcurrentTestResult
      case class AsyncErrorReporter(msg: String) extends ConcurrentTestResult
      object Done extends ConcurrentTestResult

      trait ConcurrentTestsLike extends AsyncFunSpecRegistration with OneInstancePerTest {

        type Registration = Future[ConcurrentTestResult]

        override protected def transformFun(testFun: => Future[ConcurrentTestResult]): () => AsyncOutcome =
          () => {
            val futureResult = testFun
            FutureOutcome(
              futureResult.map { result =>
                result match {
                  case Done => Succeeded
                  case AsyncErrorReporter(msg: String) => Failed(new RuntimeException(msg))
                }
              }.recover {
                case ex: Throwable if !Suite.anExceptionThatShouldCauseAnAbort(ex) => Failed(ex)
              }
            )
          }

      }

      class ConcurrentTests extends ConcurrentTestsLike

      class ExampleSpec extends ConcurrentTests {

        it("test 1") {
          Future {
            Done
          }
        }

        it("test 2") {
          Future {
            Done
          }
        }

        it("test 3") {
          Future {
            AsyncErrorReporter("an error message")
          }
        }

        override def newInstance = new ExampleSpec
      }

      val rep = new EventRecordingReporter
      val spec = new ExampleSpec
      val status = spec.run(None, Args(reporter = rep))
      status.waitUntilCompleted()
      assert(rep.testSucceededEventsReceived.length == 2)
      assert(rep.testFailedEventsReceived.length == 1)
      val ex = rep.testFailedEventsReceived(0).throwable
      assert(ex.isDefined)
      val t = ex.get
      assert(t.isInstanceOf[RuntimeException])
      assert(t.getMessage == "an error message")
    }

  }

}