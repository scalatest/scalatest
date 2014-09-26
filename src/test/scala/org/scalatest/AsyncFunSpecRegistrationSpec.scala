package org.scalatest

import SharedHelpers.EventRecordingReporter
import scala.concurrent.Future

class AsyncFunSpecRegistrationSpec extends FunSpec {

  describe("AsyncFunSpecRegistration") {

    describe("can be used to define custom style traits that uses custom return type, ") {

      sealed trait ConcurrentTestResult
      case class AsyncErrorReporter(msg: String) extends ConcurrentTestResult
      object Done extends ConcurrentTestResult

      trait ConcurrentTestsLike extends AsyncFunSpecRegistration with OneInstancePerTest {

        type Registration = Future[ConcurrentTestResult]

        import scala.language.implicitConversions

        implicit def convertToFuture(o: ConcurrentTestResult): Future[ConcurrentTestResult] = Future { o }

        override protected def transformToOutcome(testFun: => Future[ConcurrentTestResult]): () => AsyncOutcome =
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

      it("ConcurrentTestsLike which handles custom result type from Future correctly.") {

        class ExampleSpec extends ConcurrentTestsLike {

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

      it("ConcurrentTestsLike which handles custom result type correctly.") {

        class ExampleSpec extends ConcurrentTestsLike {

          it("test 1") {
            Done
          }

          it("test 2") {
            Done
          }

          it("test 3") {
            AsyncErrorReporter("an error message")
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

      class ConcurrentTests extends ConcurrentTestsLike

      it("ConcurrentTests which handles custom result type from Future correctly.") {

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

      it("ConcurrentTests which handles custom result type correctly.") {

        class ExampleSpec extends ConcurrentTests {

          it("test 1") {
            Done
          }

          it("test 2") {
            Done
          }

          it("test 3") {
            AsyncErrorReporter("an error message")
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

}
