package org.scalatest.fixture

import org.scalatest._
import SharedHelpers.EventRecordingReporter
import scala.concurrent.Future

/*class AsyncFunSpecRegistrationSpec extends org.scalatest.FunSpec {

  describe("AsyncFunSpecRegistration") {

    describe("can be used to define custom style traits that uses custom return type, ") {

      sealed trait ConcurrentTestResult
      case class AsyncErrorReporter(msg: String) extends ConcurrentTestResult
      object Done extends ConcurrentTestResult

      trait ConcurrentTestsLike extends AsyncFunSpecRegistration with OneInstancePerTest {

        type Registration = Future[ConcurrentTestResult]

        implicit def convertToFuture(o: ConcurrentTestResult): Future[ConcurrentTestResult] = Future { o }

        override protected def transformToOutcome(testFun: FixtureParam => Future[ConcurrentTestResult]): (FixtureParam) => AsyncOutcome =
          (fixture: FixtureParam) => {
            val futureResult = testFun(fixture)
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

          type FixtureParam = String
          def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] =
            test("testing")

          it("test 1") { fixture =>
            Future {
              Done
            }
          }

          it("test 2") { fixture =>
            Future {
              Done
            }
          }

          it("test 3") { fixture =>
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

          type FixtureParam = String
          def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] =
            test("testing")

          it("test 1") { fixture =>
            Done
          }

          it("test 2") { fixture =>
            Done
          }

          it("test 3") { fixture =>
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

      abstract class ConcurrentTests extends ConcurrentTestsLike

      it("ConcurrentTests which handles custom result type from Future correctly.") {

        class ExampleSpec extends ConcurrentTests {

          type FixtureParam = String
          def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] =
            test("testing")

          it("test 1") { fixture =>
            Future {
              Done
            }
          }

          it("test 2") { fixture =>
            Future {
              Done
            }
          }

          it("test 3") { fixture =>
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

          type FixtureParam = String
          def withAsyncFixture(test: OneArgAsyncTest): Future[Outcome] =
            test("testing")

          it("test 1") { fixture =>
            Done
          }

          it("test 2") { fixture =>
            Done
          }

          it("test 3") { fixture =>
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

}*/