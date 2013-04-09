package org.scalatest.concurrent
/*
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.OptionValues
import org.scalatest.time.Span
import org.scalatest.time.Milliseconds
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask
import akka.util.{Timeout => AkkaTimeout}
import akka.util.duration._
import org.scalatest.exceptions.TestFailedException
import org.scalatest.SharedHelpers.thisLineNumber
import org.scalatest.exceptions.TestPendingException
import org.scalatest.exceptions.TestCanceledException

class Sleeper extends Actor {
  def receive = {
    case napTime: Long => 
      Thread.sleep(napTime)
      sender ! "good morning"
    case vme: VirtualMachineError => 
      sender ! akka.actor.Status.Failure(vme)  // The standard Akka way to inform Future about the exception
      //throw vme  // throw this will cause the program to exit here.
    case e: Throwable => 
      sender ! akka.actor.Status.Failure(e)  // The standard Akka way to inform Future about the exception 
      // throw rte  // in Akka app this could be thrown and handled by supervisor.
    case _ => 
      sender ! "hi"
  }
}

class AkkaFuturesSpec extends FunSpec with ShouldMatchers with OptionValues with AkkaFutures {
  
  implicit val timeout = AkkaTimeout(1 second)
  
  val system = ActorSystem("TestSystem")
  val sleeper = system.actorOf(Props[Sleeper], name = "sleeper")
  
  describe("With a akka.dispatch.Future") {
    
    describe("when using the isReadyWithin method") {
      
      it("can be queried to make sure it is completed") {
        val future = sleeper ? 10L
        future.isReadyWithin(Span(20, Milliseconds)) should be (true)
      }
      
      it("should eventually return false if the future is never ready") {
        val future = sleeper ? 20L
        future.isReadyWithin(Span(10, Milliseconds)) should be (false)
      }
      
      
    }
    
    describe("when using futureValue method") {
      
      it("should return future value correctly if code in actor executed successfully") {
        val future = sleeper ? 10L
        Thread.sleep(20L)
        future.futureValue should be ("good morning")
      }
      
      it("should wrap any exception that normally causes a test to fail to propagate back wrapped in a TFE") {
        val rte = new RuntimeException("oops")
        val future = sleeper ? rte
        val caught = intercept[TestFailedException] {
          future.futureValue
        }
        caught.failedCodeLineNumber.value should equal(thisLineNumber - 2)
        caught.failedCodeFileName.value should be("AkkaFuturesSpec.scala")
        assert(caught.cause.value.isInstanceOf[RuntimeException])
        caught.cause.value.getMessage should be("oops")
      }
      
      it("should allow errors that do not normally cause a test to fail to propagate back without being wrapped in a TFE") {
        val future = sleeper ? new VirtualMachineError {}
        intercept[VirtualMachineError] {
          future.futureValue
        }
      }
      
      it("should allow TestPendingException, which does not normally cause a test to fail, through immediately when thrown") {
        val future = sleeper ? new TestPendingException
        intercept[TestPendingException] {
          future.futureValue
        }
      }
      
      it("should allow TestCanceledException, which does not normally cause a test to fail, through immediately when thrown") {
        val future = sleeper ? new TestCanceledException(sde => None, None, sde => 0)
        intercept[TestCanceledException] {
          future.futureValue
        }
      }
    }
    
  }
}*/
class AkkaFuturesSpec {}
