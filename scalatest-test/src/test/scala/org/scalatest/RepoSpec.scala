
package org.scalatest

import scala.concurrent._

import org.scalatest.Succeeded
import org.scalatest.freespec.AsyncFreeSpec

class RepoSpec extends AsyncFreeSpec /* with ParallelTestExecution */ {

  implicit override val executionContext: ExecutionContext =
    scala.concurrent.ExecutionContext.Implicits.global

  ('a' to 'z').map { x =>
    x.toString - {
      (1 to 20).map(i =>
        if (i == 2) {
         println("I GOT TO i == 2!")
          s"$x $i" ignore {
            info(s"about to ignore Future(Succeeded) $x $i")
            println(s"about to ignore Future(Succeeded) $x $i")
            Future(Succeeded)
          }
        }
        else if (i == 4)
          s"$x $i" is {
            info(s"about to is pending $x $i")
            pending
          }
        else if (i == 6)
          s"$x $i" ignore {
            info(s"about to ignore Future(pending) $x $i")
            pending
          }
        else if (i == 8)
          s"$x $i" in {
            info(s"about to in pending $x $i")
            pending
          }
        else if (i == 6) 
          s"$x $i" in {
            info(s"about to in succeed $x $i")
            succeed
          }
        else 
          s"$x $i" in Future(Succeeded)
       )
      // (1 to 10).map(i => s"$x $i" in Future { Thread.sleep(10); Succeeded })
    }
  }
}
/*

import org.scalatest.freespec.AsyncFreeSpec

class ExampleRepoSpec extends AsyncFlatSpec /* with ParallelTestExecution */ {

        //SCALATESTJS-ONLY implicit override def executionContext = org.scalatest.concurrent.TestExecutionContext.runNow
        //SCALATESTNATIVE-ONLY implicit override def executionContext = scala.concurrent.ExecutionContext.Implicits.global

        val a = 1

        it should "test 1" in {
          assert(a == 1)
        }

        it should "test 1.5" in {
          try assert(a == 2)
          catch {  
            case ex: Exception =>
              println("Here is the stack trace of the exception thrown from test 2:")
              ex.printStackTrace()
              throw ex
          }
        }

        it should "test 2" in {
          assert(a == 2)
        }

        it should "test 3" in {
          pending
        }

        it should "test 4" in {
          cancel
        }

        it should "test 5" ignore {
          cancel
        }

        // override def newInstance = new ExampleRepoSpec
      }
*/
