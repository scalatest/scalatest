package org.scalatest

import events.Event
import org.scalatest.events.TestFailed
import java.io.PrintStream
import DispatchReporter.propagateDispose

private[scalatest] class StopOnFailureReporter(dispatch: Reporter, stopper: Stopper, val out: PrintStream) extends CatchReporter {
    
  def doApply(event: Event) {
    event match {
      case testFailed: TestFailed => stopper.requestStop()
      case _ => 
    }
    dispatch(event)
  }
  
  def doDispose() {
    propagateDispose(dispatch)
  }
}
