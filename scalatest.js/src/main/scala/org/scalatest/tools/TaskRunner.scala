package org.scalatest.tools

import sbt.testing._
import org.scalajs.testinterface.TestUtils
import org.scalatest.Suite

final class TaskRunner(task: TaskDef, cl: ClassLoader) extends Task {
  def tags(): Array[String] = Array.empty
  def taskDef(): TaskDef = task

  def execute(eventHandler: EventHandler, loggers: Array[Logger], continuation: (Array[Task]) => Unit): Unit = {
    continuation(execute(eventHandler, loggers))
  }

  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val suite = TestUtils.newInstance(task.fullyQualifiedName, cl)(Seq.empty).asInstanceOf[Suite]

    // TODO: Support nested suites
    Array.empty
  }
}
