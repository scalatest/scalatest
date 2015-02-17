package org.scalatest.tools

import sbt.testing._
import org.scalajs.testinterface.TestUtils

final class TaskRunner(task: TaskDef, cl: ClassLoader) extends Task {
  def tags(): Array[String] = Array.empty
  def taskDef(): TaskDef = task

  def execute(eventHandler: EventHandler, loggers: Array[Logger], continuation: (Array[Task]) => Unit): Unit = {
    continuation(execute(eventHandler, loggers))
  }

  def execute(eventHandler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    /*for (suite <- Platform.loadModule[TestSuite](task.fullyQualifiedName(), cl)) {
      loggers.foreach(_.info(Console.GREEN + task.fullyQualifiedName() + Console.RESET))

      for (property <- suite.properties) {
        val startTS = System.currentTimeMillis()
        val result = property(())
        val endTS = System.currentTimeMillis()

        loggers.foreach(_.info(result.formatted(property.name)))
        eventHandler.handle(event(result, endTS - startTS))
      }
    }*/

    val suite = TestUtils.newInstance(task.fullyQualifiedName, cl)(Seq.empty)

    /*for (suite <- Platform.loadModule[org.scalatest.Suite](task.fullyQualifiedName(), cl)) {

    }*/

    Array.empty
  }
}
