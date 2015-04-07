package org.scalatest.tools

import org.scalatest.Tracker
import sbt.testing.{Framework => BaseFramework, _}

class Framework extends BaseFramework {
  
  def name: String = "ScalaTest"

  def fingerprints: Array[Fingerprint] =
    Array(
      new SubclassFingerprint {
        def superclassName = "org.scalatest.Suite"
        def isModule = false
        def requireNoArgConstructor = true
      })


  def slaveRunner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader, send: (String) => Unit): Runner =
    runner(args, remoteArgs, testClassLoader)

  def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader): Runner = {
    val theseArgs = args
    val theseRemoteArgs = remoteArgs

    new Runner {

      val tracker = new Tracker

      def done(): String = ""

      def remoteArgs(): Array[String] = {
        theseRemoteArgs
      }

      def args: Array[String] = {
        theseArgs
      }

      def tasks(list: Array[TaskDef]): Array[Task] = {
        list.map(t => new TaskRunner(t, testClassLoader, tracker))
      }

      def receiveMessage(msg: String): Option[String] = {
        None
      }

      def serializeTask(task: Task, serializer: (TaskDef) => String): String =
        serializer(task.taskDef())

      def deserializeTask(task: String, deserializer: (String) => TaskDef): Task =
        new TaskRunner(deserializer(task), testClassLoader, tracker)
    }
  }
}
