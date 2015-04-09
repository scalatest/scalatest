package org.scalatest.tools

import org.scalatest.Tracker
import org.scalatest.events.Summary
import sbt.testing.{Framework => BaseFramework, Event => SbtEvent, Status => SbtStatus, _}

import scala.compat.Platform

class SlaveRunner(theArgs: Array[String], theRemoteArgs: Array[String], testClassLoader: ClassLoader, notifyServer: String => Unit) extends Runner {

  // TODO: To take these from test arguments
  val presentAllDurations: Boolean = true
  val presentInColor: Boolean = true
  val presentShortStackTraces: Boolean = true
  val presentFullStackTraces: Boolean = true
  val presentUnformatted: Boolean = false
  val presentReminder: Boolean = false
  val presentReminderWithShortStackTraces: Boolean = false
  val presentReminderWithFullStackTraces: Boolean = false
  val presentReminderWithoutCanceledTests: Boolean = false

  val tracker = new Tracker

  def done(): String = ""

  def remoteArgs(): Array[String] = {
    theRemoteArgs
  }

  def args: Array[String] = {
    theArgs
  }

  def tasks(list: Array[TaskDef]): Array[Task] = {
    list.map(t => new TaskRunner(t, testClassLoader, tracker, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted, presentReminder,
      presentReminderWithShortStackTraces, presentReminderWithFullStackTraces, presentReminderWithoutCanceledTests, Some(notifyServer)))
  }

  def receiveMessage(msg: String): Option[String] =
    None

  def serializeTask(task: Task, serializer: (TaskDef) => String): String =
    serializer(task.taskDef())

  def deserializeTask(task: String, deserializer: (String) => TaskDef): Task =
    new TaskRunner(deserializer(task), testClassLoader, tracker, presentAllDurations, presentInColor, presentShortStackTraces, presentFullStackTraces, presentUnformatted, presentReminder,
      presentReminderWithShortStackTraces, presentReminderWithFullStackTraces, presentReminderWithoutCanceledTests, Some(notifyServer))

}