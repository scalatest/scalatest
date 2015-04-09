package org.scalatest.tools

import org.scalatest.{Tracker, Reporter}
import org.scalatest.events.{ExceptionalEvent, Summary}
import sbt.testing.{Framework => BaseFramework, Event => SbtEvent, Status => SbtStatus, _}
import scala.collection.mutable.ListBuffer
import scala.compat.Platform

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
    new SlaveRunner(args, remoteArgs, testClassLoader, send)

  def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader): Runner =
    new MasterRunner(args, remoteArgs, testClassLoader)
}
