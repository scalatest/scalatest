/*
 * Copyright 2001-2024 Artima, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.scalatest.tools

import org.scalatest.{Tracker, Reporter}
import org.scalatest.events.{ExceptionalEvent, Summary}
import sbt.testing.{Framework => BaseFramework, Event => SbtEvent, Status => SbtStatus, _}
import scala.collection.mutable.ListBuffer
import scala.compat.Platform

class Framework extends BaseFramework {
  
  def name(): String = "ScalaTest"

  def fingerprints(): Array[Fingerprint] =
    Array(
      new SubclassFingerprint {
        def superclassName(): String = "org.scalatest.Suite"
        def isModule(): Boolean = false
        def requireNoArgConstructor(): Boolean = true
      })

  def slaveRunner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader, send: (String) => Unit): Runner = 
    new SlaveRunner(args, remoteArgs, testClassLoader, send)

  def runner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader): Runner = 
    new MasterRunner(args, remoteArgs, testClassLoader)
}
