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

import java.util.concurrent.atomic.AtomicReference
import scala.scalanative.reflect.annotation.EnableReflectiveInstantiation
import java.io.{File, PrintWriter}
import org.scalatest.{Retries, Succeeded, Failed}
import org.scalatest.time.Span
import org.scalatest.time.Second

@EnableReflectiveInstantiation
object Runner {

  private[scalatest] val masterFun: AtomicReference[Option[String => Unit]] = new AtomicReference(None)

  def setMasterFun(fun: String => Unit): Unit = {
    masterFun.set(Some(fun))
  }

  def setDiscoveredSuites(suites: Set[String]): Unit = {
    System.setProperty("scalatest.DiscoveredSuites", suites.mkString(","))
  }

  def discoveredSuites: Option[Set[String]] = {
    masterFun.get match {
      case Some(fun) => 
        val tempFile = File.createTempFile("scalatest-", "-getDiscoveredSuites")
        // Tell the master runner to write the discovered suites to the temp file.
        fun(s"getDiscoveredSuites-${tempFile.getAbsolutePath}")
        // The above is asynchronous, so we need to wait until the temp file is written.
        // Bad idea to block, but I think that's the only way to do it here, and we do not expect discoveredSuites to be called often.
        while (tempFile.length() == 0)
          Thread.sleep(500)  
        val source = scala.io.Source.fromFile(tempFile)
        try {
          Some(source.mkString.split(",").toSet)
        } finally { 
          source.close()
          tempFile.delete()
        }
      case None => 
        val discoveredSuites = System.getProperty("scalatest.DiscoveredSuites")
        if (discoveredSuites == null || discoveredSuites.trim.isEmpty) None
        else Some(discoveredSuites.split(",").toSet)
    }
  }

}