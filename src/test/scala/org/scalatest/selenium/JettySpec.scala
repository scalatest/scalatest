/*
 * Copyright 2001-2012 Artima, Inc.
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
package org.scalatest.selenium

import org.scalatest._
import org.eclipse.jetty.server.Server
import org.eclipse.jetty.webapp.WebAppContext

trait JettySpec extends FunSpec {
  
  private val serverThread = new Thread() {
    private val server = new Server(0)
    
    override def run() {
      val webapp = new WebAppContext("webapp", "/")
      server.setHandler(webapp)
      server.setStopAtShutdown(true)
      server.start()
      server.join()
    }
    
    def done() {
      server.stop()
    }
    
    def isStarted = server.isStarted()
    def getHost = {
      val conn = server.getConnectors()(0)
      "http://localhost:" + conn.getLocalPort + "/"
    }
  }
  
  lazy val host = serverThread.getHost

  override def run(testName: Option[String], args: Args): Status = {
    serverThread.start()
    while (!serverThread.isStarted)
      Thread.sleep(10)
    
    try super.run(testName, args)
    finally serverThread.done()
  }
}
