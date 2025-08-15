/*
 * Copyright 2001-2025 Artima, Inc.
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

import org.scalatest.events.{Event, EventXmlHelper}
import org.scalatest.ResourcefulReporter
import java.net.Socket
import java.io.PrintWriter
import java.io.BufferedOutputStream

private[scalatest] class XmlSocketReporter(host: String, port: Int) extends ResourcefulReporter {
  
  private val socket = new Socket(host, port)
  private val out = new PrintWriter(new BufferedOutputStream(socket.getOutputStream))
  
  def apply(event: Event): Unit = {
    out.println(EventXmlHelper.toXml(event).toString)
    out.flush()
  }

  def dispose(): Unit = {
    out.flush()
    out.close()
    socket.close()
  }
}
