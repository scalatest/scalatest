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

import org.scalatest.events._
import org.scalatest.ResourcefulReporter
import org.scalatest.Suite.anExceptionThatShouldCauseAnAbort
import java.net.Socket
import java.io.ObjectOutputStream
import java.io.BufferedOutputStream
import java.util.concurrent.atomic.AtomicReference

private[scalatest] class SocketReporter(host: String, port: Int) extends ResourcefulReporter {

  private val socket = new AtomicReference(new Socket(host, port))
  private val out = new AtomicReference(new ObjectOutputStream(socket.get.getOutputStream))

  def refresh(): Unit = {
    try {
      out.get.close()
      socket.get.close()
    }
    catch {
      case t: Throwable if anExceptionThatShouldCauseAnAbort(t) =>
    }
    socket.set(new Socket(host, port))
    out.set(new ObjectOutputStream(socket.get.getOutputStream))
  }
  
  def apply(event: Event): Unit = {
    synchronized {
      try {
        out.get.writeObject(event)
        out.get.flush()
      }
      catch {
        case e: java.io.NotSerializableException =>
          refresh()
          out.get.writeObject(event.ensureSerializable())

        case e: Throwable if anExceptionThatShouldCauseAnAbort(e) => refresh()
      }
    }
  }

  def dispose(): Unit = {
    out.get.flush()
    out.get.close()
    socket.get.close()
  }
  
}
