package org.scalatest.tools

import org.scalatest.events.Event
import org.scalatest.ResourcefulReporter
import java.net.Socket
import java.io.ObjectOutputStream
import java.io.BufferedOutputStream

private[scalatest] class SocketReporter(host: String, port: Int) extends ResourcefulReporter {

  private val socket = new Socket(host, port)
  private val out = new ObjectOutputStream(socket.getOutputStream)
  
  def apply(event: Event) {
    synchronized {
      out.writeObject(event)
      out.flush()
    }
  }

  def dispose() {
    out.flush()
    out.close()
    socket.close()
  }
  
}