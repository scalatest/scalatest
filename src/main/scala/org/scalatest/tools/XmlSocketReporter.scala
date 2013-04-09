package org.scalatest.tools

import org.scalatest.events.Event
import org.scalatest.ResourcefulReporter
import java.net.Socket
import java.io.PrintWriter
import java.io.BufferedOutputStream

private[scalatest] class XmlSocketReporter(host: String, port: Int) extends ResourcefulReporter {
  
  private val socket = new Socket(host, port)
  private val out = new PrintWriter(new BufferedOutputStream(socket.getOutputStream))
  
  def apply(event: Event) {
    out.println(event.toXml.toString)
    out.flush()
  }

  def dispose() {
    out.flush()
    out.close()
    socket.close()
  }
}