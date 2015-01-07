package org.scalatest.websocket

import java.net.URI

import org.eclipse.jetty.server.handler.HandlerList
import org.eclipse.jetty.server.{Server, ServerConnector}
import org.eclipse.jetty.servlet.ServletHandler
import org.eclipse.jetty.websocket.api.WebSocketAdapter
import org.eclipse.jetty.websocket.servlet.{WebSocketServlet, WebSocketServletFactory}
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.scalatest.{BeforeAndAfter, FlatSpec, Suite, WebSocketTests}

import scala.concurrent.duration._

trait EmbeddedJetty extends BeforeAndAfter {
  self: Suite =>

  def handlerList: HandlerList

  private var srv: Server = _

  def port = srv.getConnectors collectFirst {
    case con: ServerConnector => con.getLocalPort
  }

  before {
    srv = new Server(-1)
    srv.setHandler(handlerList)
    srv.start()
  }

  after {
    srv.stop()
  }
}

class WebSocketSuiteExample extends FlatSpec with WebSocketTests with EmbeddedJetty {

  class EchoSocket extends WebSocketAdapter {
    override def onWebSocketText(message: String): Unit =
      getRemote.sendString(s"echo: $message") 
  }

  class EchoServlet extends WebSocketServlet {
    override def configure(webSocketServletFactory: WebSocketServletFactory): Unit =
      webSocketServletFactory.register(classOf[EchoSocket])
  }

  override def handlerList: HandlerList = {
    val list = new HandlerList
    val servletHandler = new ServletHandler
    servletHandler.addServletWithMapping(classOf[EchoServlet], "/echo")
    list.addHandler(servletHandler)
    list
  }

  override def endpoint: URI = URI.create(s"ws://localhost:$port/echo")

  "EchoServer" should "echo any text message" in {
    connect {
      session:WebSocketSessionType =>
        session.getRemote.sendString("Hello World!")
    }

    verify(ws, timeout(5.seconds.toMillis.toInt)).onWebSocketText(anyString())
  }

}
