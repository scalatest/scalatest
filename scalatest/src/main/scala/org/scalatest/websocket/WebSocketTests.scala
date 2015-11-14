package org.scalatest

import java.net.URI

import org.eclipse.jetty.websocket.api.{Session, WebSocketListener}
import org.eclipse.jetty.websocket.client.WebSocketClient
import org.mockito.Matchers._
import org.mockito.Mockito._
import org.mockito.invocation.InvocationOnMock
import org.mockito.stubbing.Answer
import org.scalatest._
import org.scalatest.mock.MockitoSugar

trait WebSocketTests
  extends BeforeAndAfter // with SuiteMixin
  with MockitoSugar {
  self: Suite =>

  type WebSocketClientType = WebSocketClient
  type WebSocketType = WebSocketListener
  type WebSocketSessionType = Session

  def endpoint: URI

  protected var client: WebSocketClientType = _
  var ws: WebSocketType = _

  before {
    ws = mock[WebSocketType]

    client = classOf[WebSocketClientType].newInstance()
    client.start()
  }

  def connect(onConnect: WebSocketSessionType => Unit):Unit =
    doConnect(Some(onConnect))

  def doConnect(onConnectOpt: Option[WebSocketSessionType => Unit] = None):Unit = {
    client.connect(ws, endpoint)

    onConnectOpt match {
      case Some(onConnect) =>
        when(ws.onWebSocketConnect(any[WebSocketSessionType])).then(new Answer[Unit] {
          override def answer(invocationOnMock: InvocationOnMock): Unit = {
            val session = invocationOnMock.getArguments.head.asInstanceOf[WebSocketSessionType]
            onConnect(session)
          }
        })
      case None => // test may define custom behaviour of mocked _ws_
    }
  }

  after {
    client.stop()
  }
}