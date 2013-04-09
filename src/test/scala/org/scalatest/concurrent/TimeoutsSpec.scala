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
package org.scalatest.concurrent

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.OptionValues._
import Timeouts._
import org.scalatest.SharedHelpers.thisLineNumber
import java.io.ByteArrayInputStream
import java.net.SocketException
import java.net.ServerSocket
import java.net.Socket
import java.nio.channels.SelectionKey
import java.nio.channels.Selector
import java.nio.channels.ServerSocketChannel
import java.net.InetSocketAddress
import java.nio.channels.SocketChannel
import org.scalatest.time._
import org.scalatest.{SeveredStackTraces, FunSpec, Resources}
import org.scalatest.exceptions.TestFailedException

class TimeoutsSpec extends FunSpec with ShouldMatchers with SeveredStackTraces {

  describe("The failAfter construct") {

    it("should blow up with TestFailedException when it times out") {
      val caught = evaluating {
        failAfter(Span(100, Millis)) {
          Thread.sleep(200)
        }
      } should produce [TestFailedException]
      caught.message.value should be (Resources("timeoutFailedAfter", "100 milliseconds"))
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 5)
      caught.failedCodeFileName.value should be ("TimeoutsSpec.scala")
    }
 
    it("should pass normally when the timeout is not reached") {
      failAfter(Span(200, Millis)) {
        Thread.sleep(100)
      }
    }

    it("should blow up with TestFailedException when the task does not response interrupt request and pass after the timeout") {
      val caught = evaluating {
        failAfter(timeout = Span(100, Millis)) {
          for (i <- 1 to 10) {
            try {
              Thread.sleep(50)
            }
            catch {
              case _: InterruptedException =>
                Thread.interrupted() // Swallow the interrupt
            }
          }
        }
      } should produce [TestFailedException]
    }

    it("should not catch exception thrown from the test") {
      val caught = evaluating {
        failAfter(Span(100, Millis)) {
          throw new InterruptedException
        }
      } should produce [InterruptedException]
    }

    it("should set the exception thrown from the test after timeout as cause of TestFailedException") {
      val caught = evaluating {
        failAfter(Span(100, Millis)) {
          for (i <- 1 to 10) {
            try {
              Thread.sleep(50)
            }
            catch {
              case _: InterruptedException =>
                Thread.interrupted() // Swallow the interrupt
            }
          }
          throw new IllegalArgumentException("Something went wrong!")
        }
      } should produce [TestFailedException]
      caught.getCause().getClass === classOf[IllegalArgumentException]
    }
 
    it("should close a Socket connection via SocketInterruptor when the timeout is reached") {
      val serverSocket = new ServerSocket(0)
      @volatile
      var drag = true
      val serverThread = new Thread() {
        override def run() {
          val clientSocket = serverSocket.accept()
          while(drag) {
            try {
              Thread.sleep(100)
            }
            catch {
              case _: InterruptedException => Thread.interrupted()
            }
          }
          serverSocket.close()
        }
      }
      serverThread.start()
      val clientSocket = new Socket("localhost", serverSocket.getLocalPort())
      val inputStream = clientSocket.getInputStream()
      
      val caught = evaluating {
        failAfter(Span(100, Millis)) {
          inputStream.read()
        } (SocketInterruptor(clientSocket))
      } should produce [TestFailedException]
      clientSocket.close()
      drag = false
    }
    
    it("should close a Socket connection via FunInterruptor when the timeout is reached") {
      val serverSocket = new ServerSocket(0)
      @volatile
      var drag = true
      val serverThread = new Thread() {
        override def run() {
          val clientSocket = serverSocket.accept()
          while(drag) {
            try {
              Thread.sleep(100)
            }
            catch {
              case _: InterruptedException => Thread.interrupted()
            }
          }
          serverSocket.close()
        }
      }
      serverThread.start()
      val clientSocket = new Socket("localhost", serverSocket.getLocalPort())
      val inputStream = clientSocket.getInputStream()
      
      val caught = evaluating {
        failAfter(Span(100, Millis)) {
          inputStream.read()
        } (Interruptor { t => clientSocket.close() })
      } should produce [TestFailedException]
      clientSocket.close()
      drag = false
    }
    
    it("should wait for the test to finish when DoNotInterrupt is used.") {
      var x = 0
      val caught = evaluating {
        failAfter(Span(100, Millis)) {
          Thread.sleep(200)
          x = 1
        } (DoNotInterrupt)
      } should produce [TestFailedException]
      x should be (1)
    }
    
    it("should close a Selector connection via SelectorInterruptor when the timeout is reached") {
      val selector = Selector.open()
      val ssChannel = ServerSocketChannel.open()
      ssChannel.configureBlocking(false)
      ssChannel.socket().bind(new InetSocketAddress(0))
      ssChannel.register(selector, SelectionKey.OP_ACCEPT)
      @volatile
      var drag = true
      val serverThread = new Thread() {
        override def run() {
          selector.select()
          val it = selector.selectedKeys.iterator
          while (it.hasNext) {
            val selKey = it.next().asInstanceOf[SelectionKey]
            it.remove()
            if (selKey.isAcceptable()) {
              val ssChannel = selKey.channel().asInstanceOf[ServerSocketChannel]
              while(drag) {
                try {
                  Thread.sleep(100)
                }
                catch {
                  case _: InterruptedException => Thread.interrupted()
                }
              }
            }
          }
          ssChannel.close()
        }
      }
    
      val clientSelector = Selector.open();
      val sChannel = SocketChannel.open()
      sChannel.configureBlocking(false);
      sChannel.connect(new InetSocketAddress("localhost", ssChannel.socket().getLocalPort()));
      sChannel.register(selector, sChannel.validOps());
    
      val caught = evaluating {
        failAfter(Span(100, Millis)) {
          clientSelector.select()
        } (SelectorInterruptor(clientSelector))
      } should produce [TestFailedException]
      clientSelector.close()
      drag = false
    }
  }
  
/*
  describe("The cancelAfter construct") {
    
    it("should blow up with TestCanceledException when timeout") {
      val caught = evaluating {
        cancelAfter(1000) {
          Thread.sleep(2000)
        }
      } should produce [TestCanceledException]
      caught.message.value should be (Resources("timeoutCanceledAfter", "1000"))
      caught.failedCodeLineNumber.value should equal (thisLineNumber - 5)
      caught.failedCodeFileName.value should be ("TimeoutsSpec.scala")
    }
    
    it("should pass normally when timeout is not reached") {
      cancelAfter(2000) {
        Thread.sleep(1000)
      }
    }
    
    it("should blow up with TestCanceledException when the task does not response interrupt request and pass after the timeout") {
      val caught = evaluating {
        cancelAfter(timeout = 1000) {
          for (i <- 1 to 10) {
            try {
              Thread.sleep(500)
            }
            catch {
              case _: InterruptedException =>
                Thread.interrupted() // Swallow the interrupt
            }
          }
        }
      } should produce [TestCanceledException]
    }
    
    it("should not catch exception thrown from the test") {
      val caught = evaluating {
        cancelAfter(1000) {
          throw new InterruptedException
        }
      } should produce [InterruptedException]
    }
    
    it("should set exception thrown from the test after timeout as cause of TestCanceledException") {
      val caught = evaluating {
        cancelAfter(1000) {
          for (i <- 1 to 10) {
            try {
              Thread.sleep(500)
            }
            catch {
              case _: InterruptedException =>
                Thread.interrupted() // Swallow the interrupt
            }
          }
          throw new IllegalArgumentException("Something goes wrong!")
        }
      } should produce [TestCanceledException]
      caught.getCause().getClass === classOf[IllegalArgumentException]
    }
    
    it("should close Socket connection via SocketInterruptor when timeout reached") {
      val serverSocket = new ServerSocket(0)
      @volatile
      var drag = true
      val serverThread = new Thread() {
        override def run() {
          val clientSocket = serverSocket.accept()
          while(drag) {
            try {
              Thread.sleep(1000)
            }
            catch {
              case _: InterruptedException => Thread.interrupted()
            }
          }
          serverSocket.close()
        }
      }
      serverThread.start()
      val clientSocket = new Socket("localhost", serverSocket.getLocalPort())
      val inputStream = clientSocket.getInputStream()
      
      val caught = evaluating {
        cancelAfter(1000) {
          inputStream.read()
        } (SocketInterruptor(clientSocket))
      } should produce [TestCanceledException]
      clientSocket.close()
      drag = false
    }
    
    it("should close Socket connection via FunInterruptor when timeout reached") {
      val serverSocket = new ServerSocket(0)
      @volatile
      var drag = true
      val serverThread = new Thread() {
        override def run() {
          val clientSocket = serverSocket.accept()
          while(drag) {
            try {
              Thread.sleep(1000)
            }
            catch {
              case _: InterruptedException => Thread.interrupted()
            }
          }
          serverSocket.close()
        }
      }
      serverThread.start()
      val clientSocket = new Socket("localhost", serverSocket.getLocalPort())
      val inputStream = clientSocket.getInputStream()
      
      val caught = evaluating {
        cancelAfter(1000) {
          inputStream.read()
        } ( Interruptor { clientSocket.close() } )
      } should produce [TestCanceledException]
      clientSocket.close()
      drag = false
    }
    
    it("should wait for the test to finish when DoNotInterrupt is used.") {
      var x = 0
      val caught = evaluating {
        cancelAfter(1000) {
          Thread.sleep(2000)
          x = 1
        } ( DoNotInterrupt() )
      } should produce [TestCanceledException]
      x should be (1)
    }
    
    it("should close Selector connection via SelectorInterruptor when timeout reached") {
      val selector = Selector.open()
      val ssChannel = ServerSocketChannel.open()
      ssChannel.configureBlocking(false)
      ssChannel.socket().bind(new InetSocketAddress(0))
      ssChannel.register(selector, SelectionKey.OP_ACCEPT)
      @volatile
      var drag = true
      val serverThread = new Thread() {
        override def run() {
          selector.select()
          val it = selector.selectedKeys.iterator
          while (it.hasNext) {
            val selKey = it.next().asInstanceOf[SelectionKey]
            it.remove()
            if (selKey.isAcceptable()) {
              val ssChannel = selKey.channel().asInstanceOf[ServerSocketChannel]
              while(drag) {
                try {
                  Thread.sleep(1000)
                }
                catch {
                  case _: InterruptedException => Thread.interrupted()
                }
              }
            }
          }
          ssChannel.close()
        }
      }
    
      val clientSelector = Selector.open();
      val sChannel = SocketChannel.open()
      sChannel.configureBlocking(false);
      sChannel.connect(new InetSocketAddress("localhost", ssChannel.socket().getLocalPort()));
      sChannel.register(selector, sChannel.validOps());
    
      val caught = evaluating {
        cancelAfter(1000) {
          clientSelector.select()
        } (SelectorInterruptor(clientSelector))
      } should produce [TestCanceledException]
      clientSelector.close()
      drag = false
    }
  }
*/
}
