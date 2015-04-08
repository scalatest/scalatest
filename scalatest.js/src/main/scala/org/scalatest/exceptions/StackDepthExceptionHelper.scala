package org.scalatest.exceptions

private[scalatest] object StackDepthExceptionHelper {

  def getStackDepth(stackTraces: Array[StackTraceElement], fileName: String, methodName: String, adjustment: Int = 0): Int = {
    stackTraces.takeWhile(_.getFileName.startsWith("https://")).length
  }

  def getStackDepthFun(fileName: String, methodName: String, adjustment: Int = 0): (StackDepthException => Int) = { sde =>
    getStackDepth(sde.getStackTrace, fileName, methodName, adjustment)
  }
}