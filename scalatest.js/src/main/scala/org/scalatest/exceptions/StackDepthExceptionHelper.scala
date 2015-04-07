package org.scalatest.exceptions

private[scalatest] object StackDepthExceptionHelper {

  def getStackDepth(stackTraces: Array[StackTraceElement], fileName: String, methodName: String, adjustment: Int = 0): Int = {
    val preceedingHttps = stackTraces.takeWhile(_.getFileName.startsWith("https://"))
    preceedingHttps.length + stackTraces.drop(preceedingHttps.length).takeWhile(_.getFileName.startsWith("file:/")).length - 1
  }

  def getStackDepthFun(fileName: String, methodName: String, adjustment: Int = 0): (StackDepthException => Int) = { sde =>
    getStackDepth(sde.getStackTrace, fileName, methodName, adjustment)
  }
}