package org.scalatest.exceptions

private[scalatest] object StackDepthExceptionHelper {

  def getStackDepth(stackTraces: Array[StackTraceElement], fileName: String, methodName: String, adjustment: Int = 0): Int = {
    stackTraces.takeWhile(_.getFileName.startsWith("https://")).length
  }

  def getStackDepthFun(fileName: String, methodName: String, adjustment: Int = 0): (StackDepthException => Int) = { sde =>
    getStackDepth(sde.getStackTrace, fileName, methodName, adjustment)
  }

  def getFailedCodeFileName(stackTraceElement: StackTraceElement): Option[String] = {
    val fileName = stackTraceElement.getFileName
    if (fileName != null) {
      val lastPathSeparatorIdx = fileName.lastIndexOf("/")
      if (lastPathSeparatorIdx >= 0)
        Some(fileName.substring(lastPathSeparatorIdx + 1))
      else
        Some(fileName)
    }
    else None
  }

  val macroCodeStackDepth: Int = 9
}