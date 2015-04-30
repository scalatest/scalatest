package org.scalatest.exceptions

private[scalatest] object StackDepthExceptionHelper {

  def getStackDepth(stackTraces: Array[StackTraceElement], fileName: String, methodName: String, adjustment: Int = 0): Int = {
    // the scala-js part is temporary short-cut way to overcome locally built scala-js problem.
    stackTraces.takeWhile(st => st.getFileName.startsWith("https://") || st.getFileName.contains("scala-js")).length
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