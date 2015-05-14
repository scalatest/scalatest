package org.scalatest.exceptions

private[scalatest] object StackDepthExceptionHelper {

  def getStackDepth(stackTraces: Array[StackTraceElement], fileName: String, methodName: String, adjustment: Int = 0): Int = {
    // the scala-js part is temporary short-cut way to overcome locally built scala-js problem.
    //stackTraces.takeWhile(st => st.getFileName.startsWith("https://") || st.getFileName.contains("scala-js")).length + adjustment
    val depth1 = stackTraces.takeWhile(st => st.getFileName.startsWith("https://") || st.getFileName.contains("scala-js")).length
    if (depth1 > 0 && stackTraces(depth1 - 1).getFileName == stackTraces(depth1 + 1).getFileName && stackTraces(depth1).getFileName == stackTraces(depth1 + 2).getFileName)  // for trait mixin stack trace
      depth1 + 2 + adjustment
    else if (depth1 > 0 && stackTraces(depth1).getFileName == stackTraces(depth1 + 3).getFileName && stackTraces(depth1 + 1).getFileName == stackTraces(depth1 + 2).getFileName && stackTraces(depth1).getFileName != stackTraces(depth1 + 1).getFileName)
      depth1 + 3 + adjustment
    else
      depth1 + adjustment
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