/*
 * Copyright 2001-2015 Artima, Inc.
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
package org.scalatest.exceptions

private[scalatest] object StackDepthExceptionHelper {

  def getStackDepth(stackTraces: Array[StackTraceElement], fileName: String, methodName: String, adjustment: Int = 0): Int = {
    // the scala-js part is temporary short-cut way to overcome locally built scala-js problem.
    //stackTraces.takeWhile(st => st.getFileName.startsWith("https://") || st.getFileName.contains("scala-js")).length + adjustment
    val depth1 = stackTraces.takeWhile(st => st.getFileName != null && (st.getFileName.startsWith("https://") || st.getFileName.contains("scala-js"))).length
    if (depth1 > 0 && stackTraces(depth1).getFileName == stackTraces(depth1 + 2).getFileName && stackTraces(depth1).getFileName == stackTraces(depth1 + 4).getFileName && stackTraces(depth1 + 1).getFileName == stackTraces(depth1 + 3).getFileName)
      depth1 + 5 + adjustment
    else if (depth1 > 0 && stackTraces(depth1).getFileName == stackTraces(depth1 + 2).getFileName && stackTraces(depth1).getFileName == stackTraces(depth1 + 3).getFileName && stackTraces(depth1).getFileName == stackTraces(depth1 + 4).getFileName && stackTraces(depth1).getFileName == stackTraces(depth1 + 5).getFileName)
      depth1 + 3 + adjustment
    else if (depth1 > 0 && stackTraces(depth1).getFileName == stackTraces(depth1 + 1).getFileName && stackTraces(depth1).getFileName == stackTraces(depth1 + 2).getFileName && stackTraces(depth1).getFileName == stackTraces(depth1 + 3).getFileName)
      depth1 + 1 + adjustment
    else if (depth1 > 0 && stackTraces(depth1 - 1).getFileName == stackTraces(depth1 + 1).getFileName && stackTraces(depth1).getFileName == stackTraces(depth1 + 2).getFileName) // for trait mixin stack trace
      depth1 + 2 + adjustment
    else if (depth1 > 0 && stackTraces(depth1).getFileName == stackTraces(depth1 + 3).getFileName && stackTraces(depth1 + 1).getFileName == stackTraces(depth1 + 2).getFileName && stackTraces(depth1).getFileName != stackTraces(depth1 + 1).getFileName)
      depth1 + 3 + adjustment
    else if (depth1 > 0 && stackTraces(depth1).getFileName == stackTraces(depth1 + 1).getFileName && stackTraces(depth1 + 2).getFileName.endsWith("scalajs/runtime/AnonFunctions.scala"))
      depth1 + 1 + adjustment
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
}