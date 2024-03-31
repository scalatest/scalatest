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

import org.scalactic._
import org.scalactic.exceptions.NullArgumentException

private[scalatest] object StackDepthExceptionHelper {

  def getStackDepth(stackTraces: Array[StackTraceElement], fileName: String, methodName: String, adjustment: Int = 0): Int = {
    // the scala-js part is temporary short-cut way to overcome locally built scala-js problem.
    // stackTraces.takeWhile(st => st.getFileName.startsWith("https://") || st.getFileName.contains("scala-js")).length + adjustment
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

  def isMatch(ele: StackTraceElement, pos: source.Position): Boolean =
    Option(ele.getFileName).map(retrieveFileName) == Some(pos.fileName) && ele.getLineNumber == pos.lineNumber

  def getStackDepth(stackTrace: Array[StackTraceElement], pos: source.Position): Int = {
    val idx = stackTrace.indexWhere (e => isMatch(e, pos))
    if (idx >= 0)
      idx
    else {
      // Let's find the nearest for now, we could do better when we have SourceInfo into StackDepthException directly
      val firstFileNameIdx = stackTrace.indexWhere(e => Option(e.getFileName).map(retrieveFileName) == Some(pos.fileName))
      if (firstFileNameIdx >= 0)
        firstFileNameIdx
      else
        0
    }
  }

  def getStackDepthFun(pos: source.Position): (StackDepthException => Int) = { sde =>
    getStackDepth(sde.getStackTrace, pos)
  }

  def retrieveFileName(fileName: String): String = {
    val lastPathSeparatorIdx = fileName.lastIndexOf("/")
    if (lastPathSeparatorIdx >= 0)
      fileName.substring(lastPathSeparatorIdx + 1)
    else
      fileName
  }

  def getFailedCodeFileName(stackTraceElement: StackTraceElement): Option[String] = {
    val fileName = stackTraceElement.getFileName
    if (fileName != null)
      Some(retrieveFileName(fileName))
    else
      None
  }

  def posOrElseStackDepthFun(pos: Option[source.Position], sdf: StackDepthException => Int): Either[source.Position, StackDepthException => Int] = {
    // requireNonNull(pos, sdf) TODO30 this doesn't compile, probably a problem with hiding a package name again
    if (pos == null) throw new NullArgumentException("pos was null")
    if (sdf == null) throw new NullArgumentException("sdf was null")
    pos match {
      case Some(null) => throw new NullArgumentException("pos was Some(null)")
      case _ =>
    }
    pos match {
      case Some(pos) => Left(pos)
      case None => Right(sdf)
    }
  }
}