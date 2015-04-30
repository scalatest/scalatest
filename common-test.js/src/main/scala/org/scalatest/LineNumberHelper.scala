package org.scalatest

private[scalatest] trait LineNumberHelper {

  def thisLineNumber = {
    val st = Thread.currentThread.getStackTrace
    // the scala-js part is temporary short-cut way to overcome locally built scala-js problem.
    st.dropWhile(st => st.getFileName.startsWith("https://") || st.getFileName.contains("scala-js")).dropWhile(e => e.getFileName.endsWith("LineNumberHelper.scala") || e.getFileName.endsWith("SharedHelpers.scala")).head.getLineNumber
  }

}