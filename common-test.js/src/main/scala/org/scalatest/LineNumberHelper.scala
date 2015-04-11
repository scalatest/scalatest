package org.scalatest

private[scalatest] trait LineNumberHelper {

  def thisLineNumber = {
    val st = Thread.currentThread.getStackTrace
    st.dropWhile(_.getFileName.startsWith("https://")).dropWhile(e => e.getFileName.endsWith("LineNumberHelper.scala") || e.getFileName.endsWith("SharedHelpers.scala")).head.getLineNumber
  }

}