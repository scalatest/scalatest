package org.scalatest

private[scalatest] trait LineNumberHelper {

  import scala.language.experimental.macros

  def thisLineNumber: Int = macro LineNumberMacro.thisLineNumberImpl

}