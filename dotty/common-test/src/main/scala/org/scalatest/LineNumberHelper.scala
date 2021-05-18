package org.scalatest

import scala.quoted._

private[scalatest] trait LineNumberHelper {
  inline def thisLineNumber = ${ LineNumberMacro.thisLineNumberImpl }
}

object LineNumberMacro {
  def thisLineNumberImpl(using Quotes): Expr[Int] =
    Expr(quotes.reflect.Position.ofMacroExpansion.startLine + 1)
}
