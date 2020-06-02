package org.scalatest

import scala.quoted._
import scala.tasty._

private[scalatest] trait LineNumberHelper {
  inline def thisLineNumber = ${ LineNumberMacro.thisLineNumberImpl }
}

object LineNumberMacro {
  def thisLineNumberImpl(implicit qctx: QuoteContext): Expr[Int] = {
    import qctx.tasty._
    Expr(rootPosition.startLine)
  }
}
