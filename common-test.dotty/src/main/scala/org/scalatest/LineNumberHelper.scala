package org.scalatest

import scala.quoted._
import scala.tasty._

private[scalatest] trait LineNumberHelper {
  inline def thisLineNumber = $LineNumberMacro.thisLineNumberImpl
}

object LineNumberMacro {
  def thisLineNumberImpl(implicit refl: Reflection): Expr[Int] = {
    import refl._

    refl.rootPosition.startLine.toExpr
  }
}
