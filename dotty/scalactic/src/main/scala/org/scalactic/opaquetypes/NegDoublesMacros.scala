package org.scalactic.opaquetypes

import scala.quoted.*

private[opaquetypes] object NegFiniteDoubleMacros {
  def intLiteralToNegFiniteDouble(x: Expr[Int])(using Quotes): Expr[NegDoubles.NegFiniteDouble] = {
    import quotes.reflect.*
    x.value match {
      case Some(v) if v < 0 => Expr(v.toDouble).asExprOf[NegDoubles.NegFiniteDouble]
      case Some(_) => report.errorAndAbort("NegFiniteDouble cannot be instantiated with a non-negative integer literal")
      case None => report.errorAndAbort("NegFiniteDouble conversion requires an integer literal")
    }
  }

  def floatLiteralToNegFiniteDouble(x: Expr[Float])(using Quotes): Expr[NegDoubles.NegFiniteDouble] = {
    import quotes.reflect.*
    x.value match {
      case Some(v) if v < 0.0f => Expr(v.toDouble).asExprOf[NegDoubles.NegFiniteDouble]
      case Some(_) => report.errorAndAbort("NegFiniteDouble cannot be instantiated with a non-negative float literal")
      case None => report.errorAndAbort("NegFiniteDouble conversion requires a float literal")
    }
  }

  def doubleLiteralToNegFiniteDouble(x: Expr[Double])(using Quotes): Expr[NegDoubles.NegFiniteDouble] = {
    import quotes.reflect.*
    x.value match {
      case Some(v) if v < 0.0 && v != Double.PositiveInfinity && v != Double.NegativeInfinity =>
        Expr(v).asExprOf[NegDoubles.NegFiniteDouble]
      case Some(_) => report.errorAndAbort("NegFiniteDouble cannot be instantiated with a non-negative double literal or infinity")
      case None => report.errorAndAbort("NegFiniteDouble conversion requires a double literal")
    }
  }
}
