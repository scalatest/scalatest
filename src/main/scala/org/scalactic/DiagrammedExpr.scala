/*
 * Copyright 2001-2013 Artima, Inc.
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
package org.scalactic

private[org] case class AnchorValue(anchor: Int, value: Any)

/**
 * A trait that represent an expression recorded by <code>DiagrammedExprMacro</code>, which includes the following members:
 *
 * <ul>
 * <li>a boolean value</li>
 * <li>an anchor that records the position of this expression</li>
 * <li>anchor values of this expression (including sub-expressions)</li>
 * </ul>
 *
 */
trait DiagrammedExpr[T] {
  val anchor: Int
  def anchorValues: List[AnchorValue]
  def value: T

  protected[scalactic] def eliminateDuplicates(anchorValues: List[AnchorValue]): List[AnchorValue] =
    (anchorValues.groupBy(_.anchor).map { case (anchor, group) =>
      group.last
    }).toList
}

/**
 * <code>DiagrammedExpr</code> companion object that provides factory methods to create different sub types of <code>DiagrammedExpr</code>
 */
object DiagrammedExpr {

  /**
   * Create simple <code>DiagrammedExpr</code> that wraps expressions that is not <code>Select</code>, <code>Apply</code> or <code>TypeApply</code>.
   *
   * @param expression the expression value
   * @param anchor the anchor of the expression
   * @return a simple <code>DiagrammedExpr</code>
   */
  def simpleExpr[T](expression: T, anchor: Int): DiagrammedExpr[T] = new DiagrammedSimpleExpr(expression, anchor)

  /**
   * Create apply <code>DiagrammedExpr</code> that wraps <code>Apply</code> or <code>TypeApply</code> expression.
   *
   * @param qualifier the qualifier of the <code>Apply</code> or <code>TypeApply</code> expression
   * @param args the arguments of the <code>Apply</code> or <code>TypeApply</code> expression
   * @param value the expression value
   * @param anchor the anchor of the expression
   * @return an apply <code>DiagrammedExpr</code>
   */
  def applyExpr[T](qualifier: DiagrammedExpr[_], args: List[DiagrammedExpr[_]], value: T, anchor: Int): DiagrammedExpr[T] =
    new DiagrammedApplyExpr(qualifier, args, value, anchor)

  /**
   * Create select <code>DiagrammedExpr</code> that wraps <code>Select</code> expression.
   *
   * @param qualifier the qualifier of the <code>Apply</code> or <code>TypeApply</code> expression
   * @param value the expression value
   * @param anchor the anchor of the expression
   * @return a select <code>DiagrammedExpr</code>
   */
  def selectExpr[T](qualifier: DiagrammedExpr[_], value: T, anchor: Int): DiagrammedExpr[T] =
    new DiagrammedSelectExpr(qualifier, value, anchor)
}

private[scalactic] class DiagrammedSimpleExpr[T](val value: T, val anchor: Int) extends DiagrammedExpr[T] {
  def anchorValues = List(AnchorValue(anchor, value))
}

private[scalactic] class DiagrammedApplyExpr[T](qualifier: DiagrammedExpr[_], args: List[DiagrammedExpr[_]], val value: T, val anchor: Int) extends DiagrammedExpr[T] {

  def anchorValues = {
    val quantifierAnchorValues = eliminateDuplicates(qualifier.anchorValues)

    val argsAnchorValues =
      args.flatMap { arg =>
        eliminateDuplicates(arg.anchorValues)
      }

    quantifierAnchorValues.toList ::: AnchorValue(anchor, value) :: argsAnchorValues.filter(_.anchor >= 0)
  }
}

private[scalactic] class DiagrammedSelectExpr[T](qualifier: DiagrammedExpr[_], val value: T, val anchor: Int) extends DiagrammedExpr[T] {
  def anchorValues = {
    val quantifierAnchorValues = eliminateDuplicates(qualifier.anchorValues)

    quantifierAnchorValues.toList ::: List(AnchorValue(anchor, value))
  }
}