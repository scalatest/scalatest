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
package org.scalautils

private[org] case class AnchorValue(anchor: Int, value: Any)

trait DiagrammedExpr[T] {
  val anchor: Int
  def anchorValues: List[AnchorValue]
  def value: T

  protected[scalautils] def eliminateDuplicates(anchorValues: List[AnchorValue]): List[AnchorValue] =
    (anchorValues.groupBy(_.anchor).map { case (anchor, group) =>
      group.last
    }).toList
}

object DiagrammedExpr {

  def simpleExpr[T](expression: T, anchor: Int): DiagrammedExpr[T] = new DiagrammedSimpleExpr(expression, anchor)

  def applyExpr[T](qualifier: DiagrammedExpr[_], args: List[DiagrammedExpr[_]], value: T, anchor: Int): DiagrammedExpr[T] =
    new DiagrammedApplyExpr(qualifier, args, value, anchor)

  def selectExpr[T](qualifier: DiagrammedExpr[_], value: T, anchor: Int): DiagrammedExpr[T] =
    new DiagrammedSelectExpr(qualifier, value, anchor)
}

private[scalautils] class DiagrammedSimpleExpr[T](val value: T, val anchor: Int) extends DiagrammedExpr[T] {
  def anchorValues = List(AnchorValue(anchor, value))
}

private[scalautils] class DiagrammedApplyExpr[T](qualifier: DiagrammedExpr[_], args: List[DiagrammedExpr[_]], val value: T, val anchor: Int) extends DiagrammedExpr[T] {

  def anchorValues = {
    val quantifierAnchorValues = eliminateDuplicates(qualifier.anchorValues)

    val argsAnchorValues =
      args.flatMap { arg =>
        eliminateDuplicates(arg.anchorValues)
      }

    quantifierAnchorValues.toList ::: AnchorValue(anchor, value) :: argsAnchorValues.filter(_.anchor >= 0)
  }
}

private[scalautils] class DiagrammedSelectExpr[T](qualifier: DiagrammedExpr[_], val value: T, val anchor: Int) extends DiagrammedExpr[T] {
  def anchorValues = {
    val quantifierAnchorValues = eliminateDuplicates(qualifier.anchorValues)

    quantifierAnchorValues.toList ::: List(AnchorValue(anchor, value))
  }
}