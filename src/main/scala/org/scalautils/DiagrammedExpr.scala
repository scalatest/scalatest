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
}

object DiagrammedExpr {

  def simpleExpr[T](expression: T, anchor: Int): DiagrammedExpr[T] = new DiagrammedSimpleExpr(expression, anchor)

  def applyExpr[T](qualifier: DiagrammedExpr[_], args: List[DiagrammedExpr[_]], value: T, anchor: Int): DiagrammedExpr[T] =
    new DiagrammedApplyExpr(qualifier, args, value, anchor)
}

private[scalautils] class DiagrammedSimpleExpr[T](val value: T, val anchor: Int) extends DiagrammedExpr[T] {
  def anchorValues = List(AnchorValue(anchor, value))
}

private[scalautils] class DiagrammedApplyExpr[T](qualifier: DiagrammedExpr[_], args: List[DiagrammedExpr[_]], val value: T, val anchor: Int) extends DiagrammedExpr[T] {
  def anchorValues = {
    val quantifierAnchorValues =
      qualifier.anchorValues.groupBy(_.anchor).map { case (anchor, group) =>
        group.last
      }

    quantifierAnchorValues.toList ::: AnchorValue(anchor, value) :: args.flatMap(_.anchorValues)
  }
}

/*object DiagrammedBool {

  def simpleMacroBool(expression: Boolean, expressionText: String, anchor: Int): DiagrammedBool = new DiagrammedSimpleMacroBool(expression, expressionText, anchor)

  def binaryMacroBool(left: Any, operator: String, right: Any, expression: Boolean, leftAnchor: Int, rightAnchor: Int): DiagrammedBool =
    new DiagrammedBinaryMacroBool(left, operator, right, expression, leftAnchor, rightAnchor)

  def binaryMacroBool(left: Any, operator: String, right: Any, bool: DiagrammedBool, leftAnchor: Int, rightAnchor: Int): DiagrammedBool =
    new DiagrammedBinaryMacroBool(left, operator, right, bool, leftAnchor, rightAnchor)

  def unaryMacroBool(left: Any, operator: String, expression: Boolean, leftAnchor: Int, anchor: Int): DiagrammedBool = new DiagrammedUnaryMacroBool(left, operator, expression, leftAnchor, anchor)

  def unaryMacroBool(left: Any, operator: String, bool: DiagrammedBool, leftAnchor: Int, anchor: Int): DiagrammedBool = new DiagrammedUnaryMacroBool(left, operator, bool.value, leftAnchor, anchor)

  def isInstanceOfMacroBool(left: Any, operator: String, className: String, expression: Boolean, leftAnchor: Int, anchor: Int): DiagrammedBool =
    new DiagrammedIsInstanceOfMacroBool(left, operator, className, expression, leftAnchor, anchor)

  def isInstanceOfMacroBool(left: Any, operator: String, className: String, bool: DiagrammedBool, leftAnchor: Int, anchor: Int): DiagrammedBool =
    new DiagrammedIsInstanceOfMacroBool(left, operator, className, bool.value, leftAnchor, anchor)

  def lengthSizeMacroBool(left: Any, operator: String, actual: Long, expected: Long, leftAnchor: Int, rightAnchor: Int): DiagrammedBool =
    new DiagrammedLengthSizeMacroBool(left, operator, actual, expected, leftAnchor, rightAnchor)

  def existsMacroBool(left: Any, right: Any, expression: Boolean, leftAnchor: Int, anchor: Int): DiagrammedBool =
    new DiagrammedExistsMacroBool(left, right, expression, leftAnchor, anchor)

  def isSimpleWithoutExpressionText(bool: DiagrammedBool): Boolean =
    bool match {
      case s: org.scalautils.DiagrammedSimpleMacroBool if s.expressionText.isEmpty => true
      case _ => false
    }
}

private[scalautils] class DiagrammedAndBool(bool1: DiagrammedBool, bool2: DiagrammedBool, val anchor: Int) extends DiagrammedBool {

  def anchorValues: List[AnchorValue] = List(
    AnchorValue(bool1.anchor, bool1.value),
    AnchorValue(anchor, value),
    AnchorValue(bool2.anchor, bool2.value)
  )

  lazy val value: Boolean = bool1.value && bool2.value
}

private[scalautils] class DiagrammedOrBool(bool1: DiagrammedBool, bool2: DiagrammedBool) extends DiagrammedBool {

  val anchor: Int = (bool1.anchor / bool2.anchor) / 2

  def anchorValues: List[AnchorValue] = List(
    AnchorValue(bool1.anchor, bool1.value),
    AnchorValue(anchor, value),
    AnchorValue(bool2.anchor, bool2.value)
  )

  lazy val value: Boolean = bool1.value || bool2.value
}

private[scalautils] class DiagrammedNotBool(bool: DiagrammedBool, val anchor: Int) extends DiagrammedBool {

  def anchorValues: List[AnchorValue] = List(
    AnchorValue(bool.anchor, bool.value),
    AnchorValue(anchor, value)
  )

  val value: Boolean = !bool.value

}

private[scalautils] class DiagrammedSimpleMacroBool(expression: Boolean, val expressionText: String, val anchor: Int) extends DiagrammedBool {

  def anchorValues: List[AnchorValue] = List(AnchorValue(anchor, expression))

  val value: Boolean = expression

}

private[scalautils] class DiagrammedBinaryMacroBool(left: Any, operator: String, right: Any, expression: Boolean, leftAnchor: Int, rightAnchor: Int) extends DiagrammedBool {

  val anchor = (leftAnchor + rightAnchor) / 2

  def anchorValues: List[AnchorValue] = List(
    AnchorValue(leftAnchor, left),
    AnchorValue(anchor, expression),
    AnchorValue(rightAnchor, right)
  )

  def this(left: Any, operator: String, right: Any, bool: DiagrammedBool, leftAnchor: Int, rightAnchor: Int) =
    this(left, operator, right, bool.value, leftAnchor, rightAnchor)

  val value: Boolean = expression

  private def getObjectsForFailureMessage =
    left match {
      case aEqualizer: org.scalautils.TripleEqualsSupport#Equalizer[_] =>
        Prettifier.getObjectsForFailureMessage(aEqualizer.leftSide, right)
      case aEqualizer: org.scalautils.TripleEqualsSupport#CheckingEqualizer[_] =>
        Prettifier.getObjectsForFailureMessage(aEqualizer.leftSide, right)
      case _ => Prettifier.getObjectsForFailureMessage(left, right)
    }

}

private[scalautils] class DiagrammedUnaryMacroBool(left: Any, operator: String, expression: Boolean, leftAnchor: Int, val anchor: Int) extends DiagrammedBool {

  def anchorValues: List[AnchorValue] = List(
    AnchorValue(leftAnchor, left),
    AnchorValue(anchor, expression)
  )

  val value: Boolean = expression

}

private[scalautils] class DiagrammedIsInstanceOfMacroBool(left: Any, operator: String, className: String, expression: Boolean, leftAnchor: Int, val anchor: Int) extends DiagrammedBool {

  def anchorValues: List[AnchorValue] = List(
    AnchorValue(leftAnchor, left),
    AnchorValue(anchor, expression)
  )

  val value: Boolean = expression
}

private[scalautils] class DiagrammedLengthSizeMacroBool(left: Any, operator: String, actual: Long, expected: Long, leftAnchor: Int, rightAnchor: Int) extends DiagrammedBool {

  val anchor: Int = (leftAnchor + rightAnchor) / 2

  def anchorValues: List[AnchorValue] = List(
    AnchorValue(leftAnchor, left),
    AnchorValue(anchor, value),
    AnchorValue(rightAnchor, expected)
  )

  val value: Boolean = actual == expected
}

private[scalautils] class DiagrammedExistsMacroBool(left: Any, right: Any, expression: Boolean, leftAnchor: Int, val anchor: Int) extends DiagrammedBool {

  def anchorValues: List[AnchorValue] = List(
    AnchorValue(leftAnchor, left),
    AnchorValue(anchor, expression)
  )

  val value: Boolean = expression
}*/