/*
 * Copyright 2001-2016 Artima, Inc.
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

import org.scalactic.source.CaseClassMeta

trait Differ[T] {

  def difference(a: T, b: Any): Difference

}

object Differ {

  def simpleClassName(v: Any): String = {
    val className = v.getClass.getName
    val lastIdxOfDot = className.lastIndexOf(".")
    val shortName =
      if (lastIdxOfDot >= 0)
        className.substring(lastIdxOfDot + 1)
      else
        className
    if (shortName == "$colon$colon")
      "List"
    else if (shortName.startsWith("Set$Set"))
      "Set"
    else if (shortName.startsWith("Map$Map"))
      "Map"
    else if (shortName.startsWith("Tuple"))
      shortName.takeWhile(_ != '$')
    else
      shortName
  }

  implicit def default[T]: Differ[T] = new AnyDiffer[T]

  implicit def stringEquality: Differ[String] = StringDiffer

  import scala.language.higherKinds

  implicit def product[A <: Product]: Differ[Product] = ProductDiffer

  implicit def genSeq[E, SEQ[e] <: scala.collection.GenSeq[e]]: Differ[SEQ[E]] = (new GenSeqDiffer[E]).asInstanceOf[Differ[SEQ[E]]]

  implicit def genSet[E, SET[e] <: scala.collection.GenSet[e]]: Differ[SET[E]] = (new GenSetDiffer[E]).asInstanceOf[Differ[SET[E]]]

  implicit def genMap[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]]: Differ[MAP[K, V]] = (new GenMapDiffer[K, V]).asInstanceOf[Differ[MAP[K, V]]]

}

class AnyDiffer[T] extends Differ[T] {

  def difference(a: T, b: Any): Difference = {

    (a, b) match {
      case (s1: String, s2: String) => StringDiffer.difference(s1, s2)
      case (s1: scala.collection.GenMap[Any, Any], s2: scala.collection.GenMap[Any, Any]) => GenMapDiffer.difference(s1, s2)
      case (s1: scala.collection.GenSeq[_], s2: scala.collection.GenSeq[_]) => GenSeqDiffer.difference(s1, s2)
      case (s1: scala.collection.GenSet[Any], s2: scala.collection.GenSet[Any]) => GenSetDiffer.difference(s1, s2)
      // SKIP-SCALATESTJS-START
      case (s1: Product, s2: Product) if CaseClassMeta.isCaseClass(s1) && CaseClassMeta.isCaseClass(s2) => CaseClassDiffer.difference(s1, s2)
      // SKIP-SCALATESTJS-END
      case (s1: Product, s2: Product) => ProductDiffer.difference(s1, s2)
      case _ =>
        if (a != b)
          new Difference {

            def inlineDiff = Some((a, b))

            def sideBySideDiff = None

            def analysis = None
          }
        else
          Difference.empty
    }
  }

}

object AnyDiffer extends AnyDiffer[Any]

trait StringDiffer extends Differ[String] {

  def difference(a: String, b: Any): Difference =
    new Difference {
      def diffStrings(s: String, t: String): Tuple2[String, String] = {
        def findCommonPrefixLength(s: String, t: String): Int = {
          val max = s.length.min(t.length) // the maximum potential size of the prefix
          var i = 0
          var found = false
          while (i < max & !found) {
            found = (s.charAt(i) != t.charAt(i))
            if (!found)
              i = i + 1
          }
          i
        }
        def findCommonSuffixLength(s: String, t: String): Int = {
          val max = s.length.min(t.length) // the maximum potential size of the suffix
          var i = 0
          var found = false
          while (i < max & !found) {
            found = (s.charAt(s.length - 1 - i) != t.charAt(t.length - 1 - i))
            if (!found)
              i = i + 1
          }
          i
        }
        if (s != t) {
          val commonPrefixLength = findCommonPrefixLength(s, t)
          val commonSuffixLength = findCommonSuffixLength(s.substring(commonPrefixLength), t.substring(commonPrefixLength))
          val prefix = s.substring(0, commonPrefixLength)
          val suffix = if (s.length - commonSuffixLength < 0) "" else s.substring(s.length - commonSuffixLength)
          val sMiddleEnd = s.length - commonSuffixLength
          val tMiddleEnd = t.length - commonSuffixLength
          val sMiddle = s.substring(commonPrefixLength, sMiddleEnd)
          val tMiddle = t.substring(commonPrefixLength, tMiddleEnd)
          val MaxContext = 20
          val shortPrefix = if (commonPrefixLength > MaxContext) "..." + prefix.substring(prefix.length - MaxContext) else prefix
          val shortSuffix = if (commonSuffixLength > MaxContext) suffix.substring(0, MaxContext) + "..." else suffix
          (shortPrefix + "[" + sMiddle + "]" + shortSuffix, shortPrefix + "[" + tMiddle + "]" + shortSuffix)
        }
        else
          (s, t)
      }

      def inlineDiff = {
        (a, b) match {
          case (aStr: String, bStr: String) if aStr != bStr => Some(diffStrings(aStr, bStr))
          case _ => if (a != b) Some((a, b)) else None
        }
      }

      def sideBySideDiff = None

      def analysis = None
    }

}

object StringDiffer extends StringDiffer

// SKIP-SCALATESTJS-START
trait CaseClassDiffer[T] extends Differ[T] {

  def difference(a: T, b: Any): Difference = {
    new Difference {
      def inlineDiff = None

      def sideBySideDiff = None

      def analysis = {
        val leftMeta = CaseClassMeta(a)
        val rightMeta = CaseClassMeta(b)

        val diffSet =
          leftMeta.caseAccessorNames.flatMap { name =>
            val leftValue = leftMeta.value(name)
            try {
              val rightValue = rightMeta.value(name)
              if (leftValue != rightValue) {
                val nestedDiff = AnyDiffer.difference(leftValue, rightValue)
                if (nestedDiff == Difference.empty)
                  Some(name + ": " + leftValue + " -> " + rightValue)
                else {
                  nestedDiff.inlineDiff match {
                    case Some((leftee, rightee)) => Some(name + ": " + leftee + " -> " + rightee)
                    case _ =>
                      nestedDiff.analysis match {
                        case Some(analysis) =>
                          Some(name + ": " + analysis + "")

                        case None => Some(name + ": " + leftValue + " -> " + rightValue)
                      }
                  }
                }

              }
              else
                None
            }
            catch {
              case iae: IllegalArgumentException => None
            }
          }

        if (diffSet.isEmpty)
          None
        else {
          val shortName = Differ.simpleClassName(a)
          Some(shortName + "(" + diffSet.toList.sorted.mkString(", ") + ")")
        }
      }
    }

  }

}

object CaseClassDiffer extends CaseClassDiffer[Any]
// SKIP-SCALATESTJS-END

// interesting to see https://github.com/twitter/diffy/blob/master/src/main/scala/com/twitter/diffy/compare/Difference.scala

class GenSeqDiffer[E] extends Differ[scala.collection.GenSeq[E]] {

  def difference(aSeq: scala.collection.GenSeq[E], b: Any): Difference = {
    new Difference {
      def inlineDiff = None

      def sideBySideDiff = None

      def analysis = {
        b match {
          case bSeq: scala.collection.GenSeq[_] =>
            val diffSet =
              ((0 until aSeq.length) flatMap { i =>
                val leftEl = aSeq(i)
                if (bSeq.isDefinedAt(i)) {
                  val rightEl = bSeq(i)
                  if (leftEl != rightEl)
                    Some(i + ": " + leftEl + " -> " + rightEl)
                  else
                    None
                }
                else
                  Some(i + ": " + leftEl + " -> ")
              }).toSet ++
                ((aSeq.length until bSeq.length) flatMap { i =>
                  Some(i + ": -> " + bSeq(i))
                }).toSet

            val shortName = Differ.simpleClassName(aSeq)
            if (diffSet.isEmpty)
              None
            else
              Some(shortName + "(" + diffSet.toList.sorted.mkString(", ") + ")")

          case _ => None
        }
      }
    }
  }

}

object GenSeqDiffer extends GenSeqDiffer[Any]

class GenSetDiffer[E] extends Differ[scala.collection.GenSet[E]] {

  def difference(aSet: scala.collection.GenSet[E], b: Any): Difference = {
    new Difference {
      def inlineDiff = None

      def sideBySideDiff = None

      def analysis = {
        b match {
          case bSet: scala.collection.GenSet[E] =>
            val missingInRight = aSet.diff(bSet)
            val missingInLeft = bSet.diff(aSet)

            val shortName = Differ.simpleClassName(aSet)
            if (missingInLeft.isEmpty && missingInRight.isEmpty)
              None
            else {
              val diffList =
                List(
                  if (missingInLeft.isEmpty) "" else "missingInLeft: [" + missingInLeft.mkString(", ") + "]",
                  if (missingInRight.isEmpty) "" else "missingInRight: [" + missingInRight.mkString(", ") + "]"
                ).filter(_.nonEmpty)
              Some(shortName + "(" + diffList.mkString(", ") + ")")
            }

          case _ => None
        }
      }
    }
  }

}

object GenSetDiffer extends GenSetDiffer[Any]

class GenMapDiffer[K, V] extends Differ[scala.collection.GenMap[K, V]] {

  def difference(aMap: scala.collection.GenMap[K, V], b: Any): Difference = {
    new Difference {
      def inlineDiff = None

      def sideBySideDiff = None

      def analysis = {
        b match {
          case bMap: scala.collection.GenMap[K, V] =>
            val leftKeySet = aMap.keySet
            val rightKeySet = bMap.keySet
            val missingKeyInRight = leftKeySet.diff(rightKeySet)
            val missingKeyInLeft = rightKeySet.diff(leftKeySet)
            val intersectKeys = leftKeySet.intersect(rightKeySet)
            val diffSet =
              intersectKeys.flatMap { k =>
                val leftValue = aMap(k)
                val rightValue = bMap(k)
                if (leftValue != rightValue)
                  Some(k + ": " + leftValue + " -> " + rightValue)
                else
                  None
              }.toSet ++
                missingKeyInLeft.flatMap { k =>
                  val rightValue = bMap(k)
                  Option(k + ": -> " + rightValue)
                }.toSet ++
                missingKeyInRight.flatMap { k =>
                  val leftValue = aMap(k)
                  Option(k + ": " + leftValue + " -> ")
                }.toSet

            val shortName = Differ.simpleClassName(aMap)
            if (diffSet.isEmpty)
              None
            else
              Some(shortName + "(" + diffSet.toList.sorted.mkString(", ") + ")")

          case _ =>
            None
        }
      }
    }
  }

}

object GenMapDiffer extends GenMapDiffer[Any, Any]

trait ProductDiffer extends Differ[Product] {

  def difference(aProduct: Product, b: Any): Difference = {
    // SKIP-SCALATESTJS-START
    if (aProduct != null && CaseClassMeta.isCaseClass(aProduct) && b != null && CaseClassMeta.isCaseClass(b))
      CaseClassDiffer.difference(aProduct, b)
    else
    // SKIP-SCALATESTJS-END
      new Difference {
        def inlineDiff = None

        def sideBySideDiff = None

        def analysis = {
          b match {
            case bProduct: scala.Product =>
              val diffSet =
                ((0 until aProduct.productArity) flatMap { i =>
                  val leftEl = aProduct.productElement(i)
                  if (bProduct.productArity > i) {
                    val rightEl = bProduct.productElement(i)
                    if (leftEl != rightEl)
                      Some("_" + (i + 1) + ": " + leftEl + " -> " + rightEl)
                    else
                      None
                  }
                  else
                    Some("_" + (i + 1) + ": " + leftEl + " -> ")
                }).toSet ++
                  ((aProduct.productArity until bProduct.productArity) flatMap { i =>
                    Some("_" + (i + 1) + ": -> " + bProduct.productElement(i))
                  }).toSet

              val shortName = Differ.simpleClassName(aProduct)
              if (diffSet.isEmpty)
                None
              else
                Some(shortName + "(" + diffSet.toList.sorted.mkString(", ") + ")")

            case _ => None
          }
        }
      }
  }

}

object ProductDiffer extends ProductDiffer