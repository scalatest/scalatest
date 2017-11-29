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

import org.scalactic.source.ObjectMeta

private[scalactic] trait Differ {

  def difference(a: Any, b: Any, prettifier: Prettifier): PrettyPair

}

private[scalactic] object Differ {

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
}

private[scalactic] trait StringDiffer extends Differ {

  def difference(a: Any, b: Any, prettifier: Prettifier): PrettyPair = {
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

    (a, b) match {
      case (aStr: String, bStr: String) =>
        val (aRes, bRes) = diffStrings(aStr, bStr)
        PrettyPair(
          prettifier(aRes),
          prettifier(bRes),
          Some(prettifier(aRes) + " -> " + prettifier(bRes))
        )

      case _ =>
        PrettyPair(
          prettifier(a),
          prettifier(b),
          None
        )
    }
  }
}

private[scalactic] object StringDiffer extends StringDiffer

private[scalactic] class GenSeqDiffer extends Differ {

  def difference(a: Any, b: Any, prettifier: Prettifier): PrettyPair = {
    (a, b) match {
      case (aSeq: scala.collection.GenSeq[_], bSeq: scala.collection.GenSeq[_]) =>
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
          PrettyPair(prettifier(a), prettifier(b), None)
        else
          PrettyPair(prettifier(a), prettifier(b), Some(shortName + "(" + diffSet.toList.sorted.mkString(", ") + ")"))

      case _ => PrettyPair(prettifier(a), prettifier(b), None)
    }
  }

}

private[scalactic] object GenSeqDiffer extends GenSeqDiffer

private[scalactic] class GenSetDiffer extends Differ {

  def difference(a: Any, b: Any, prettifier: Prettifier): PrettyPair = {
    (a, b) match {
      case (aSet: scala.collection.GenSet[Any], bSet: scala.collection.GenSet[Any]) =>
        val missingInRight = aSet.diff(bSet)
        val missingInLeft = bSet.diff(aSet)

        val shortName = Differ.simpleClassName(aSet)
        if (missingInLeft.isEmpty && missingInRight.isEmpty)
          PrettyPair(prettifier(a), prettifier(b), None)
        else {
          val diffList =
            List(
              if (missingInLeft.isEmpty) "" else "missingInLeft: [" + missingInLeft.mkString(", ") + "]",
              if (missingInRight.isEmpty) "" else "missingInRight: [" + missingInRight.mkString(", ") + "]"
            ).filter(_.nonEmpty)
          PrettyPair(prettifier(a), prettifier(b), Some(shortName + "(" + diffList.mkString(", ") + ")"))
        }

      case _ => PrettyPair(prettifier(a), prettifier(b), None)
    }
  }

}

private[scalactic] object GenSetDiffer extends GenSetDiffer

private[scalactic] class GenMapDiffer[K, V] extends Differ {

  def difference(a: Any, b: Any, prettifier: Prettifier): PrettyPair =
    (a, b) match {
      case (aMap: scala.collection.GenMap[K, V], bMap: scala.collection.GenMap[K, V]) =>
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
          PrettyPair(prettifier(a), prettifier(b), None)
        else
          PrettyPair(prettifier(a), prettifier(b), Some(shortName + "(" + diffSet.toList.sorted.mkString(", ") + ")"))

      case _ =>
        PrettyPair(prettifier(a), prettifier(b), None)
    }

}

private[scalactic] object GenMapDiffer extends GenMapDiffer

private[scalactic] trait ObjectDiffer extends Differ {

  def difference(a: Any, b: Any, prettifier: Prettifier): PrettyPair = {
    import org.scalactic.source.ObjectMeta

    val leftMeta = ObjectMeta(a)
    val rightMeta = ObjectMeta(b)

    val diffSet =
      (leftMeta.fieldNames.flatMap { name =>
        val leftValue = leftMeta.value(name)
        try {
          if (rightMeta.hasField(name)) {
            val rightValue = rightMeta.value(name)
            if (leftValue != rightValue) {
              val nestedPair = AnyDiffer.difference(leftValue, rightValue, prettifier)
              nestedPair.analysis match {
                case Some(analysis) =>
                  Some(name + ": " + analysis)

                case None =>
                  Some(name + ": " + leftValue + " -> " + rightValue)
              }
            }
            else
              None
          }
          else
            Some(name + ": " + leftValue + " -> ")
        }
        catch {
          case iae: IllegalArgumentException =>
            None
        }
      }) ++
        rightMeta.fieldNames.filter(f => !leftMeta.fieldNames.contains(f)).flatMap { name =>
          val rightValue = rightMeta.value(name)
          Some(name + ": -> " + rightValue)
        }


    if (diffSet.isEmpty)
      PrettyPair(prettifier(a), prettifier(b), None)
    else {
      val shortName = Differ.simpleClassName(a)
      PrettyPair(prettifier(a), prettifier(b), Some(shortName + "(" + diffSet.toList.sorted.mkString(", ") + ")"))
    }

  }

}

private[scalactic] object ObjectDiffer extends ObjectDiffer

private[scalactic] class AnyDiffer extends Differ {

  def difference(a: Any, b: Any, prettifier: Prettifier): PrettyPair = {

    (a, b) match {
      case (s1: String, s2: String) => StringDiffer.difference(s1, s2, prettifier)
      case (s1: scala.collection.GenMap[Any, Any], s2: scala.collection.GenMap[Any, Any]) => GenMapDiffer.difference(s1, s2, prettifier)
      case (s1: scala.collection.GenSeq[_], s2: scala.collection.GenSeq[_]) => GenSeqDiffer.difference(s1, s2, prettifier)
      case (s1: scala.collection.GenSet[Any], s2: scala.collection.GenSet[Any]) => GenSetDiffer.difference(s1, s2, prettifier)
      case (s1: Product, s2: Product) => ObjectDiffer.difference(s1, s2, prettifier)
      case _ => PrettyPair(prettifier(a), prettifier(b), None)
    }
  }

}

private[scalactic] object AnyDiffer extends AnyDiffer