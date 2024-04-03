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

// SKIP-SCALATESTNATIVE-START
import org.scalactic.source.ObjectMeta
// SKIP-SCALATESTNATIVE-END
import scala.annotation.tailrec

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

  def prettifierLimit(prettifier: Prettifier): Option[Int] = 
    prettifier match {
      case tp: TruncatingPrettifier => Some(tp.sizeLimit.value)
      case _ => None
    }
}

private[scalactic] trait StringDiffer extends Differ {

  private[scalactic] def escapedString(s: String): String = {
    def escapedChar(c: Char): String = (c: @scala.annotation.switch) match {
      case '\b' => raw"\b"
      case '\t' => raw"\t"
      case '\n' => raw"\n"
      case '\f' => raw"\f"
      case '\r' => raw"\r"
      case '"'  => "\\\"" // raw"\"" Scala 2.11 compatible
      case '\'' => raw"\'"
      case '\\' => raw"\\"
      case _    => if (c.isControl) "\\u%04X".format(c.toInt) else String.valueOf(c)
    }
    if (s.exists(c => c.isControl || c == '\\')) s.flatMap(escapedChar)
    else s
  }

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
          Some(prettifier(escapedString(aRes)) + " -> " + prettifier(escapedString(bRes)))
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

  @tailrec
  private def recurDiff(prettifier: Prettifier, aSeq: scala.collection.GenSeq[_], bSeq: scala.collection.GenSeq[_], limit: Int, idx: Int = 0, result: Vector[String] = Vector.empty): Vector[String] = 
    if (result.length <= limit)
      (aSeq.headOption, bSeq.headOption) match {
        case (Some(leftEl), Some(rightEl)) => 
          recurDiff(prettifier, aSeq.tail, bSeq.tail, limit, idx + 1, 
                    result ++ (if (leftEl != rightEl) Vector(idx + ": " + prettifier(leftEl) + " -> " + prettifier(rightEl)) else Vector.empty))
        case (Some(leftEl), None) =>
          recurDiff(prettifier, aSeq.tail, bSeq, limit, idx + 1, result :+ (idx + ": " + prettifier(leftEl) + " -> "))
        case (None, Some(rightEl)) =>
          recurDiff(prettifier, aSeq, bSeq.tail, limit, idx + 1, result :+ (idx + ": -> " + prettifier(rightEl)))
        case (None, None) =>  
          result                    
      }
    else result.dropRight(1) :+ "..."

  def difference(a: Any, b: Any, prettifier: Prettifier): PrettyPair = {
    (a, b) match {
      case (aSeq: scala.collection.GenSeq[_], bSeq: scala.collection.GenSeq[_]) =>
        val limit = Differ.prettifierLimit(prettifier).getOrElse(math.max(aSeq.length, bSeq.length))
        val diffs = recurDiff(prettifier, aSeq, bSeq, limit)
        val shortName = Differ.simpleClassName(aSeq)
        if (diffs.isEmpty)
          PrettyPair(prettifier(a), prettifier(b), None)
        else
          PrettyPair(prettifier(a), prettifier(b), Some(shortName + "(" + diffs.mkString(", ") + ")"))

      case _ => PrettyPair(prettifier(a), prettifier(b), None)
    }
  }

}

private[scalactic] object GenSeqDiffer extends GenSeqDiffer

private[scalactic] class GenSetDiffer extends Differ {

  def difference(a: Any, b: Any, prettifier: Prettifier): PrettyPair = {
    (a, b) match {
      case (aSet: scala.collection.GenSet[_], bSet: scala.collection.GenSet[_]) =>
        val missingInRight = aSet.toList.diff(bSet.toList).map(prettifier.apply)
        val missingInLeft = bSet.toList.diff(aSet.toList).map(prettifier.apply)

        val limit = Differ.prettifierLimit(prettifier).getOrElse(math.max(aSet.size, bSet.size))  

        val limitedMissingInRight: List[String] = if (missingInRight.length > limit) missingInRight.take(limit) :+ "..." else missingInRight
        val limitedMissingInLeft: List[String] = if (missingInLeft.length > limit) missingInLeft.take(limit) :+ "..." else missingInLeft

        val shortName = Differ.simpleClassName(aSet)
        if (missingInLeft.isEmpty && missingInRight.isEmpty)
          PrettyPair(prettifier(a), prettifier(b), None)
        else {
          val diffList =
            List(
              if (limitedMissingInLeft.isEmpty) "" else "missingInLeft: [" + limitedMissingInLeft.mkString(", ") + "]",
              if (limitedMissingInRight.isEmpty) "" else "missingInRight: [" + limitedMissingInRight.mkString(", ") + "]"
            ).filter(_.nonEmpty)
          PrettyPair(prettifier(a), prettifier(b), Some(shortName + "(" + diffList.mkString(", ") + ")"))
        }

      case _ => PrettyPair(prettifier(a), prettifier(b), None)
    }
  }

}

private[scalactic] object GenSetDiffer extends GenSetDiffer

private[scalactic] class GenMapDiffer[K, V] extends Differ {

  @tailrec
  private def recurDiff[AK, AV, BK, BV](prettifier: Prettifier, aMap: scala.collection.GenMap[AK, AV], bMap: scala.collection.GenMap[BK, BV], limit: Int, result: Vector[String] = Vector.empty): Vector[String] = 
    if (result.length <= limit)
      aMap.headOption match {
        case Some((aKey, aValue)) => 
          // This gives a compiler error due to k being a wildcard type:
          // val rightValue = bMap(k)
          // Not sure why aMap(k) doesn't give the same error, but regardless, fixing it
          // by pulling the value out for the key using a == comparison, which for now
          // works because of universal equality, then assuming the value exists, just
          // as bMap(k) previously was assuming.
          bMap.collect { case (nextK, nextV) if nextK == aKey => nextV }.headOption match {
            case Some(bValue) =>
              recurDiff(prettifier, aMap.tail, bMap.filter(_._1 != aKey), limit, 
                    result ++ (if (aValue != bValue) Vector(prettifier(aKey) + ": " + prettifier(aValue) + " -> " + prettifier(bValue)) else Vector.empty))
            case None => 
              recurDiff(prettifier, aMap.tail, bMap, limit, result :+ (prettifier(aKey) + ": " + prettifier(aValue) + " -> "))
          }
        case None => 
          result ++ bMap.map { case (bKey, bValue) =>
            prettifier(bKey) + ": -> " + prettifier(bValue)
          }
      }
    else result.dropRight(1) :+ "..."

  def difference(a: Any, b: Any, prettifier: Prettifier): PrettyPair =
    (a, b) match {
      case (aMap: scala.collection.GenMap[_, _], bMap: scala.collection.GenMap[_, _]) =>
        val limit = Differ.prettifierLimit(prettifier).getOrElse(math.max(aMap.size, bMap.size))      
        val diffs = recurDiff(prettifier, aMap, bMap, limit) 

        val shortName = Differ.simpleClassName(aMap)
        if (diffs.isEmpty)
          PrettyPair(prettifier(a), prettifier(b), None)
        else
          PrettyPair(prettifier(a), prettifier(b), Some(shortName + "(" + diffs.mkString(", ") + ")"))

      case _ =>
        PrettyPair(prettifier(a), prettifier(b), None)
    }

}

private[scalactic] object GenMapDiffer extends GenMapDiffer[Any, Any]

// SKIP-SCALATESTNATIVE-START
private[scalactic] trait ObjectDiffer extends Differ {

  def difference(a: Any, b: Any, prettifier: Prettifier): PrettyPair =  diffImpl(a, b, prettifier, Set.empty)

  def diffImpl(a: Any, b: Any, prettifier: Prettifier, processed: Set[Any]): PrettyPair = {
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
              if (!processed.exists(e => e == leftValue || e == rightValue)) {
                val nestedPair = AnyDiffer.diffImpl(leftValue, rightValue, prettifier, processed ++ Set(leftValue, rightValue))
                nestedPair.analysis match {
                  case Some(analysis) =>
                    Some(name + ": " + analysis)

                  case None =>
                    Some(name + ": " + leftValue + " -> " + rightValue)
                }
              }
              else 
                Some("Cyclic value detected, name: " + leftValue.toString + " -> " + rightValue.toString)
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
// SKIP-SCALATESTNATIVE-END

private[scalactic] class AnyDiffer extends Differ {

  def difference(a: Any, b: Any, prettifier: Prettifier): PrettyPair = diffImpl(a, b, prettifier, Set.empty)

  def diffImpl(a: Any, b: Any, prettifier: Prettifier, processed: Set[Any]): PrettyPair = {

    (a, b) match {
      case (s1: String, s2: String) => StringDiffer.difference(s1, s2, prettifier)
      case (s1: scala.collection.GenMap[_, _], s2: scala.collection.GenMap[_, _]) => GenMapDiffer.difference(s1, s2, prettifier)
      case (s1: scala.collection.GenSeq[_], s2: scala.collection.GenSeq[_]) => GenSeqDiffer.difference(s1, s2, prettifier)
      case (s1: scala.collection.GenSet[_], s2: scala.collection.GenSet[_]) => GenSetDiffer.difference(s1, s2, prettifier)
      // SKIP-SCALATESTNATIVE-START
      case (s1: Product, s2: Product) => ObjectDiffer.diffImpl(s1, s2, prettifier, processed)
      // SKIP-SCALATESTNATIVE-END
      case _ => PrettyPair(prettifier(a), prettifier(b), None)
    }
  }

}

private[scalactic] object AnyDiffer extends AnyDiffer

private[scalactic] class EscapingStringDiffer extends Differ {
  private def escapeString(a: Any): Any = a match {
    case s: String => StringDiffer.escapedString(s)
    case _ => a
  }
  def difference(a: Any, b: Any, prettifier: Prettifier): PrettyPair = 
    (a, b) match {
      case (s1: scala.collection.GenMap[_, _], s2: String) => // TODO: Should not need prettifying the arguments here, should do in analysis.
        PrettyPair(prettifier(s1.map { case (k, v) => (escapeString(k), escapeString(v)) }), prettifier(escapeString(s2)), None)
      case (s1: scala.collection.GenSeq[_], s2: String) => 
        val s2Escapted = escapeString(s2)
        val analysis = 
          if (s2 != s2Escapted)
            Some("RHS contains characters that might cause problem: " + s2Escapted)
          else 
            s1.find { e => 
              val eEscaped = escapeString(e)
              println("e: " + e + ", eEscaped: " + eEscaped)
              e != eEscaped
            }.map { e => 
              "LHS contains at least one string with characters that might cause problem: " + escapeString(e)
            }  
        PrettyPair(prettifier(s1), prettifier(s2), analysis)
      case (s1: scala.collection.GenSet[_], s2: String) => 
        PrettyPair(prettifier(s1), prettifier(s2), None)
      case _ => AnyDiffer.difference(a, b, prettifier)
    }
}