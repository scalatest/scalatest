/*
 * Copyright 2001-2018 Artima, Inc.
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
package org.scalatest.junit

import java.lang.reflect.Modifier

import org.scalatest.events.IndentedText
import org.scalatest.{Suite, TagAnnotation}

private[scalatest] object JUnitHelper {

  def mergeMap[A, B](ms: List[Map[A, B]])(f: (B, B) => B): Map[A, B] =
    (Map[A, B]() /: (for (m <- ms; kv <- m) yield kv)) { (a, kv) =>
      a + (if (a.contains(kv._1)) kv._1 -> f(a(kv._1), kv._2) else kv)
    }

  def autoTagClassAnnotations(tags: Map[String, Set[String]], theSuite: Suite) = {
    val suiteTags = for {
      a <- theSuite.getClass.getAnnotations
      annotationClass = a.annotationType
      if annotationClass.isAnnotationPresent(classOf[TagAnnotation])
    } yield annotationClass.getName

    val autoTestTags =
      if (suiteTags.size > 0)
        Map() ++ theSuite.testNames.map(tn => (tn, suiteTags.toSet))
      else
        Map.empty[String, Set[String]]

    mergeMap[String, Set[String]](List(tags, autoTestTags)) ( _ ++ _ )
  }

  def getIndentedTextForTest(testText: String, level: Int, includeIcon: Boolean) = {
    val decodedTestText = scala.reflect.NameTransformer.decode(testText)
    val formattedText =
      if (includeIcon) {
        val testSucceededIcon = Resources.testSucceededIconChar
        ("  " * (if (level == 0) 0 else (level - 1))) + Resources.iconPlusShortName(testSucceededIcon, decodedTestText)
      }
      else {
        ("  " * level) + decodedTestText
      }
    IndentedText(formattedText, decodedTestText, level)
  }

  def checkForPublicNoArgConstructor(clazz: java.lang.Class[_]) = {

    try {
      val constructor = clazz.getConstructor(new Array[java.lang.Class[T] forSome { type T }](0): _*)

      Modifier.isPublic(constructor.getModifiers)
    }
    catch {
      case nsme: NoSuchMethodException => false
    }
  }

}
