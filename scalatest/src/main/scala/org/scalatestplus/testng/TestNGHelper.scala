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
package org.scalatestplus.testng

import java.lang.reflect.{Method, Modifier}
import org.scalatest._
import org.scalatest.events.{Formatter, IndentedText}
import org.scalatest.exceptions.NotAllowedException

import scala.collection.immutable.TreeSet
import scala.reflect.NameTransformer.decode

private[testng] object TestNGHelper {

  def formatterForSuiteAborted(suite: Suite, message: String): Option[Formatter] = {
    val suiteClass = suite.getClass
    val actualSuiteName =
      if (suiteClass.getName == "org.scalatest.DeferredAbortedSuite")
        suiteClass.getDeclaredField("suiteClassName").get(suite).asInstanceOf[String]
      else
        suiteClass.getName
    Some(IndentedText(actualSuiteName, message, 0))
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

  def isTestMethodGoodies(m: Method) = {

    val isInstanceMethod = !Modifier.isStatic(m.getModifiers())

    // name must have at least 4 chars (minimum is "test")
    val simpleName = m.getName
    val firstFour = if (simpleName.length >= 4) simpleName.substring(0, 4) else ""

    val paramTypes = m.getParameterTypes
    val hasNoParams = paramTypes.length == 0

    // Discover testNames(Informer) because if we didn't it might be confusing when someone
    // actually wrote a testNames(Informer) method and it was silently ignored.
    val isTestNames = simpleName == "testNames"
    val isTestTags = simpleName == "testTags"
    val isTestDataFor = (simpleName == "testDataFor" && paramTypes.length == 2 && classOf[String].isAssignableFrom(paramTypes(0)) && classOf[ConfigMap].isAssignableFrom(paramTypes(1))) ||
      (simpleName == "testDataFor$default$2" && paramTypes.length == 0)

    (isInstanceMethod, simpleName, firstFour, paramTypes, hasNoParams, isTestNames, isTestTags, isTestDataFor)
  }

  def takesInformer(m: Method) = {
    val paramTypes = m.getParameterTypes
    paramTypes.length == 1 && classOf[Informer].isAssignableFrom(paramTypes(0))
  }

  val InformerInParens = "(Informer)"
  val FixtureAndInformerInParens = "(FixtureParam, Informer)"
  val FixtureInParens = "(FixtureParam)"

  object EncodedOrdering extends Ordering[String] {
    def compare(x: String, y: String): Int = {
      decode(x) compareTo decode(y)
    }
  }

  def yeOldeTestNames(theSuite: Suite): Set[String] = {

    def isTestMethod(m: Method) = {

      // Factored out to share code with fixture.Suite.testNames
      val (isInstanceMethod, simpleName, firstFour, paramTypes, hasNoParams, isTestNames, isTestTags, isTestDataFor) = isTestMethodGoodies(m)

      isInstanceMethod && (firstFour == "test") && !isTestDataFor && ((hasNoParams && !isTestNames && !isTestTags) || takesInformer(m))
    }

    val testNameArray =
      for (m <- theSuite.getClass.getMethods; if isTestMethod(m))
        yield if (takesInformer(m)) m.getName + InformerInParens else m.getName

    val result = TreeSet.empty[String](EncodedOrdering) ++ testNameArray
    if (result.size != testNameArray.length) {
      throw new NotAllowedException("Howdy", 0)
    }
    result
  }

  def testMethodTakesAFixtureAndInformer(testName: String) = testName.endsWith(FixtureAndInformerInParens)

  def testMethodTakesAnInformer(testName: String): Boolean = testName.endsWith(InformerInParens)

  def testMethodTakesAFixture(testName: String) = testName.endsWith(FixtureInParens)

  def simpleNameForTest(testName: String) =
    if (testName.endsWith(FixtureAndInformerInParens))
      testName.substring(0, testName.length - FixtureAndInformerInParens.length)
    else if (testName.endsWith(FixtureInParens))
      testName.substring(0, testName.length - FixtureInParens.length)
    else if (testName.endsWith(InformerInParens))
      testName.substring(0, testName.length - InformerInParens.length)
    else
      testName

  def getMethodForTestName(theSuite: org.scalatest.Suite, testName: String): Method = {
    val candidateMethods = theSuite.getClass.getMethods.filter(_.getName == simpleNameForTest(testName))
    val found =
      if (testMethodTakesAFixtureAndInformer(testName))
        candidateMethods.find(
          candidateMethod => {
            val paramTypes = candidateMethod.getParameterTypes
            paramTypes.length == 2 && paramTypes(1) == classOf[Informer]
          }
        )
      else if (testMethodTakesAnInformer(testName))
        candidateMethods.find(
          candidateMethod => {
            val paramTypes = candidateMethod.getParameterTypes
            paramTypes.length == 1 && paramTypes(0) == classOf[Informer]
          }
        )
      else if (testMethodTakesAFixture(testName))
        candidateMethods.find(
          candidateMethod => {
            val paramTypes = candidateMethod.getParameterTypes
            paramTypes.length == 1
          }
        )
      else
        candidateMethods.find(_.getParameterTypes.length == 0)

    found match {
      case Some(method) => method
      case None =>
        throw new IllegalArgumentException(Resources.testNotFound(testName))
    }
  }

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

}