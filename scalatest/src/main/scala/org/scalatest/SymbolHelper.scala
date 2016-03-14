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
package org.scalatest

import matchers.MatchResult

private[scalatest] class SymbolHelper(FailureMessages: FailureMessages, matchersHelper: MatchersHelper) {

  // SKIP-SCALATESTJS-START
  def matchSymbolToPredicateMethod(left: AnyRef, right: Symbol, hasArticle: Boolean, articleIsA: Boolean, stackDepth: Int = 0): MatchResult = {

    // If 'empty passed, rightNoTick would be "empty"
    val propertyName = right.name

    matchersHelper.accessProperty(left, right, true) match {

      case None =>

        // if propertyName is '>, mangledPropertyName would be "$greater"
        val mangledPropertyName = matchersHelper.transformOperatorChars(propertyName)

        // methodNameToInvoke would also be "empty"
        val methodNameToInvoke = mangledPropertyName

        // methodNameToInvokeWithIs would be "isEmpty"
        val methodNameToInvokeWithIs = "is"+ mangledPropertyName(0).toUpper + mangledPropertyName.substring(1)

        val firstChar = propertyName(0).toLower
        val methodNameStartsWithVowel = firstChar == 'a' || firstChar == 'e' || firstChar == 'i' ||
          firstChar == 'o' || firstChar == 'u'

        throw matchersHelper.newTestFailedException(
          if (methodNameStartsWithVowel)
            FailureMessages.hasNeitherAnOrAnMethod(left, UnquotedString(methodNameToInvoke), UnquotedString(methodNameToInvokeWithIs))
          else
            FailureMessages.hasNeitherAOrAnMethod(left, UnquotedString(methodNameToInvoke), UnquotedString(methodNameToInvokeWithIs)),
          None,
          stackDepth
        )

      case Some(result) =>

        val (wasNot, was) =
          if (hasArticle) {
            if (articleIsA) (Resources.rawWasNotA, Resources.rawWasA) else (Resources.rawWasNotAn, Resources.rawWasAn)
          }
          else (Resources.rawWasNot, Resources.rawWas)

        MatchResult(
          result == true, // Right now I just leave the return value of accessProperty as Any
          wasNot,
          was,
          Vector(left, UnquotedString(propertyName))
        )
    }
  }
  // SKIP-SCALATESTJS-END

}