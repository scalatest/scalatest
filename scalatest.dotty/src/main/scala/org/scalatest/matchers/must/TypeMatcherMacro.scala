/*
 * Copyright 2001-2014 Artima, Inc.
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
package org.scalatest.matchers.must

import org.scalatest.matchers.dsl.{ResultOfAnTypeInvocation, MatcherWords, ResultOfATypeInvocation, ResultOfNotWordForAny}

// //import org.scalatest.words.{FactResultOfAnTypeInvocation, FactResultOfATypeInvocation}
// import org.scalactic.Prettifier
// import org.scalatest.{UnquotedString, Resources, Suite, FailureMessages, Assertions}

import scala.quoted._
import scala.tasty._

object TypeMatcherMacro {

  // Do checking on type parameter and generate AST to call TypeMatcherHelper.checkAType, used by 'mustBe a [type]' syntax
  def mustBeATypeImpl(self: Expr[org.scalatest.matchers.must.Matchers#AnyMustWrapper[_]], aType: Expr[ResultOfATypeInvocation[_]])(implicit qctx: QuoteContext): Expr[org.scalatest.Assertion] = {
    import qctx.tasty._
    org.scalatest.matchers.TypeMatcherMacro.checkTypeParameter(qctx)(aType.unseal, "a")
    '{
      org.scalatest.matchers.TypeMatcherHelper.assertAType(($self).leftSideValue, $aType, ($self).prettifier, ($self).pos)
    }
  }

  // Do checking on type parameter and generate AST to call TypeMatcherHelper.checkAnType, used by 'mustBe an [type]' syntax
  def mustBeAnTypeImpl(self: Expr[org.scalatest.matchers.must.Matchers#AnyMustWrapper[_]], anType: Expr[ResultOfAnTypeInvocation[_]])(implicit qctx: QuoteContext): Expr[org.scalatest.Assertion] = {
    import qctx.tasty._
    org.scalatest.matchers.TypeMatcherMacro.checkTypeParameter(qctx)(anType.unseal, "an")
    '{
      org.scalatest.matchers.TypeMatcherHelper.assertAnType(($self).leftSideValue, $anType, ($self).prettifier, ($self).pos)
    }
  }

}