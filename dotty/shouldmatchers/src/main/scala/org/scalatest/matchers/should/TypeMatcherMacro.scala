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
package org.scalatest.matchers.should

import org.scalatest.matchers.dsl.{ResultOfAnTypeInvocation, MatcherWords, ResultOfATypeInvocation, ResultOfNotWordForAny}

// //import org.scalatest.words.{FactResultOfAnTypeInvocation, FactResultOfATypeInvocation}
// import org.scalactic.Prettifier
// import org.scalatest.{UnquotedString, Resources, Suite, FailureMessages, Assertions}

import scala.quoted._
import scala.tasty._

object TypeMatcherMacro {

  //   // Check that no type parameter is specified, if any does, give a friendly compiler warning.
  def checkTypeParameter(qctx: QuoteContext)(tree: qctx.tasty.Term, methodName: String): Unit = {
    import qctx.tasty._

    // TODO#Macros: Select lack unapply
    /*
    tree.underlyingArgument match {
      case Apply(TypeApply(Select(_,methodNameTermName), typeList: List[TypeTree]), _)
      if methodNameTermName.decoded == methodName =>
        // Got a type list, let's go through it
        typeList.foreach {
          case Applied(tpt, args) => // type is specified, let's give warning.
            // TODO#Macros: Blocked by error reporting API
            // context.warning(args(0).pos, "Type parameter should not be specified because it will be erased at runtime, please use _ instead.  Note that in future version of ScalaTest this will give a compiler error.")

          case _ => // otherwise don't do anything
        }
      case _ => // otherwise don't do anything
    } */

  }

  // Do checking on type parameter and generate AST to call TypeMatcherHelper.checkAType, used by 'shouldBe a [type]' syntax
  def shouldBeATypeImpl(self: Expr[org.scalatest.matchers.should.Matchers#AnyShouldWrapper[_]], aType: Expr[ResultOfATypeInvocation[_]])(implicit qctx: QuoteContext): Expr[org.scalatest.Assertion] = {
    import qctx.tasty._
    checkTypeParameter(qctx)(aType.unseal, "a")
    '{
      org.scalatest.matchers.TypeMatcherHelper.assertAType(($self).leftSideValue, $aType, ($self).prettifier, ($self).pos)
    }
  }

  // Do checking on type parameter and generate AST to call TypeMatcherHelper.checkAType, used by 'shouldBe an [type]' syntax
  def shouldBeAnTypeImpl(self: Expr[org.scalatest.matchers.should.Matchers#AnyShouldWrapper[_]], anType: Expr[ResultOfAnTypeInvocation[_]])(implicit qctx: QuoteContext): Expr[org.scalatest.Assertion] = {
    import qctx.tasty._
    checkTypeParameter(qctx)(anType.unseal, "an")
    '{
      org.scalatest.matchers.TypeMatcherHelper.assertAnType(($self).leftSideValue, $anType, ($self).prettifier, ($self).pos)
    }
  }

}