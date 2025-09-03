/*
 * Copyright 2001-2025 Artima, Inc.
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
import org.scalactic.source.Position
import org.scalactic.Prettifier

// //import org.scalatest.words.{FactResultOfAnTypeInvocation, FactResultOfATypeInvocation}
// import org.scalactic.Prettifier
// import org.scalatest.{UnquotedString, Resources, Suite, FailureMessages, Assertions}

import scala.quoted.*

object TypeMatcherMacro {

  //   // Check that no type parameter is specified, if any does, give a friendly compiler warning.
  def checkTypeParameter(using Quotes)(tree: quotes.reflect.Term, methodName: String): Unit = {
    import quotes.reflect.*

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
  def shouldBeATypeImpl[T](leftSideValue: Expr[T], aType: Expr[ResultOfATypeInvocation[_]], pos: Expr[Position], prettifier: Expr[Prettifier])(using Quotes, Type[T]): Expr[org.scalatest.Assertion] = {
    import quotes.reflect.*
    checkTypeParameter(aType.asTerm, "a")
    '{
      org.scalatest.matchers.TypeMatcherHelper.assertAType($leftSideValue, $aType, $prettifier, $pos)
    }
  }

  // Do checking on type parameter and generate AST to call TypeMatcherHelper.checkAType, used by 'shouldBe an [type]' syntax
  def shouldBeAnTypeImpl[T](leftSideValue: Expr[T], anType: Expr[ResultOfAnTypeInvocation[_]], pos: Expr[Position], prettifier: Expr[Prettifier])(using Quotes, Type[T]): Expr[org.scalatest.Assertion] = {
    import quotes.reflect.*
    checkTypeParameter(anType.asTerm, "an")
    '{
      org.scalatest.matchers.TypeMatcherHelper.assertAnType($leftSideValue, $anType, $prettifier, $pos)
    }
  }

}