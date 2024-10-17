/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest.matchers

import org.scalatest.matchers.dsl.{ResultOfAnTypeInvocation, MatcherWords, ResultOfATypeInvocation, ResultOfNotWordForAny}

// //import org.scalatest.words.{FactResultOfAnTypeInvocation, FactResultOfATypeInvocation}
// import org.scalactic.Prettifier
// import org.scalatest.{UnquotedString, Resources, Suite, FailureMessages, Assertions}

import scala.quoted._

object TypeMatcherMacro {

//   // Check that no type parameter is specified, if any does, give a friendly compiler warning.
  def checkTypeParameter(using Quotes)(tree: quotes.reflect.Term, methodName: String): Unit = {
    import quotes.reflect._

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

  // Do checking on type parameter and generate AST that create a 'a type' matcher
  def aTypeMatcherImpl(aType: Expr[ResultOfATypeInvocation[_]])(using Quotes): Expr[Matcher[Any]] = {
    import quotes.reflect._

    // check type parameter
    checkTypeParameter(aType.asTerm, "a")

    /**
     * Generate AST that does the following code:
     *
     * org.scalatest.matchers.TypeMatcherHelper.aTypeMatcher(aType)
     */
    '{ TypeMatcherHelper.aTypeMatcher($aType) }
  }

  // Do checking on type parameter and generate AST that create a 'an type' matcher
  def anTypeMatcherImpl(anType: Expr[ResultOfAnTypeInvocation[_]])(using Quotes): Expr[Matcher[Any]] = {
    import quotes.reflect._

    // check type parameter
    checkTypeParameter(anType.asTerm, "an")

    /**
     * Generate AST that does the following code:
     *
     * org.scalatest.matchers.TypeMatcherHelper.anTypeMatcher(anType)
     */
    '{ TypeMatcherHelper.anTypeMatcher($anType) }
  }

  // Do checking on type parameter and generate AST that create a negated 'a type' matcher
  def notATypeMatcher(aType: Expr[ResultOfATypeInvocation[_]])(using Quotes): Expr[Matcher[Any]] = {
    import quotes.reflect._

    // check type parameter
    checkTypeParameter(aType.asTerm, "a")

    /**
     * Generate AST that does the following code:
     *
     * org.scalatest.matchers.TypeMatcherHelper.notATypeMatcher(aType)
     */
    '{ TypeMatcherHelper.notATypeMatcher($aType) }
  }

  // Do checking on type parameter and generate AST that create a negated 'an type' matcher
  def notAnTypeMatcher(anType: Expr[ResultOfAnTypeInvocation[_]])(using Quotes): Expr[Matcher[Any]] = {
    import quotes.reflect._

    // check type parameter
    checkTypeParameter(anType.asTerm, "an")

    /**
     * Generate AST that does the following code:
     *
     * org.scalatest.matchers.TypeMatcherHelper.notAnTypeMatcher(anType)
     */
    '{ TypeMatcherHelper.notAnTypeMatcher($anType) }
  }

  // Do checking on type parameter and generate AST that does a 'and not' logical expression matcher for 'a type' matcher.
  def andNotATypeMatcher[T:Type](self: Expr[Matcher[T]#AndNotWord], aType: Expr[ResultOfATypeInvocation[_]])(using Quotes): Expr[Matcher[T]] = {
    // create a negated matcher from notATypeMatcher
    val rhs = notATypeMatcher(aType)

    /**
     * Generate AST for code that call the 'and' method on the Matcher instance (reference through 'owner'):
     *
     * owner.and(rhs)
     */
    '{ ($self).owner.and($rhs) }
  }

  // Do checking on type parameter and generate AST that does a 'and not' logical expression matcher for 'an type' matcher.
  def andNotAnTypeMatcher[T:Type](self: Expr[Matcher[T]#AndNotWord], anType: Expr[ResultOfAnTypeInvocation[_]])(using Quotes): Expr[Matcher[T]] = {

    // create a negated matcher from notAnTypeMatcher
    val rhs = notAnTypeMatcher(anType)

    /**
     * Generate AST for code that call the 'and' method on the Matcher instance (reference through 'owner'):
     *
     * owner.and(rhs)
     */
    '{ ($self).owner.and($rhs) }
  }

  // Do checking on type parameter and generate AST that does a 'or not' logical expression matcher for 'a type' matcher.
  def orNotATypeMatcher[T:Type](self: Expr[Matcher[T]#OrNotWord], aType: Expr[ResultOfATypeInvocation[_]])(using Quotes): Expr[Matcher[T]] = {
    // create a negated matcher from notATypeMatcher
    val rhs = notATypeMatcher(aType)

    /**
     * Generate AST for code that call the 'or' method on the Matcher instance (reference through 'owner'):
     *
     * owner.or(rhs)
     */
    '{ ($self).owner.or($rhs) }
  }

  // Do checking on type parameter and generate AST that does a 'or not' logical expression matcher for 'an type' matcher.
  def orNotAnTypeMatcher[T:Type](self: Expr[Matcher[T]#OrNotWord], anType: Expr[ResultOfAnTypeInvocation[_]])(using Quotes): Expr[Matcher[T]] = {

    // create a negated matcher from notAnTypeMatcher
    val rhs = notAnTypeMatcher(anType)

    /**
     * Generate AST for code that call the 'or' method on the Matcher instance (reference through 'owner'):
     *
     * owner.or(rhs)
     */
    '{ ($self).owner.or($rhs) }
  }

//   /*def expectTypeImpl(context: Context)(tree: context.Tree, beMethodName: String, assertMethodName: String): context.Expr[org.scalatest.Fact] = {
//     import context.universe._

//     // check type parameter
//     checkTypeParameter(context)(tree, "a")

//     /**
//      * Generate AST to call TypeMatcherHelper.checkAType:
//      *
//      * org.scalatest.matchers.TypeMatcherHelper.checkAType(lhs, aType)
//      */
//     val callHelper =
//       context.macroApplication match {
//         case Apply(Select(qualifier, _), _) =>
//           Apply(
//             Select(
//               Select(
//                 Select(
//                   Select(
//                     Ident(TermName("org")),
//                     TermName("scalatest")
//                   ),
//                   TermName("matchers")
//                 ),
//                 TermName("TypeMatcherHelper")
//               ),
//               TermName(assertMethodName)
//             ),
//             List(Select(qualifier, TermName("leftSideValue")), tree)
//           )

//         case _ => context.abort(context.macroApplication.pos, s"This macro should be used with $beMethodName [Type] syntax only.")
//       }

//     context.Expr(callHelper)
//   }

//   def willBeATypeImpl(context: Context)(aType: context.Expr[FactResultOfATypeInvocation[_]]): context.Expr[org.scalatest.Fact] =
//     expectTypeImpl(context)(aType.tree, "willBe a", "expectAType")

//   def willBeAnTypeImpl(context: Context)(anType: context.Expr[FactResultOfAnTypeInvocation[_]]): context.Expr[org.scalatest.Fact] =
//     expectTypeImpl(context)(anType.tree, "willBe an", "expectAnType")*/

  // Do checking on type parameter and generate AST to call TypeMatcherHelper.assertATypeShouldBeTrue
  def assertATypeShouldBeTrueImpl(self: Expr[ResultOfNotWordForAny[_]], aType: Expr[ResultOfATypeInvocation[_]])(using Quotes): Expr[org.scalatest.Assertion] = {
    import quotes.reflect._
    checkTypeParameter(aType.asTerm, "a")
    '{
      TypeMatcherHelper.assertATypeShouldBeTrue(($self).left, $aType, ($self).shouldBeTrue, ($self).prettifier, ($self).pos)
    }
  }

  // Do checking on type parameter and generate AST to call TypeMatcherHelper.assertAnTypeShouldBeTrue
  def assertAnTypeShouldBeTrueImpl(self: Expr[ResultOfNotWordForAny[_]], anType: Expr[ResultOfAnTypeInvocation[_]])(using Quotes): Expr[org.scalatest.Assertion] = {
    import quotes.reflect._
    checkTypeParameter(anType.asTerm, "an")
    '{
      TypeMatcherHelper.assertAnTypeShouldBeTrue(($self).left, $anType, ($self).shouldBeTrue, ($self).prettifier, ($self).pos)
    }
  }

//   /*def expectTypeWillBeTrueImpl(context: Context)(tree: context.Tree, beMethodName: String, expectMethodName: String): context.Expr[org.scalatest.Fact] = {
//     import context.universe._

//     // check type parameter
//     checkTypeParameter(context)(tree, "a")

//     /**
//      * Generate AST to call TypeMatcherHelper.checkATypeShouldBeTrue:
//      *
//      * org.scalatest.matchers.TypeMatcherHelper.checkATypeShouldBeTrue(lhs, aType, shouldBeTrue)
//      */
//     val callHelper =
//       context.macroApplication match {
//         case Apply(Select(qualifier, _), _) =>
//           Apply(
//             Select(
//               Select(
//                 Select(
//                   Select(
//                     Ident(TermName("org")),
//                     TermName("scalatest")
//                   ),
//                   TermName("matchers")
//                 ),
//                 TermName("TypeMatcherHelper")
//               ),
//               TermName(expectMethodName)
//             ),
//             List(Select(qualifier, TermName("left")), tree, Select(qualifier, TermName("shouldBeTrue")))
//           )

//         case _ => context.abort(context.macroApplication.pos, s"This macro should be used with $beMethodName [Type] syntax only.")
//       }

//     context.Expr(callHelper)
//   }

//   // Do checking on type parameter and generate AST to call TypeMatcherHelper.assertATypeShouldBeTrue
//   def expectATypeWillBeTrueImpl(context: Context)(aType: context.Expr[FactResultOfATypeInvocation[_]]): context.Expr[org.scalatest.Fact] =
//     expectTypeWillBeTrueImpl(context)(aType.tree, "will not be a", "expectATypeWillBeTrue")

//   // Do checking on type parameter and generate AST to call TypeMatcherHelper.assertAnTypeShouldBeTrue
//   def expectAnTypeWillBeTrueImpl(context: Context)(anType: context.Expr[FactResultOfAnTypeInvocation[_]]): context.Expr[org.scalatest.Fact] =
//     expectTypeWillBeTrueImpl(context)(anType.tree, "will not be an", "expectAnTypeWillBeTrue")*/

}
