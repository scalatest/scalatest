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
package org.scalatest.matchers

import scala.reflect.macros.Context
import org.scalatest.words.{MatcherWords, ResultOfATypeInvocation}
import org.scalatest.{UnquotedString, Resources, Suite, FailureMessages, Assertions}
import org.scalactic.Prettifier

private[scalatest] object TypeMatcherMacro {

  def checkNoTypeParameter(context: Context)(tree: context.Tree) {

    import context.universe._

    def containsTypeParameter(typeList: List[TypeTree]): Boolean =
      typeList.exists { t =>
        t.original match {
          case AppliedTypeTree(_, _) => true
          case _ => false
        }
      }

    tree match {
      case Apply(
             TypeApply(
               Select(
                 _,
                 methodNameTermName
               ),
               typeList: List[TypeTree]
             ),
             _
           ) if methodNameTermName.decoded == "a" && containsTypeParameter(typeList) =>
        context.abort(context.macroApplication.pos, "Type parameter is not allowed because it will be erased at runtime, please use _ instead.")

      case _ =>
    }

  }

  def aTypeMatcherImpl(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Matcher[Any]] = {

    import context.universe._

    val tree = aType.tree

    checkNoTypeParameter(context)(tree)

    context.Expr(
      Apply(
        Select(
          Select(
            Select(
              Select(
                Ident(newTermName("org")),
                newTermName("scalatest")
              ),
              newTermName("matchers")
            ),
            newTermName("TypeMatcherHelper")
          ),
          newTermName("aTypeMatcher")
        ),
        List(tree)
      )
    )

  }

  def notATypeMatcher(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Matcher[Any]] = {
    import context.universe._

    val tree = aType.tree

    checkNoTypeParameter(context)(tree)

    context.Expr(
      Apply(
        Select(
          Select(
            Select(
              Select(
                Ident(newTermName("org")),
                newTermName("scalatest")
              ),
              newTermName("matchers")
            ),
            newTermName("TypeMatcherHelper")
          ),
          newTermName("notATypeMatcher")
        ),
        List(tree)
      )
    )
  }

  def andNotATypeMatcher(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Matcher[Any]] = {

    import context.universe._

    val rhs = notATypeMatcher(context)(aType)

    context.macroApplication match {
      case Apply(Select(qualifier, _), _) =>
        context.Expr(
          Apply(
            Select(
              Select(
                qualifier,
                "owner"
              ),
              newTermName("and")
            ),
            List(rhs.tree)
          )
        )
      case _ => context.abort(context.macroApplication.pos, "This macro should be used with 'and not' syntax only.")
    }

  }

  def orNotATypeMatcher(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Matcher[Any]] = {

    import context.universe._

    val rhs = notATypeMatcher(context)(aType)

    context.macroApplication match {
      case Apply(Select(qualifier, _), _) =>
        context.Expr(
          Apply(
            Select(
              Select(
                qualifier,
                "owner"
              ),
              newTermName("or")
            ),
            List(rhs.tree)
          )
        )
      case _ => context.abort(context.macroApplication.pos, "This macro should be used with 'or not' syntax only.")
    }

  }

  def shouldBeATypeImpl(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Unit] = {

    import context.universe._

    import context.universe._

    val tree = aType.tree

    checkNoTypeParameter(context)(tree)

    val callHelper =
      context.macroApplication match {
        case Apply(Select(qualifier, _), _) =>
          Apply(
            Select(
              Select(
                Select(
                  Select(
                    Ident(newTermName("org")),
                    newTermName("scalatest")
                  ),
                  newTermName("matchers")
                ),
                newTermName("TypeMatcherHelper")
              ),
              newTermName("checkAType")
            ),
            List(Select(qualifier, newTermName("leftSideValue")), tree)
          )

        case _ => context.abort(context.macroApplication.pos, "This macro should be used with shouldBe a [Type] syntax only.")
      }

    context.Expr(callHelper)

  }

  def mustBeATypeImpl(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Unit] = {

    import context.universe._

    import context.universe._

    val tree = aType.tree

    checkNoTypeParameter(context)(tree)

    val callHelper =
      context.macroApplication match {
        case Apply(Select(qualifier, _), _) =>
          Apply(
            Select(
              Select(
                Select(
                  Select(
                    Ident(newTermName("org")),
                    newTermName("scalatest")
                  ),
                  newTermName("matchers")
                ),
                newTermName("TypeMatcherHelper")
              ),
              newTermName("checkAType")
            ),
            List(Select(qualifier, newTermName("leftSideValue")), tree)
          )

        case _ => context.abort(context.macroApplication.pos, "This macro should be used with mustBe a [Type] syntax only.")
      }

    context.Expr(callHelper)

  }

  def checkATypeShouldBeTrueImpl(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Unit] = {

    import context.universe._

    import context.universe._

    val tree = aType.tree

    checkNoTypeParameter(context)(tree)

    val callHelper =
      context.macroApplication match {
        case Apply(Select(qualifier, _), _) =>
          Apply(
            Select(
              Select(
                Select(
                  Select(
                    Ident(newTermName("org")),
                    newTermName("scalatest")
                  ),
                  newTermName("matchers")
                ),
                newTermName("TypeMatcherHelper")
              ),
              newTermName("checkATypeShouldBeTrue")
            ),
            List(Select(qualifier, newTermName("left")), tree, Select(qualifier, newTermName("shouldBeTrue")))
          )

        case _ => context.abort(context.macroApplication.pos, "This macro should be used with should not be a [Type] syntax only.")
      }

    context.Expr(callHelper)

  }



}