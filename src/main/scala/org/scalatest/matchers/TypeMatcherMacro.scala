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
import org.scalatest.words.{ResultOfAnTypeInvocation, MatcherWords, ResultOfATypeInvocation}
import org.scalatest.{UnquotedString, Resources, Suite, FailureMessages, Assertions}
import org.scalactic.Prettifier

private[scalatest] object TypeMatcherMacro {

  def checkNoTypeParameter(context: Context)(tree: context.Tree, methodName: String) {

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
           ) if methodNameTermName.decoded == methodName && containsTypeParameter(typeList) =>
        context.warning(context.macroApplication.pos, "Type parameter should not be specified because it will be erased at runtime, please use _ instead.  Note that in future version of ScalaTest this will give a compiler error.")

      case _ =>
    }

  }

  def aTypeMatcherImpl(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Matcher[Any]] = {

    import context.universe._

    val tree = aType.tree

    checkNoTypeParameter(context)(tree, "a")

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

  def anTypeMatcherImpl(context: Context)(anType: context.Expr[ResultOfAnTypeInvocation[_]]): context.Expr[Matcher[Any]] = {

    import context.universe._

    val tree = anType.tree

    checkNoTypeParameter(context)(tree, "an")

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
          newTermName("anTypeMatcher")
        ),
        List(tree)
      )
    )

  }

  def notATypeMatcher(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Matcher[Any]] = {
    import context.universe._

    val tree = aType.tree

    checkNoTypeParameter(context)(tree, "a")

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

  def notAnTypeMatcher(context: Context)(anType: context.Expr[ResultOfAnTypeInvocation[_]]): context.Expr[Matcher[Any]] = {
    import context.universe._

    val tree = anType.tree

    checkNoTypeParameter(context)(tree, "an")

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
          newTermName("notAnTypeMatcher")
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

  def andNotAnTypeMatcher(context: Context)(anType: context.Expr[ResultOfAnTypeInvocation[_]]): context.Expr[Matcher[Any]] = {

    import context.universe._

    val rhs = notAnTypeMatcher(context)(anType)

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

  def orNotAnTypeMatcher(context: Context)(anType: context.Expr[ResultOfAnTypeInvocation[_]]): context.Expr[Matcher[Any]] = {

    import context.universe._

    val rhs = notAnTypeMatcher(context)(anType)

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

    checkNoTypeParameter(context)(tree, "a")

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

    checkNoTypeParameter(context)(tree, "a")

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

  def shouldBeAnTypeImpl(context: Context)(anType: context.Expr[ResultOfAnTypeInvocation[_]]): context.Expr[Unit] = {

    import context.universe._

    import context.universe._

    val tree = anType.tree

    checkNoTypeParameter(context)(tree, "an")

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
              newTermName("checkAnType")
            ),
            List(Select(qualifier, newTermName("leftSideValue")), tree)
          )

        case _ => context.abort(context.macroApplication.pos, "This macro should be used with shouldBe an [Type] syntax only.")
      }

    context.Expr(callHelper)

  }

  def mustBeAnTypeImpl(context: Context)(anType: context.Expr[ResultOfAnTypeInvocation[_]]): context.Expr[Unit] = {

    import context.universe._

    import context.universe._

    val tree = anType.tree

    checkNoTypeParameter(context)(tree, "an")

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
              newTermName("checkAnType")
            ),
            List(Select(qualifier, newTermName("leftSideValue")), tree)
          )

        case _ => context.abort(context.macroApplication.pos, "This macro should be used with mustBe an [Type] syntax only.")
      }

    context.Expr(callHelper)

  }

  def checkATypeShouldBeTrueImpl(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Unit] = {

    import context.universe._

    import context.universe._

    val tree = aType.tree

    checkNoTypeParameter(context)(tree, "a")

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

  def checkAnTypeShouldBeTrueImpl(context: Context)(anType: context.Expr[ResultOfAnTypeInvocation[_]]): context.Expr[Unit] = {

    import context.universe._

    import context.universe._

    val tree = anType.tree

    checkNoTypeParameter(context)(tree, "an")

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
              newTermName("checkAnTypeShouldBeTrue")
            ),
            List(Select(qualifier, newTermName("left")), tree, Select(qualifier, newTermName("shouldBeTrue")))
          )

        case _ => context.abort(context.macroApplication.pos, "This macro should be used with should not be an [Type] syntax only.")
      }

    context.Expr(callHelper)

  }

}