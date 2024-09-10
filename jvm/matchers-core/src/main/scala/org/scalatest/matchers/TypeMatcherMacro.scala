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

import org.scalatest.matchers.dsl.{ResultOfAnTypeInvocation, MatcherWords, ResultOfATypeInvocation}
import scala.reflect.macros.whitebox.Context

//import org.scalatest.matchers.dsl.{FactResultOfAnTypeInvocation, FactResultOfATypeInvocation}
import org.scalactic.Prettifier
import org.scalatest.{UnquotedString, Resources, Suite, FailureMessages, Assertions}

private[scalatest] object TypeMatcherMacro {

  // Check that no type parameter is specified, if any does, give a friendly compiler warning.
  def checkTypeParameter(context: Context)(tree: context.Tree, methodName: String): Unit = {

    import context.universe._

    tree match {
      case Apply(
             TypeApply(
               Select(
                 _,
                 methodNameTermName
               ),
               typeList
             ),
             _
           ) if methodNameTermName.decodedName.toString == methodName =>
        // Got a type list, let's go through it
        typeList.foreach { case t: TypeTree =>
          t.original match {
            case AppliedTypeTree(tpt, args) => // type is specified, let's give warning.
              context.warning(args(0).pos, "Type parameter should not be specified because it will be erased at runtime, please use _ instead.  Note that in future version of ScalaTest this will give a compiler error.")

            case _ => // otherwise don't do anything
          }
        }

      case _ => // otherwise don't do anything
    }

  }

  // Do checking on type parameter and generate AST that create a 'a type' matcher
  def aTypeMatcherImpl(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Matcher[Any]] = {

    import context.universe._

    val tree = aType.tree

    // check type parameter
    checkTypeParameter(context)(tree, "a")

    /**
     * Generate AST that does the following code:
     *
     * org.scalatest.matchers.TypeMatcherHelper.aTypeMatcher(aType)
     */
    context.Expr(
      Apply(
        Select(
          Select(
            Select(
              Select(
                Select(
                  Ident(TermName("_root_")),
                  TermName("org")
                ),
                TermName("scalatest")
              ),
              TermName("matchers")
            ),
            TermName("TypeMatcherHelper")
          ),
          TermName("aTypeMatcher")
        ),
        List(tree)
      )
    )

  }

  // Do checking on type parameter and generate AST that create a 'an type' matcher
  def anTypeMatcherImpl(context: Context)(anType: context.Expr[ResultOfAnTypeInvocation[_]]): context.Expr[Matcher[Any]] = {

    import context.universe._

    val tree = anType.tree

    // check type parameter
    checkTypeParameter(context)(tree, "an")

    /**
     * Generate AST that does the following code:
     *
     * org.scalatest.matchers.TypeMatcherHelper.anTypeMatcher(anType)
     */
    context.Expr(
      Apply(
        Select(
          Select(
            Select(
              Select(
                Select(
                  Ident(TermName("_root_")),
                  TermName("org")
                ),
                TermName("scalatest")
              ),
              TermName("matchers")
            ),
            TermName("TypeMatcherHelper")
          ),
          TermName("anTypeMatcher")
        ),
        List(tree)
      )
    )

  }

  // Do checking on type parameter and generate AST that create a negated 'a type' matcher
  def notATypeMatcher(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Matcher[Any]] = {
    import context.universe._

    val tree = aType.tree

    // check type parameter
    checkTypeParameter(context)(tree, "a")

    /**
     * Generate AST that does the following code:
     *
     * org.scalatest.matchers.TypeMatcherHelper.notATypeMatcher(aType)
     */
    context.Expr(
      Apply(
        Select(
          Select(
            Select(
              Select(
                Select(
                  Ident(TermName("_root_")),
                  TermName("org")
                ),
                TermName("scalatest")
              ),
              TermName("matchers")
            ),
            TermName("TypeMatcherHelper")
          ),
          TermName("notATypeMatcher")
        ),
        List(tree)
      )
    )
  }

  // Do checking on type parameter and generate AST that create a negated 'an type' matcher
  def notAnTypeMatcher(context: Context)(anType: context.Expr[ResultOfAnTypeInvocation[_]]): context.Expr[Matcher[Any]] = {
    import context.universe._

    val tree = anType.tree

    // check type parameter
    checkTypeParameter(context)(tree, "an")

    /**
     * Generate AST that does the following code:
     *
     * org.scalatest.matchers.TypeMatcherHelper.notAnTypeMatcher(anType)
     */
    context.Expr(
      Apply(
        Select(
          Select(
            Select(
              Select(
                Select(
                  Ident(TermName("_root_")),
                  TermName("org")
                ),
                TermName("scalatest")
              ),
              TermName("matchers")
            ),
            TermName("TypeMatcherHelper")
          ),
          TermName("notAnTypeMatcher")
        ),
        List(tree)
      )
    )
  }

  // Do checking on type parameter and generate AST that does a 'and not' logical expression matcher for 'a type' matcher.
  def andNotATypeMatcher(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Matcher[Any]] = {

    import context.universe._

    // create a negated matcher from notATypeMatcher
    val rhs = notATypeMatcher(context)(aType)

    /**
     * Generate AST for code that call the 'and' method on the Matcher instance (reference through 'owner'):
     *
     * owner.and(rhs)
     */
    context.macroApplication match {
      case Apply(Select(qualifier, _), _) =>
        context.Expr(
          Apply(
            Select(
              Select(
                qualifier,
                TermName("owner")
              ),
              TermName("and")
            ),
            List(rhs.tree)
          )
        )
      case _ => context.abort(context.macroApplication.pos, "This macro should be used with 'and not' syntax only.")
    }

  }

  // Do checking on type parameter and generate AST that does a 'and not' logical expression matcher for 'an type' matcher.
  def andNotAnTypeMatcher(context: Context)(anType: context.Expr[ResultOfAnTypeInvocation[_]]): context.Expr[Matcher[Any]] = {

    import context.universe._

    // create a negated matcher from notAnTypeMatcher
    val rhs = notAnTypeMatcher(context)(anType)

    /**
     * Generate AST for code that call the 'and' method on the Matcher instance (reference through 'owner'):
     *
     * owner.and(rhs)
     */
    context.macroApplication match {
      case Apply(Select(qualifier, _), _) =>
        context.Expr(
          Apply(
            Select(
              Select(
                qualifier,
                TermName("owner")
              ),
              TermName("and")
            ),
            List(rhs.tree)
          )
        )
      case _ => context.abort(context.macroApplication.pos, "This macro should be used with 'and not' syntax only.")
    }

  }

  // Do checking on type parameter and generate AST that does a 'or not' logical expression matcher for 'a type' matcher.
  def orNotATypeMatcher(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Matcher[Any]] = {

    import context.universe._

    // create a negated matcher from notATypeMatcher
    val rhs = notATypeMatcher(context)(aType)

    /**
     * Generate AST for code that call the 'or' method on the Matcher instance (reference through 'owner'):
     *
     * owner.or(rhs)
     */
    context.macroApplication match {
      case Apply(Select(qualifier, _), _) =>
        context.Expr(
          Apply(
            Select(
              Select(
                qualifier,
                TermName("owner")
              ),
              TermName("or")
            ),
            List(rhs.tree)
          )
        )
      case _ => context.abort(context.macroApplication.pos, "This macro should be used with 'or not' syntax only.")
    }

  }

  // Do checking on type parameter and generate AST that does a 'or not' logical expression matcher for 'an type' matcher.
  def orNotAnTypeMatcher(context: Context)(anType: context.Expr[ResultOfAnTypeInvocation[_]]): context.Expr[Matcher[Any]] = {

    import context.universe._

    // create a negated matcher from notAnTypeMatcher
    val rhs = notAnTypeMatcher(context)(anType)

    /**
     * Generate AST for code that call the 'or' method on the Matcher instance (reference through 'owner'):
     *
     * owner.or(rhs)
     */
    context.macroApplication match {
      case Apply(Select(qualifier, _), _) =>
        context.Expr(
          Apply(
            Select(
              Select(
                qualifier,
                TermName("owner")
              ),
              TermName("or")
            ),
            List(rhs.tree)
          )
        )
      case _ => context.abort(context.macroApplication.pos, "This macro should be used with 'or not' syntax only.")
    }

  }

  // Do checking on type parameter and generate AST to call TypeMatcherHelper.<methodName>, used by 'shouldBe a [type]' and 'shouldBe an [type]' syntax
  def assertTypeImpl(context: Context)(tree: context.Tree, beMethodName: String, assertMethodName: String): context.Expr[org.scalatest.Assertion] = {
    import context.universe._

    def valDef(name: String, rhs: Tree): ValDef =
      ValDef(
        Modifiers(),
        TermName(name),
        TypeTree(),
        rhs
      )

    // check type parameter
    checkTypeParameter(context)(tree, "a")

    /**
     * Generate AST to call TypeMatcherHelper.checkAType:
     *
     * org.scalatest.matchers.TypeMatcherHelper.checkAType(lhs, aType)
     */
    val callHelper =
      context.macroApplication match {
        case Apply(Select(qualifier, _), _) =>
          q"""
            ${valDef("$org_scalatest_type_matcher_macro_left", qualifier.duplicate)}
            ${Apply(
              Select(
                Select(
                  Select(
                    Select(
                      Select(
                        Ident(TermName("_root_")),
                        TermName("org")
                      ),
                      TermName("scalatest")
                    ),
                    TermName("matchers")
                  ),
                  TermName("TypeMatcherHelper")
                ),
                TermName(assertMethodName)
              ),
              List(Select(Ident(TermName("$org_scalatest_type_matcher_macro_left")), TermName("leftSideValue")), tree, Select(Ident(TermName("$org_scalatest_type_matcher_macro_left")), TermName("prettifier")), Select(Ident(TermName("$org_scalatest_type_matcher_macro_left")), TermName("pos")))
            )}
          """

        case _ => context.abort(context.macroApplication.pos, s"This macro should be used with $beMethodName [Type] syntax only.")
      }

    context.Expr(callHelper)
  }

  // Do checking on type parameter and generate AST to call TypeMatcherHelper.checkAType, used by 'shouldBe a [type]' syntax
  def shouldBeATypeImpl(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[org.scalatest.Assertion] =
    assertTypeImpl(context)(aType.tree, "shouldBe a", "assertAType")

  // Do checking on type parameter and generate AST to call TypeMatcherHelper.checkAType, used by 'mustBe a [type]' syntax
  def mustBeATypeImpl(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[org.scalatest.Assertion] =
    assertTypeImpl(context)(aType.tree, "mustBe a", "assertAType")

  // Do checking on type parameter and generate AST to call TypeMatcherHelper.checkAType, used by 'shouldBe an [type]' syntax
  def shouldBeAnTypeImpl(context: Context)(anType: context.Expr[ResultOfAnTypeInvocation[_]]): context.Expr[org.scalatest.Assertion] =
    assertTypeImpl(context)(anType.tree, "shouldBe an", "assertAnType")

  // Do checking on type parameter and generate AST to call TypeMatcherHelper.checkAnType, used by 'mustBe an [type]' syntax
  def mustBeAnTypeImpl(context: Context)(anType: context.Expr[ResultOfAnTypeInvocation[_]]): context.Expr[org.scalatest.Assertion] =
    assertTypeImpl(context)(anType.tree, "mustBe an", "assertAnType")

  /*def expectTypeImpl(context: Context)(tree: context.Tree, beMethodName: String, assertMethodName: String): context.Expr[org.scalatest.Fact] = {
    import context.universe._

    // check type parameter
    checkTypeParameter(context)(tree, "a")

    /**
     * Generate AST to call TypeMatcherHelper.checkAType:
     *
     * org.scalatest.matchers.TypeMatcherHelper.checkAType(lhs, aType)
     */
    val callHelper =
      context.macroApplication match {
        case Apply(Select(qualifier, _), _) =>
          Apply(
            Select(
              Select(
                Select(
                  Select(
                    Select(
                      Ident(TermName("_root_")),
                      TermName("org")
                    ),
                    TermName("scalatest")
                  ),
                  TermName("matchers")
                ),
                TermName("TypeMatcherHelper")
              ),
              TermName(assertMethodName)
            ),
            List(Select(qualifier, TermName("leftSideValue")), tree)
          )

        case _ => context.abort(context.macroApplication.pos, s"This macro should be used with $beMethodName [Type] syntax only.")
      }

    context.Expr(callHelper)
  }

  def willBeATypeImpl(context: Context)(aType: context.Expr[FactResultOfATypeInvocation[_]]): context.Expr[org.scalatest.Fact] =
    expectTypeImpl(context)(aType.tree, "willBe a", "expectAType")

  def willBeAnTypeImpl(context: Context)(anType: context.Expr[FactResultOfAnTypeInvocation[_]]): context.Expr[org.scalatest.Fact] =
    expectTypeImpl(context)(anType.tree, "willBe an", "expectAnType")*/

  def assertTypeShouldBeTrueImpl(context: Context)(tree: context.Tree, beMethodName: String, assertMethodName: String): context.Expr[org.scalatest.Assertion] = {
    import context.universe._

    def valDef(name: String, rhs: Tree): ValDef =
      ValDef(
        Modifiers(),
        TermName(name),
        TypeTree(),
        rhs
      )

    // check type parameter
    checkTypeParameter(context)(tree, "a")

    /**
     * Generate AST to call TypeMatcherHelper.checkATypeShouldBeTrue:
     *
     * org.scalatest.matchers.TypeMatcherHelper.checkATypeShouldBeTrue(lhs, aType, shouldBeTrue)
     */
    val callHelper =
      context.macroApplication match {
        case Apply(Select(qualifier, _), _) =>
          q"""
            ${valDef("$org_scalatest_type_matcher_macro_left", qualifier.duplicate)}
            ${Apply(
              Select(
                Select(
                  Select(
                    Select(
                      Select(
                        Ident(TermName("_root_")),
                        TermName("org")
                      ),
                      TermName("scalatest")
                    ),
                    TermName("matchers")
                  ),
                  TermName("TypeMatcherHelper")
                ),
                TermName(assertMethodName)
              ),
              List(Select(qualifier, TermName("left")), tree, Select(qualifier, TermName("shouldBeTrue")), Select(Ident(TermName("$org_scalatest_type_matcher_macro_left")), TermName("prettifier")), Select(Ident(TermName("$org_scalatest_type_matcher_macro_left")), TermName("pos")))
            )}
          """

        case _ => context.abort(context.macroApplication.pos, s"This macro should be used with $beMethodName [Type] syntax only.")
      }

    context.Expr(callHelper)
  }

  // Do checking on type parameter and generate AST to call TypeMatcherHelper.assertATypeShouldBeTrue
  def assertATypeShouldBeTrueImpl(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[org.scalatest.Assertion] =
    assertTypeShouldBeTrueImpl(context)(aType.tree, "should not be a", "assertATypeShouldBeTrue")

  // Do checking on type parameter and generate AST to call TypeMatcherHelper.assertAnTypeShouldBeTrue
  def assertAnTypeShouldBeTrueImpl(context: Context)(anType: context.Expr[ResultOfAnTypeInvocation[_]]): context.Expr[org.scalatest.Assertion] =
    assertTypeShouldBeTrueImpl(context)(anType.tree, "should not be an", "assertAnTypeShouldBeTrue")

  /*def expectTypeWillBeTrueImpl(context: Context)(tree: context.Tree, beMethodName: String, expectMethodName: String): context.Expr[org.scalatest.Fact] = {
    import context.universe._

    // check type parameter
    checkTypeParameter(context)(tree, "a")

    /**
     * Generate AST to call TypeMatcherHelper.checkATypeShouldBeTrue:
     *
     * org.scalatest.matchers.TypeMatcherHelper.checkATypeShouldBeTrue(lhs, aType, shouldBeTrue)
     */
    val callHelper =
      context.macroApplication match {
        case Apply(Select(qualifier, _), _) =>
          Apply(
            Select(
              Select(
                Select(
                  Select(
                    Select(
                      Ident(TermName("_root_")),
                      TermName("org")
                    ),
                    TermName("scalatest")
                  ),
                  TermName("matchers")
                ),
                TermName("TypeMatcherHelper")
              ),
              TermName(expectMethodName)
            ),
            List(Select(qualifier, TermName("left")), tree, Select(qualifier, TermName("shouldBeTrue")))
          )

        case _ => context.abort(context.macroApplication.pos, s"This macro should be used with $beMethodName [Type] syntax only.")
      }

    context.Expr(callHelper)
  }

  // Do checking on type parameter and generate AST to call TypeMatcherHelper.assertATypeShouldBeTrue
  def expectATypeWillBeTrueImpl(context: Context)(aType: context.Expr[FactResultOfATypeInvocation[_]]): context.Expr[org.scalatest.Fact] =
    expectTypeWillBeTrueImpl(context)(aType.tree, "will not be a", "expectATypeWillBeTrue")

  // Do checking on type parameter and generate AST to call TypeMatcherHelper.assertAnTypeShouldBeTrue
  def expectAnTypeWillBeTrueImpl(context: Context)(anType: context.Expr[FactResultOfAnTypeInvocation[_]]): context.Expr[org.scalatest.Fact] =
    expectTypeWillBeTrueImpl(context)(anType.tree, "will not be an", "expectAnTypeWillBeTrue")*/

}
