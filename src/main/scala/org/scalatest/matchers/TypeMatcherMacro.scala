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
import org.scalatest.{UnquotedString, Resources}
import org.scalactic.Prettifier

private[scalatest] object TypeMatcherMacro {

  def beResultOfATypeInvocation(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Matcher[Any]] = {

    import context.universe._

    println("###" + showRaw(aType.tree))

    aType.tree match {
      case Apply(
             TypeApply(
               Select(
                 _,
                 methodNameTermName
               ),
               List(
                 typeTree: TypeTree
               )
             ),
             _
           ) => println("***a: " + showRaw(typeTree.original))
      case _ =>
    }

    reify {
      new Matcher[Any] {
        def apply(left: Any): MatchResult = {
          val clazz = aType.splice.clazz
          MatchResult(
            clazz.isAssignableFrom(left.getClass),
            Resources("wasNotAnInstanceOf"),
            Resources("wasAnInstanceOf"),
            Vector(left, UnquotedString(clazz.getName))
          )
        }
        override def toString: String = "be (" + Prettifier.default(aType.splice) + ")"
      }
    }

  }

  def notBeResultOfATypeInvocation(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Matcher[Any]] =
    context.universe.reify {
      new Matcher[Any] {
        def apply(left: Any): MatchResult = {
          val clazz = aType.splice.clazz
          MatchResult(
            !clazz.isAssignableFrom(left.getClass),
            Resources("wasAnInstanceOf"),
            Resources("wasNotAnInstanceOf"),
            Vector(left, UnquotedString(clazz.getName))
          )
        }
        override def toString: String = "not be " + Prettifier.default(aType.splice)
      }
    }

  def andNotBeResultOfATypeInvocation(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Matcher[Any]] = {

    import context.universe._

    val rhs = notBeResultOfATypeInvocation(context)(aType)

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

  def orNotBeResultOfATypeInvocation(context: Context)(aType: context.Expr[ResultOfATypeInvocation[_]]): context.Expr[Matcher[Any]] = {

    import context.universe._

    val rhs = notBeResultOfATypeInvocation(context)(aType)

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

}