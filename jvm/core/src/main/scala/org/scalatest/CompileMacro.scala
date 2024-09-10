/*
 * Copyright 2001-2012 Artima, Inc.
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

import org.scalactic._
import org.scalatest.verbs.{TypeCheckWord, CompileWord}
import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.{TypecheckException, ParseException}
import org.scalatest.exceptions._

private[scalatest] object CompileMacro {

  // extract the code string from the AST
  def getCodeStringFromCodeExpression(c: Context)(methodName: String, code: c.Expr[String]): String = {
    import c.universe._
    code.tree match {
      case Literal(Constant(codeStr)) => codeStr.toString  // normal string literal
      case Select(
        Apply(
          Select(
            _,
            augmentStringTermName
          ),
          List(
            Literal(Constant(codeStr))
          )
        ),
        stripMarginTermName
      ) if augmentStringTermName.decodedName.toString == "augmentString" && stripMarginTermName.decodedName.toString == "stripMargin" => codeStr.toString.stripMargin // """xxx""".stripMargin string literal
      case _ => c.abort(c.enclosingPosition, methodName + " only works with String literals.")
    }
  }

  def containsAnyValNullStatement(c: Context)(trees: List[c.Tree]): Boolean = {

    import c.universe._

    trees match {
      case head :: tail =>
        head match {
          case Block(stats, expr) =>
            containsAnyValNullStatement(c)(tail ++ stats ++ List(expr))

          case ValDef(mods, name, tpt, Literal(Constant(null))) if tpt.tpe == null /*&& rhs.symbol == definitions.NullClass*/ =>
            true

          case _ => containsAnyValNullStatement(c)(tail)
        }

      case _ => false
    }

  }

  // parse and type check a code snippet, generate code to throw TestFailedException when type check passes or parse error
  def assertTypeErrorImpl(c: Context)(code: c.Expr[String])(pos: c.Expr[source.Position]): c.Expr[Assertion] = {
    import c.universe._

    // extract code snippet
    val codeStr = getCodeStringFromCodeExpression(c)("assertNoTypeError", code)

    try {
      val tree = c.parse("{ "+codeStr+" }")
      if (!containsAnyValNullStatement(c)(List(tree))) {
        c.typecheck(tree) // parse and type check code snippet
        // If reach here, type check passes, let's generate code to throw TestFailedException
        val messageExpr = c.Expr[String](q"${Resources.expectedTypeErrorButGotNone(codeStr)}")
        reify {
          throw new TestFailedException((_: StackDepthException) => Some(messageExpr.splice), None, pos.splice)
        }
      }
      else {
        reify {
          // statement such as val i: Int = null, compile fails as expected, generate code to return Succeeded
          Succeeded
        }
      }
    } catch {
      case e: TypecheckException =>
        reify {
          // type check failed as expected, generate code to return Succeeded
          Succeeded
        }
      case e: ParseException =>
        // parse error, generate code to throw TestFailedException
        val messageExpr = c.Expr[String](q"${Resources.expectedTypeErrorButGotParseError(e.getMessage, codeStr)}")
        reify {
          throw new TestFailedException((_: StackDepthException) => Some(messageExpr.splice), None, pos.splice)
        }
    }
  }

  def expectTypeErrorImpl(c: Context)(code: c.Expr[String])(prettifier: c.Expr[Prettifier], pos: c.Expr[source.Position]): c.Expr[Fact] = {
    import c.universe._

    // extract code snippet
    val codeStr = getCodeStringFromCodeExpression(c)("expectNoTypeError", code)

    try {
      val tree = c.parse("{ "+codeStr+" }")
      if (!containsAnyValNullStatement(c)(List(tree))) {
        c.typecheck(tree)  // parse and type check code snippet
        // If reach here, type check passes, let's generate code to throw TestFailedException
        val messageExpr = c.Expr[String](q"${Resources.expectedTypeErrorButGotNone(codeStr)}")
        reify {
          Fact.No(
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty, 
            prettifier.splice
          )
        }
      }
      else {
        val messageExpr = c.Expr[String](q"${Resources.gotTypeErrorAsExpected(codeStr)}")
        reify {
          // statement such as val i: Int = null, type check fails as expected, return Yes
          Fact.Yes(
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty, 
            prettifier.splice
          )
        }
      }

    } catch {
      case e: TypecheckException =>
        val messageExpr = c.Expr[String](q"${Resources.gotTypeErrorAsExpected(codeStr)}")
        reify {
          // type check failed as expected, return Yes
          Fact.Yes(
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty, 
            prettifier.splice
          )
        }
      case e: ParseException =>
        // parse error, generate code to throw TestFailedException
        val messageExpr = c.Expr[String](q"${Resources.expectedTypeErrorButGotParseError(e.getMessage, codeStr)}")
        reify {
          Fact.No(
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty, 
            prettifier.splice
          )
        }
    }
  }

  // parse and type check a code snippet, generate code to throw TestFailedException when both parse and type check succeeded
  def assertDoesNotCompileImpl(c: Context)(code: c.Expr[String])(pos: c.Expr[source.Position]): c.Expr[Assertion] = {
    import c.universe._

    // extract code snippet
    val codeStr = getCodeStringFromCodeExpression(c)("assertDoesNotCompile", code)

    try {
      val tree = c.parse("{ "+codeStr+" }")
      if (!containsAnyValNullStatement(c)(List(tree))) {
        c.typecheck(tree)  // parse and type check code snippet
        // Both parse and type check succeeded, the code snippet compiles unexpectedly, let's generate code to throw TestFailedException
        val messageExpr = c.Expr[String](q"${Resources.expectedCompileErrorButGotNone(codeStr)}")
        reify {
          throw new TestFailedException((_: StackDepthException) => Some(messageExpr.splice), None, pos.splice)
        }
      }
      else
        reify {
          // statement such as val i: Int = null, compile fails as expected, generate code to return Succeeded
          Succeeded
        }
    } catch {
      case e: TypecheckException =>
        reify {
          // type check error, code snippet does not compile as expected, generate code to return Succeeded
          Succeeded
        }
      case e: ParseException =>
        reify {
          // parse error, code snippet does not compile as expected, generate code to return Succeeded
          Succeeded
        }
      case t: Throwable =>
        t.printStackTrace()
        throw t
    }
  }

  // parse and type check a code snippet, generate code to return Fact (Yes or No).
  def expectDoesNotCompileImpl(c: Context)(code: c.Expr[String])(prettifier: c.Expr[Prettifier], pos: c.Expr[source.Position]): c.Expr[Fact] = {
    import c.universe._

    // extract code snippet
    val codeStr = getCodeStringFromCodeExpression(c)("expectDoesNotCompile", code)

    try {
      val tree = c.parse("{ "+codeStr+" }")
      if (!containsAnyValNullStatement(c)(List(tree))) {
        c.typecheck(tree) // parse and type check code snippet
        // Both parse and type check succeeded, the code snippet compiles unexpectedly, let's generate code to throw TestFailedException
        val messageExpr = c.Expr[String](q"${Resources.expectedCompileErrorButGotNone(codeStr)}")
        reify {
          Fact.No(
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty, 
            prettifier.splice
          )
          //throw new TestFailedException(messageExpr.splice, stackDepth)
        }
      }
      else {
        val messageExpr = c.Expr[String](q"${Resources.didNotCompile(codeStr)}")
        reify {
          // statement such as val i: Int = null, compile fails as expected, generate code to return Fact.Yes
          Fact.Yes(
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty, 
            prettifier.splice
          )
        }
      }
    } catch {
      case e: TypecheckException =>
        val messageExpr = c.Expr[String](q"${Resources.didNotCompile(codeStr)}")
        reify {
          // type check error, code snippet does not compile as expected, generate code to return Succeeded
          Fact.Yes(
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty, 
            prettifier.splice
          )
        }
      case e: ParseException =>
        val messageExpr = c.Expr[String](q"${Resources.didNotCompile(codeStr)}")
        reify {
          // parse error, code snippet does not compile as expected, generate code to return Succeeded
          Fact.Yes(
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty, 
            prettifier.splice
          )
          //Succeeded
        }
    }
  }

  // parse and type check a code snippet, generate code to throw TestFailedException when either parse or type check fails.
  def assertCompilesImpl(c: Context)(code: c.Expr[String])(pos: c.Expr[source.Position]): c.Expr[Assertion] = {
    import c.universe._

    // extract code snippet
    val codeStr = getCodeStringFromCodeExpression(c)("assertCompiles", code)

    try {
      val tree = c.parse("{ " + codeStr + " }")
      c.typecheck(tree) // parse and type check code snippet
      // Both parse and type check succeeded, the code snippet compiles as expected, generate code to return Succeeded
      reify {
        Succeeded
      }
    } catch {
      case e: TypecheckException =>
        // type check error, compiles fails, generate code to throw TestFailedException
        val messageExpr = c.Expr[String](q"${Resources.expectedNoErrorButGotTypeError(e.getMessage, codeStr)}")
        reify {
          throw new TestFailedException((_: StackDepthException) => Some(messageExpr.splice), None, pos.splice)
        }
      case e: ParseException =>
        // parse error, compiles fails, generate code to throw TestFailedException
        val messageExpr = c.Expr[String](q"${Resources.expectedNoErrorButGotParseError(e.getMessage, codeStr)}")
        reify {
          throw new TestFailedException((_: StackDepthException) => Some(messageExpr.splice), None, pos.splice)
        }
    }
  }

  def expectCompilesImpl(c: Context)(code: c.Expr[String])(prettifier: c.Expr[Prettifier], pos: c.Expr[source.Position]): c.Expr[Fact] = {
    import c.universe._

    // extract code snippet
    val codeStr = getCodeStringFromCodeExpression(c)("expectCompiles", code)

    try {
      c.typecheck(c.parse("{ " + codeStr + " }")) // parse and type check code snippet
      // Both parse and type check succeeded, the code snippet compiles as expected, generate code to return Succeeded
      val messageExpr = c.Expr[String](q"${Resources.compiledSuccessfully(codeStr)}")
      reify {
        Fact.Yes(
          messageExpr.splice,
          messageExpr.splice,
          messageExpr.splice,
          messageExpr.splice,
          Vector.empty,
          Vector.empty,
          Vector.empty,
          Vector.empty, 
          prettifier.splice
        )
        //Succeeded
      }
    } catch {
      case e: TypecheckException =>
        // type check error, compiles fails, generate code to throw TestFailedException
        val messageExpr = c.Expr[String](q"${Resources.expectedNoErrorButGotTypeError(e.getMessage, codeStr)}")
        reify {
          Fact.No(
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty, 
            prettifier.splice
          )
        }
      case e: ParseException =>
        // parse error, compiles fails, generate code to throw TestFailedException
        val messageExpr = c.Expr[String](q"${Resources.expectedNoErrorButGotParseError(e.getMessage, codeStr)}")
        reify {
          Fact.No(
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            messageExpr.splice,
            Vector.empty,
            Vector.empty,
            Vector.empty,
            Vector.empty, 
            prettifier.splice
          )
        }
    }
  }

  // check that a code snippet does not compile
  def assertNotCompileImpl(c: Context)(compileWord: c.Expr[CompileWord], pos: c.Expr[source.Position])(shouldOrMust: String): c.Expr[Assertion] = {

    import c.universe._

    // parse and type check a code snippet, generate code to throw TestFailedException if both parse and type check succeeded
    def checkNotCompile(code: String): c.Expr[Assertion] = {
      try {
        val tree = c.parse("{ " + code + " }")
        if (!containsAnyValNullStatement(c)(List(tree))) {
          c.typecheck(tree)
          // both parse and type check succeeded, compiles succeeded unexpectedly, generate code to throw TestFailedException
          val messageExpr = c.Expr[String](q"${Resources.expectedCompileErrorButGotNone(code)}")
          reify {
            throw new TestFailedException((_: StackDepthException) => Some(messageExpr.splice), None, pos.splice)
          }
        }
        else
          reify {
            // statement such as val i: Int = null, compile fails as expected, generate code to return Succeeded
            Succeeded
          }
      } catch {
        case e: TypecheckException =>
          reify {
            // type check error, compile fails as expected, generate code to return Succeeded
            Succeeded
          }
        case e: ParseException =>
          reify {
            // parse error, compile fails as expected, generate code to return Succeeded
            Succeeded
          }
      }
    }

    val methodName = shouldOrMust + "Not"

    c.macroApplication match {
      case Apply(
             Apply(
               Select(
                 Apply(
                   Apply(
                     _,
                     List(
                       Literal(Constant(code))
                     )
                   ),
                   _
                 ),
                 methodNameTermName
               ),
               _
             ),
             _
           ) if methodNameTermName.decodedName.toString == methodName =>
        // LHS is a normal string literal, call checkNotCompile with the extracted code string to generate code
        val codeStr = code.toString
        checkNotCompile(codeStr)

      case Apply(
             Apply(
               Select(
                 Apply(
                   Apply(
                     _,
                     List(
                       Select(
                         Apply(
                           Select(
                             _,
                             augmentStringTermName
                           ),
                           List(
                             Literal(
                               Constant(code)
                             )
                           )
                         ),
                         stripMarginTermName
                       )
                     )
                   ),
                   _
                 ),
                 methodNameTermName
               ),
               _
             ),
             _
           ) if augmentStringTermName.decodedName.toString == "augmentString" && stripMarginTermName.decodedName.toString == "stripMargin" && methodNameTermName.decodedName.toString == methodName =>
        // LHS is a """xxx""".stripMargin string literal, call checkNotCompile with the extracted code string to generate code
        val codeStr = code.toString.stripMargin
        checkNotCompile(codeStr)

      case _ => c.abort(c.enclosingPosition, "The '" + shouldOrMust + "Not compile' syntax only works with String literals.")
    }
  }

  // used by shouldNot compile syntax, delegate to assertNotCompileImpl to generate code
  def shouldNotCompileImpl(c: Context)(compileWord: c.Expr[CompileWord])(pos: c.Expr[source.Position]): c.Expr[Assertion] =
    assertNotCompileImpl(c)(compileWord, pos)("should")

  // used by mustNot compile syntax, delegate to assertNotCompileImpl to generate code
  def mustNotCompileImpl(c: Context)(compileWord: c.Expr[CompileWord])(pos: c.Expr[source.Position]): c.Expr[Assertion] =
    assertNotCompileImpl(c)(compileWord, pos)("must")

  def expectNotCompileImpl(c: Context)(compileWord: c.Expr[CompileWord], prettifier: c.Expr[Prettifier], pos: c.Expr[source.Position]): c.Expr[Fact] = {

    import c.universe._

    // parse and type check a code snippet, generate code to throw TestFailedException if both parse and type check succeeded
    def checkNotCompile(code: String): c.Expr[Fact] = {
      try {
        val tree = c.parse("{ " + code + " }")
        if (!containsAnyValNullStatement(c)(List(tree))) {
          c.typecheck(tree)  // parse and type check code snippet
          // both parse and type check succeeded, compiles succeeded unexpectedly, generate code to throw TestFailedException
          val messageExpr = c.Expr[String](q"${Resources.expectedCompileErrorButGotNone(code)}")
          reify {
            Fact.No(messageExpr.splice, prettifier.splice)
          }
        }
        else {
          val messageExpr = c.Expr[String](q"${Resources.compiledSuccessfully(code)}")
          reify {
            // statement such as val i: Int = null, type check error as expected, return Yes
            Fact.Yes(messageExpr.splice, prettifier.splice)
          }
        }
      } catch {
        case e: TypecheckException =>
          val messageExpr = c.Expr[String](q"${Resources.compiledSuccessfully(code)}")
          reify {
            // type check error, compile fails as expected, generate code to return Yes
            Fact.Yes(messageExpr.splice, prettifier.splice)
          }
        case e: ParseException =>
          val messageExpr = c.Expr[String](q"${Resources.compiledSuccessfully(code)}")
          reify {
            // parse error, compile fails as expected, generate code to return Yes
            Fact.Yes(messageExpr.splice, prettifier.splice)
          }
      }
    }

    val methodName = "willNot"

    c.macroApplication match {
      case Apply(
        Select(
          Apply(
            _,
            List(
              Literal(Constant(code))
            )
          ),
          methodNameTermName
        ),
        _
      ) if methodNameTermName.decodedName.toString == methodName =>
        // LHS is a normal string literal, call checkNotCompile with the extracted code string to generate code
        val codeStr = code.toString
        checkNotCompile(codeStr)

      case Apply(
        Select(
          Apply(
            _,
            List(
              Select(
                Apply(
                  Select(
                    _,
                    augmentStringTermName
                  ),
                  List(
                    Literal(
                      Constant(code)
                    )
                  )
                ),
                stripMarginTermName
              )
            )
          ),
          methodNameTermName
        ),
        _
      ) if augmentStringTermName.decodedName.toString == "augmentString" && stripMarginTermName.decodedName.toString == "stripMargin" && methodNameTermName.decodedName.toString == methodName =>
        // LHS is a """xxx""".stripMargin string literal, call checkNotCompile with the extracted code string to generate code
        val codeStr = code.toString.stripMargin
        checkNotCompile(codeStr)

      case _ => c.abort(c.enclosingPosition, "The 'willNot compile' syntax only works with String literals.")
    }
  }

  // used by willNot compile syntax, delegate to expectNotCompileImpl to generate code
  def willNotCompileImpl(c: Context)(compileWord: c.Expr[CompileWord])(prettifier: c.Expr[Prettifier], pos: c.Expr[source.Position]): c.Expr[Fact] =
    expectNotCompileImpl(c)(compileWord, prettifier, pos)

  // check that a code snippet does not compile
  def assertNotTypeCheckImpl(c: Context)(typeCheckWord: c.Expr[TypeCheckWord], pos: c.Expr[source.Position])(shouldOrMust: String): c.Expr[Assertion] = {

    import c.universe._

    // parse and type check a code snippet, generate code to throw TestFailedException if parse error or both parse and type check succeeded
    def checkNotTypeCheck(code: String): c.Expr[Assertion] = {
      try {
        val tree = c.parse("{ " + code + " }")
        if (!containsAnyValNullStatement(c)(List(tree))) {
          c.typecheck(tree) // parse and type check code snippet
          // both parse and type check succeeded unexpectedly, generate code to throw TestFailedException
          val messageExpr = c.Expr[String](q"${Resources.expectedTypeErrorButGotNone(code)}")
          reify {
            throw new TestFailedException((_: StackDepthException) => Some(messageExpr.splice), None, pos.splice)
          }
        }
        else {
          reify {
            // statement such as val i: Int = null, type check fails as expected, generate code to return Succeeded
            Succeeded
          }
        }
      } catch {
        case e: TypecheckException =>
          reify {
            // type check error as expected, generate code to return Succeeded
            Succeeded
          }
        case e: ParseException =>
          // expect type check error but got parse error, generate code to throw TestFailedException
          val messageExpr = c.Expr[String](q"${Resources.expectedTypeErrorButGotParseError(e.getMessage, code)}")
          reify {
            throw new TestFailedException((_: StackDepthException) => Some(messageExpr.splice), None, pos.splice)
          }
      }
    }

    val methodName = shouldOrMust + "Not"

    c.macroApplication match {
      case Apply(
             Apply(
               Select(
                 Apply(
                   Apply(
                     _,
                     List(
                      Literal(Constant(code))
                     )
                   ),
                   _
                 ),
                 methodNameTermName
               ),
               _
             ),
             _
           ) if methodNameTermName.decodedName.toString == methodName =>
        // LHS is a normal string literal, call checkNotTypeCheck with the extracted code string to generate code
        val codeStr = code.toString
        checkNotTypeCheck(codeStr)

      case Apply(
             Apply(
               Select(
                 Apply(
                   Apply(
                     _,
                     List(
                       Select(
                         Apply(
                           Select(
                             _,
                             augmentStringTermName
                           ),
                           List(
                             Literal(
                               Constant(code)
                             )
                           )
                         ),
                         stripMarginTermName
                       )
                     )
                   ),
                   _
                 ),
                 methodNameTermName
               ),
               _
             ),
             _
      ) if augmentStringTermName.decodedName.toString == "augmentString" && stripMarginTermName.decodedName.toString == "stripMargin" && methodNameTermName.decodedName.toString == methodName =>
        // LHS is a """xxx""".stripMargin string literal, call checkNotTypeCheck with the extracted code string to generate code
        val codeStr = code.toString.stripMargin
        checkNotTypeCheck(codeStr)

      case _ => c.abort(c.enclosingPosition, "The '" + shouldOrMust + "Not typeCheck' syntax only works with String literals.")
    }
  }

  // used by shouldNot typeCheck syntax, delegate to assertNotTypeCheckImpl to generate code
  def shouldNotTypeCheckImpl(c: Context)(typeCheckWord: c.Expr[TypeCheckWord])(pos: c.Expr[source.Position]): c.Expr[Assertion] =
    assertNotTypeCheckImpl(c)(typeCheckWord, pos)("should")

  // used by mustNot typeCheck syntax, delegate to assertNotTypeCheckImpl to generate code
  def mustNotTypeCheckImpl(c: Context)(typeCheckWord: c.Expr[TypeCheckWord])(pos: c.Expr[source.Position]): c.Expr[Assertion] =
    assertNotTypeCheckImpl(c)(typeCheckWord, pos)("must")

  def expectNotTypeCheckImpl(c: Context)(typeCheckWord: c.Expr[TypeCheckWord], prettifier: c.Expr[Prettifier]): c.Expr[Fact] = {

    import c.universe._

    // parse and type check a code snippet, generate code to throw TestFailedException if parse error or both parse and type check succeeded
    def checkNotTypeCheck(code: String): c.Expr[Fact] = {
      try {
        val tree = c.parse("{ " + code + " }")
        if (!containsAnyValNullStatement(c)(List(tree))) {
          c.typecheck(tree)  // parse and type check code snippet
          // both parse and type check succeeded unexpectedly, generate code to throw TestFailedException
          val messageExpr = c.Expr[String](q"${Resources.expectedTypeErrorButGotNone(code)}")
          reify {
            Fact.No(messageExpr.splice, prettifier.splice)
          }
        }
        else {
          val messageExpr = c.Expr[String](q"${Resources.gotTypeErrorAsExpected(code)}")
          reify {
            // statement such as val i: Int = null, type check fails as expected, generate code to return Yes
            Fact.Yes(messageExpr.splice, prettifier.splice)
          }
        }
      } catch {
        case e: TypecheckException =>
          val messageExpr = c.Expr[String](q"${Resources.gotTypeErrorAsExpected(code)}")
          reify {
            // type check error as expected, generate code to return Yes
            Fact.Yes(messageExpr.splice, prettifier.splice)
          }
        case e: ParseException =>
          // expect type check error but got parse error, generate code to throw No
          val messageExpr = c.Expr[String](q"${Resources.expectedTypeErrorButGotParseError(e.getMessage, code)}")
          reify {
            Fact.No(messageExpr.splice, prettifier.splice)
          }
      }
    }

    val methodName = "willNot"

    c.macroApplication match {
      case Apply(
        Select(
          Apply(
            _,
            List(
              Literal(Constant(code))
            )
          ),
          methodNameTermName
        ),
        _
      ) if methodNameTermName.decodedName.toString == methodName =>
        // LHS is a normal string literal, call checkNotTypeCheck with the extracted code string to generate code
        val codeStr = code.toString
        checkNotTypeCheck(codeStr)

      case Apply(
        Select(
          Apply(
            _,
            List(
              Select(
                Apply(
                  Select(
                    _,
                    augmentStringTermName
                  ),
                  List(
                    Literal(
                      Constant(code)
                    )
                  )
                ),
                stripMarginTermName
              )
            )
          ),
          methodNameTermName
        ),
        _
      ) if augmentStringTermName.decodedName.toString == "augmentString" && stripMarginTermName.decodedName.toString == "stripMargin" && methodNameTermName.decodedName.toString == methodName =>
        // LHS is a """xxx""".stripMargin string literal, call checkNotTypeCheck with the extracted code string to generate code
        val codeStr = code.toString.stripMargin
        checkNotTypeCheck(codeStr)

      case _ => c.abort(c.enclosingPosition, "The 'willNot typeCheck' syntax only works with String literals.")
    }
  }

  // used by willNot typeCheck syntax, delegate to expectNotTypeCheckImpl to generate code
  def willNotTypeCheckImpl(c: Context)(typeCheckWord: c.Expr[TypeCheckWord])(prettifier: c.Expr[Prettifier]): c.Expr[Fact] =
    expectNotTypeCheckImpl(c)(typeCheckWord, prettifier)

  // check that a code snippet compiles
  def assertCompileImpl(c: Context)(compileWord: c.Expr[CompileWord], pos: c.Expr[source.Position])(shouldOrMust: String): c.Expr[Assertion] = {
    import c.universe._

    // parse and type check a code snippet, generate code to throw TestFailedException if either parse error or type check error
    def checkCompile(code: String): c.Expr[Assertion] = {
      try {
        c.typecheck(c.parse("{ " + code + " }"))  // parse and type check code snippet
        // both parse and type check succeeded, compile succeeded expectedly, generate code to do nothing
        reify {
          Succeeded
        }
      } catch {
        case e: TypecheckException =>
          // type check error, compile fails unexpectedly, generate code to throw TestFailedException
          val messageExpr = c.Expr[String](q"${Resources.expectedNoErrorButGotTypeError(e.getMessage, code)}")
          reify {
            throw new TestFailedException((_: StackDepthException) => Some(messageExpr.splice), None, pos.splice)
          }
        case e: ParseException =>
          // parse error, compile failes unexpectedly, generate code to throw TestFailedException
          val messageExpr = c.Expr[String](q"${Resources.expectedNoErrorButGotParseError(e.getMessage, code)}")
          reify {
            throw new TestFailedException((_: StackDepthException) => Some(messageExpr.splice), None, pos.splice)
          }
      }
    }

    c.macroApplication match {
      case Apply(
             Apply(
               Select(
                 Apply(
                   Apply(
                     _,
                     List(
                       Literal(
                         Constant(code)
                       )
                     )
                   ),
                   _
                 ),
                 shouldOrMustTermName
               ),
               _
             ),
             _
      ) if shouldOrMustTermName.decodedName.toString == shouldOrMust =>
        // LHS is a normal string literal, call checkCompile with the extracted code string to generate code
        val codeStr = code.toString
        checkCompile(codeStr)

      case Apply(
             Apply(
               Select(
                 Apply(
                   Apply(
                     _,
                     List(
                       Select(
                         Apply(
                           Select(
                             _,
                             augmentStringTermName
                           ),
                           List(
                             Literal(
                               Constant(code)
                             )
                           )
                         ),
                         stripMarginTermName
                       )
                     )
                   ),
                   _
                 ),
                 shouldOrMustTermName
               ),
               _
             ),
           _
      ) if augmentStringTermName.decodedName.toString == "augmentString" && stripMarginTermName.decodedName.toString == "stripMargin" && shouldOrMustTermName.decodedName.toString == shouldOrMust =>
        // LHS is a """xxx""".stripMargin string literal, call checkCompile with the extracted code string to generate code
        val codeStr = code.toString.stripMargin
        checkCompile(codeStr)

      case other =>
        c.abort(c.enclosingPosition, "The '" + shouldOrMust + " compile' syntax only works with String literals.")
    }
  }

  // used by should compile syntax, delegate to assertCompileImpl to generate code
  def shouldCompileImpl(c: Context)(compileWord: c.Expr[CompileWord])(pos: c.Expr[source.Position]): c.Expr[Assertion] =
    assertCompileImpl(c)(compileWord, pos)("should")

  // used by must compile syntax, delegate to assertCompileImpl to generate code
  def mustCompileImpl(c: Context)(compileWord: c.Expr[CompileWord])(pos: c.Expr[source.Position]): c.Expr[Assertion] =
    assertCompileImpl(c)(compileWord, pos)("must")

  def expectCompileImpl(c: Context)(compileWord: c.Expr[CompileWord], prettifier: c.Expr[Prettifier]): c.Expr[Fact] = {
    import c.universe._

    // parse and type check a code snippet, generate code to throw TestFailedException if either parse error or type check error
    def checkCompile(code: String): c.Expr[Fact] = {
      try {
        c.typecheck(c.parse("{ " + code + " }"))  // parse and type check code snippet
        val messageExpr = c.Expr[String](q"${Resources.compiledSuccessfully(code)}")
        // both parse and type check succeeded, compile succeeded expectedly, generate code to do nothing
        reify {
          Fact.Yes(messageExpr.splice, prettifier.splice)
        }
      } catch {
        case e: TypecheckException =>
          // type check error, compile fails unexpectedly, generate code to throw TestFailedException
          val messageExpr = c.Expr[String](q"${Resources.expectedNoErrorButGotTypeError(e.getMessage, code)}")
          reify {
            Fact.No(messageExpr.splice, prettifier.splice)
          }
        case e: ParseException =>
          // parse error, compile failes unexpectedly, generate code to throw TestFailedException
          val messageExpr = c.Expr[String](q"${Resources.expectedNoErrorButGotParseError(e.getMessage, code)}")
          reify {
            Fact.No(messageExpr.splice, prettifier.splice)
          }
      }
    }

    c.macroApplication match {
      case Apply(
        Select(
          Apply(
            _,
            List(
              Literal(
                Constant(code)
              )
            )
          ),
          willTermName
        ),
        _
      ) if willTermName.decodedName.toString == "will" =>
        // LHS is a normal string literal, call checkCompile with the extracted code string to generate code
        val codeStr = code.toString
        checkCompile(codeStr)

      case Apply(
        Select(
          Apply(
            _,
            List(
              Select(
                Apply(
                  Select(
                    _,
                    augmentStringTermName
                  ),
                  List(
                    Literal(
                      Constant(code)
                    )
                  )
                ),
                stripMarginTermName
              )
            )
          ),
          willTermName
        ),
        _
      ) if augmentStringTermName.decodedName.toString == "augmentString" && stripMarginTermName.decodedName.toString == "stripMargin" && willTermName.decodedName.toString == "will" =>
        // LHS is a """xxx""".stripMargin string literal, call checkCompile with the extracted code string to generate code
        val codeStr = code.toString.stripMargin
        checkCompile(codeStr)

      case _ => c.abort(c.enclosingPosition, "The 'will compile' syntax only works with String literals.")
    }
  }

  // used by will compile syntax, delegate to expectCompileImpl to generate code
  def willCompileImpl(c: Context)(compileWord: c.Expr[CompileWord])(prettifier: c.Expr[Prettifier]): c.Expr[Fact] =
    expectCompileImpl(c)(compileWord, prettifier)

}
