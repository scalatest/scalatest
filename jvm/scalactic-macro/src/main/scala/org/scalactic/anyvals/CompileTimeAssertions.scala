/*
* Copyright 2001-2014 Artima, Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/
package org.scalactic.anyvals

import org.scalactic.Resources
import scala.reflect.macros.whitebox.Context

/**
 * Trait providing assertion methods that can be called at compile time from macros
 * to validate literals in source code.
 *
 * <p>
 * The intent of <code>CompileTimeAssertions</code> is to make it easier to create
 * <code>AnyVal</code>s that restrict the values of types for which Scala supports
 * literals: <code>Int</code>, <code>Long</code>, <code>Float</code>, <code>Double</code>, <code>Char</code>,
 * and <code>String</code>. For example, if you are using odd integers in many places
 * in your code, you might have validity checks scattered throughout your code. Here's
 * an example of a method that both requires an odd <code>Int</code> is passed (as a
 * <em>precondition</em>, and ensures an odd * <code>Int</code> is returned (as
 * a <em>postcondition</em>):
 * </p>
 *
 * <pre class="stHighlight">
 * def nextOdd(i: Int): Int = {
 *   def isOdd(x: Int): Boolean = x.abs % 2 == 1
 *   require(isOdd(i))
 *   (i + 2) ensuring (isOdd(_))
 * }
 * </pre>
 *
 * <p>
 * If either the precondition or postcondition check fails, an exception will
 * be thrown at runtime. If you have many methods like this you may want to
 * create a type to represent an odd <code>Int</code>, so that the checking
 * for validity errors is isolated in just one place. By using an <code>AnyVal</code>
 * you can avoid boxing the <code>Int</code>, which may be more efficient.
 * This might look like:
 * </p>
 * 
 * <pre class="stHighlight">
 * final class OddInt private (val value: Int) extends AnyVal {
 *   override def toString: String = s"OddInt($value)"
 * }
 *
 * object OddInt {
 *   def apply(value: Int): OddInt = {
 *     require(value.abs % 2 == 1)
 *     new OddInt(value)
 *   }
 * }
 * </pre>
 *
 * <p>
 * An <code>AnyVal</code> cannot have any constructor code, so to ensure that
 * any <code>Int</code> passed to the <code>OddInt</code> constructor is actually
 * odd, its constructor must be private. That way the only way to construct a
 * new <code>OddInt</code> is via the <code>apply</code> factory method in the
 * <code>OddInt</code> companion object, which can require that the value be
 * odd. This design eliminates the need for placing <code>require</code> and
 * <code>ensuring</code> clauses anywhere else that odd <code>Int</code>s are
 * needed, because the type promises the constraint. The <code>nextOdd</code>
 * method could, therefore, be rewritten as:
 * </p>
 *
 * <pre class="stHighlight">
 * def nextOdd(oi: OddInt): OddInt = OddInt(oi.value + 2)
 * </pre>
 *
 * <p>
 * Using the compile-time assertions provided by this trait, you can construct
 * a factory method implemented via a macro that causes a compile failure
 * if <code>OddInt.apply</code> is passed anything besides an odd
 * <code>Int</code> literal. Class <code>OddInt</code> would look exactly the
 * same as before:
 * </p>
 *
 * <pre class="stHighlight">
 * final class OddInt private (val value: Int) extends AnyVal {
 *   override def toString: String = s"OddInt($value)"
 * }
 * </pre>
 *
 * <p>
 * In the companion object, however, the <code>apply</code> method would
 * be implemented in terms of a macro. Because the <code>apply</code> method
 * will only work with literals, you'll need a second method that can work
 * an any expression of type <code>Int</code>. We recommend a <code>from</code> method
 * that returns an <code>Option[OddInt]</code> that returns <code>Some[OddInt}</code> if the passed <code>Int</code> is odd,
 * otherwise returns <code>None</code>, and an <code>ensuringValid</code> method that returns an <code>OddInt</code>
 * if the passed <code>Int</code> is valid, else throws <code>AssertionError</code>.
 * </p>
 *
 * <pre class="stHighlight">
 * object OddInt {
 *
 *   // The from factory method validates at run time
 *   def from(value: Int): Option[OddInt] =
 *     if (OddIntMacro.isValid(value)) Some(new OddInt(value)) else None
 *
 *   // The ensuringValid factory method validates at run time, but throws
 *   // an AssertionError if invalid
 *   def ensuringValid(value: Int): OddInt =
 *     if (OddIntMacro.isValid(value)) new OddInt(value) else {
 *       throw new AssertionError(s"$value was not a valid OddInt")
 *     }
 *
 *   // The apply factory method validates at compile time
 *   import scala.language.experimental.macros
 *   def apply(value: Int): OddInt = macro OddIntMacro.apply
 * }
 * </pre>
 *
 * <p>
 * The <code>apply</code> method refers to a macro implementation method in class
 * <code>PosIntMacro</code>. The macro implementation of any such method can look
 * very similar to this one. The only changes you'd need to make is the
 * <code>isValid</code> method implementation and the text of the error messages.
 * </p>
 *
 * <pre class="stHighlight">
 * import org.scalactic.anyvals.CompileTimeAssertions
 * import scala.reflect.macros.whitebox.Context
 *
 * object OddIntMacro extends CompileTimeAssertions {
 *
 *   // Validation method used at both compile- and run-time
 *   def isValid(i: Int): Boolean = i.abs % 2 == 1
 *
 *   // Apply macro that performs a compile-time assertion
 *   def apply(c: Context)(value: c.Expr[Int]): c.Expr[OddInt] = {
 *
 *     // Prepare potential compiler error messages
 *     val notValidMsg = "OddInt.apply can only be invoked on odd Int literals, like OddInt(3)."
 *     val notLiteralMsg = "OddInt.apply can only be invoked on Int literals, like " +
 *           "OddInt(3). Please use OddInt.from instead."
 *
 *     // Validate via a compile-time assertion
 *     ensureValidIntLiteral(c)(value, notValidMsg, notLiteralMsg)(isValid) 
 *
 *     // Validated, so rewrite the apply call to a from call
 *     c.universe.reify { OddInt.ensuringValid(value.splice) }
 *   } 
 * }
 * </pre>
 *
 * <p>
 * The <code>isValid</code> method just takes the underlying type and returns <code>true</code> if it is valid,
 * else <code>false</code>. This method is placed here so the same valiation code can be used both in
 * the <code>from</code> method at runtime and the <code>apply</code> macro at compile time. The <code>apply</code>
 * actually does just two things. It calls a <code>ensureValidIntLiteral</code>, performing a compile-time assertion
 * that value passed to <code>apply</code> is an <code>Int</code> literal that is valid (in this case, odd).
 * If the assertion fails, <code>ensureValidIntLiteral</code> will complete abruptly with an exception that will
 * contain an appropriate error message (one of the two you passed in) and cause a compiler error with that message.
 * If the assertion succeeds, <code>ensureValidIntLiteral</code> will just return normally. The next line of code
 * will then execute. This line of code then constructs an AST (abstract syntax tree) that will replace
 * the <code>OddInt.apply</code> invocation. We invoke the other factory method that either returns an <code>OddInt</code>
 * or throws an <code>AssertionError</code>, since we've proven at compile time that the call will succeed.
 * </p>
 *
 * <p>
 * You may wish to use quasi-quotes instead of reify. The reason we use reify is that this also works on 2.10 without
 * any additional plugin (i.e., you don't need macro paradise), and Scalactic supports 2.10.
 * </p>
 */
trait CompileTimeAssertions {

  /**
   * Ensures a given expression of type <code>Int</code> is a literal with a valid value according to a given validation function.
   *
   * <p>
   * If the given <code>Int</code> expression is a literal whose value satisfies the given validation function, this method will
   * return normally. Otherwise, if the given <code>Int</code> expression is not a literal, this method will complete abruptly with
   * an exception whose detail message includes the <code>String</code> passed as <code>notLiteralMsg</code>. Otherwise, the
   * given <code>Int</code> expression is a literal that does <em>not</em> satisfy the given validation function, so this method will
   * complete abruptly with an exception whose detail message includes the <code>String</code> passed as <code>notValidMsg</code>.
   * </p>
   *
   * <p>
   * This method is intended to be invoked at compile time from macros. When called from a macro, exceptions thrown by this method
   * will result in compiler errors. The detail message of the thrown exception will appear as the compiler error message.
   * </p>
   *
   * @param c the compiler context for this assertion
   * @param value the <code>Int</code> expression to validate
   * @param notValidMsg a <code>String</code> message to include in the exception thrown if the expression is a literal, but not valid
   * @param notLiteralMsg a <code>String</code> message to include in the exception thrown if the expression is not a literal
   * @param isValid a function used to validate a literal value parsed from the given expression
   */
  def ensureValidIntLiteral(c: Context)(value: c.Expr[Int], notValidMsg: String, notLiteralMsg: String)(isValid: Int => Boolean): Unit = {

    import c.universe._

    value.tree match {
      case Literal(intConst) =>
        val literalValue = intConst.value.toString.toInt
        if (!isValid(literalValue))
          c.abort(c.enclosingPosition, notValidMsg)
      case _ =>
        c.abort(c.enclosingPosition, notLiteralMsg)
    } 
  } 

  /**
   * Ensures a given expression of type <code>Long</code> is a literal with a valid value according to a given validation function.
   *
   * <p>
   * If the given <code>Long</code> expression is a literal whose value satisfies the given validation function, this method will
   * return normally. Otherwise, if the given <code>Long</code> expression is not a literal, this method will complete abruptly with
   * an exception whose detail message includes the <code>String</code> passed as <code>notLiteralMsg</code>. Otherwise, the
   * given <code>Long</code> expression is a literal that does <em>not</em> satisfy the given validation function, so this method will
   * complete abruptly with an exception whose detail message includes the <code>String</code> passed as <code>notValidMsg</code>.
   * </p>
   *
   * <p>
   * This method is intended to be invoked at compile time from macros. When called from a macro, exceptions thrown by this method
   * will result in compiler errors. The detail message of the thrown exception will appear as the compiler error message.
   * </p>
   *
   * @param c the compiler context for this assertion
   * @param value the <code>Long</code> expression to validate
   * @param notValidMsg a <code>String</code> message to include in the exception thrown if the expression is a literal, but not valid
   * @param notLiteralMsg a <code>String</code> message to include in the exception thrown if the expression is not a literal
   * @param isValid a function used to validate a literal value parsed from the given expression
   */
  def ensureValidLongLiteral(c: Context)(value: c.Expr[Long], notValidMsg: String, notLiteralMsg: String)(isValid: Long => Boolean): Unit = {

    import c.universe._

    value.tree match {
      case Literal(longConst) =>
        val literalValue = longConst.value.toString.toLong
        if (!isValid(literalValue))
          c.abort(c.enclosingPosition, notValidMsg)
      case _ =>
        c.abort(c.enclosingPosition, notLiteralMsg)
    } 
  } 

  /**
   * Ensures a given expression of type <code>Float</code> is a literal with a valid value according to a given validation function.
   *
   * <p>
   * If the given <code>Float</code> expression is a literal whose value satisfies the given validation function, this method will
   * return normally. Otherwise, if the given <code>Float</code> expression is not a literal, this method will complete abruptly with
   * an exception whose detail message includes the <code>String</code> passed as <code>notLiteralMsg</code>. Otherwise, the
   * given <code>Float</code> expression is a literal that does <em>not</em> satisfy the given validation function, so this method will
   * complete abruptly with an exception whose detail message includes the <code>String</code> passed as <code>notValidMsg</code>.
   * </p>
   *
   * <p>
   * This method is intended to be invoked at compile time from macros. When called from a macro, exceptions thrown by this method
   * will result in compiler errors. The detail message of the thrown exception will appear as the compiler error message.
   * </p>
   *
   * @param c the compiler context for this assertion
   * @param value the <code>Float</code> expression to validate
   * @param notValidMsg a <code>String</code> message to include in the exception thrown if the expression is a literal, but not valid
   * @param notLiteralMsg a <code>String</code> message to include in the exception thrown if the expression is not a literal
   * @param isValid a function used to validate a literal value parsed from the given expression
   */
  def ensureValidFloatLiteral(c: Context)(value: c.Expr[Float], notValidMsg: String, notLiteralMsg: String)(isValid: Float => Boolean): Unit = {

    import c.universe._

    value.tree match {
      case Literal(floatConst) =>
        val literalValue = floatConst.value.toString.toFloat
        if (!isValid(literalValue))
          c.abort(c.enclosingPosition, notValidMsg)
      case _ =>
        c.abort(c.enclosingPosition, notLiteralMsg)
    } 
  } 

  /**
   * Ensures a given expression of type <code>Double</code> is a literal with a valid value according to a given validation function.
   *
   * <p>
   * If the given <code>Double</code> expression is a literal whose value satisfies the given validation function, this method will
   * return normally. Otherwise, if the given <code>Double</code> expression is not a literal, this method will complete abruptly with
   * an exception whose detail message includes the <code>String</code> passed as <code>notLiteralMsg</code>. Otherwise, the
   * given <code>Double</code> expression is a literal that does <em>not</em> satisfy the given validation function, so this method will
   * complete abruptly with an exception whose detail message includes the <code>String</code> passed as <code>notValidMsg</code>.
   * </p>
   *
   * <p>
   * This method is intended to be invoked at compile time from macros. When called from a macro, exceptions thrown by this method
   * will result in compiler errors. The detail message of the thrown exception will appear as the compiler error message.
   * </p>
   *
   * @param c the compiler context for this assertion
   * @param value the <code>Double</code> expression to validate
   * @param notValidMsg a <code>String</code> message to include in the exception thrown if the expression is a literal, but not valid
   * @param notLiteralMsg a <code>String</code> message to include in the exception thrown if the expression is not a literal
   * @param isValid a function used to validate a literal value parsed from the given expression
   */
  def ensureValidDoubleLiteral(c: Context)(value: c.Expr[Double], notValidMsg: String, notLiteralMsg: String)(isValid: Double => Boolean): Unit = {

    import c.universe._

    value.tree match {
      case Literal(doubleConst) =>
        val literalValue = doubleConst.value.toString.toDouble
        if (!isValid(literalValue))
          c.abort(c.enclosingPosition, notValidMsg)
      case _ =>
        c.abort(c.enclosingPosition, notLiteralMsg)
    } 
  } 

  /**
   * Ensures a given expression of type <code>String</code> is a literal with a valid value according to a given validation function.
   *
   * <p>
   * If the given <code>String</code> expression is a literal whose value satisfies the given validation function, this method will
   * return normally. Otherwise, if the given <code>String</code> expression is not a literal, this method will complete abruptly with
   * an exception whose detail message includes the <code>String</code> passed as <code>notLiteralMsg</code>. Otherwise, the
   * given <code>String</code> expression is a literal that does <em>not</em> satisfy the given validation function, so this method will
   * complete abruptly with an exception whose detail message includes the <code>String</code> passed as <code>notValidMsg</code>.
   * </p>
   *
   * <p>
   * This method is intended to be invoked at compile time from macros. When called from a macro, exceptions thrown by this method
   * will result in compiler errors. The detail message of the thrown exception will appear as the compiler error message.
   * </p>
   *
   * @param c the compiler context for this assertion
   * @param value the <code>String</code> expression to validate
   * @param notValidMsg a <code>String</code> message to include in the exception thrown if the expression is a literal, but not valid
   * @param notLiteralMsg a <code>String</code> message to include in the exception thrown if the expression is not a literal
   * @param isValid a function used to validate a literal value parsed from the given expression
   */
  def ensureValidStringLiteral(c: Context)(value: c.Expr[String], notValidMsg: String, notLiteralMsg: String)(isValid: String => Boolean): Unit = {

    import c.universe._

    value.tree match {
      case Literal(stringConst) =>
        val literalValue = stringConst.value.toString
        if (!isValid(literalValue))
          c.abort(c.enclosingPosition, notValidMsg)
      case _ =>
        c.abort(c.enclosingPosition, notLiteralMsg)
    } 
  } 

  /**
   * Ensures a given expression of type <code>Char</code> is a literal with a valid value according to a given validation function.
   *
   * <p>
   * If the given <code>Char</code> expression is a literal whose value satisfies the given validation function, this method will
   * return normally. Otherwise, if the given <code>Char</code> expression is not a literal, this method will complete abruptly with
   * an exception whose detail message includes the <code>String</code> passed as <code>notLiteralMsg</code>. Otherwise, the
   * given <code>Char</code> expression is a literal that does <em>not</em> satisfy the given validation function, so this method will
   * complete abruptly with an exception whose detail message includes the <code>String</code> passed as <code>notValidMsg</code>.
   * </p>
   *
   * <p>
   * This method is intended to be invoked at compile time from macros. When called from a macro, exceptions thrown by this method
   * will result in compiler errors. The detail message of the thrown exception will appear as the compiler error message.
   * </p>
   *
   * @param c the compiler context for this assertion
   * @param value the <code>Char</code> expression to validate
   * @param notValidMsg a <code>String</code> message to include in the exception thrown if the expression is a literal, but not valid
   * @param notLiteralMsg a <code>String</code> message to include in the exception thrown if the expression is not a literal
   * @param isValid a function used to validate a literal value parsed from the given expression
   */
  def ensureValidCharLiteral(c: Context)(value: c.Expr[Char], notValidMsg: String, notLiteralMsg: String)(isValid: Char => Boolean): Unit = {

    import c.universe._

    value.tree match {
      case Literal(charConst) =>
        val literalValue = charConst.value.toString.head
        if (!isValid(literalValue))
          c.abort(c.enclosingPosition, notValidMsg)
      case _ =>
        c.abort(c.enclosingPosition, notLiteralMsg)
    } 
  } 
}

/**
 * Companion object that facilitates the importing of <code>CompileTimeAssertions</code> members as 
 * an alternative to mixing in the trait.
 */ 
object CompileTimeAssertions extends CompileTimeAssertions
