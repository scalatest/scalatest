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
package org.scalactic

import reflect.macros.Context

/**
 * Trait that contains <code>require</code>, and <code>requireState</code>, and <code>requireNonNull</code> methods for checking pre-conditions
 * that give descriptive error messages extracted via a macro.
 * 
 * <p>These methods of trait <code>Requirements</code> aim to improve error messages provided when a pre-condition check fails at runtime in
 * production code. Although it is recommended practice to supply helpful error messages when doing pre-condition checks, often people
 * don't. Instead of this:
 * 
 * <pre class="stREPL">
 * scala&gt; val length = 5
 * length: Int = 5
 * 
 * scala&gt; val idx = 6
 * idx: Int = 6
 * 
 * scala&gt; require(idx &gt;= 0 &amp;&amp; idx &lt;= length, "index, " + idx + ", was less than zero or greater than or equal to length, " + length)
 * java.lang.IllegalArgumentException: <strong>requirement failed: index, 6, was less than zero or greater than or equal to length, 5</strong>
 * 	at scala.Predef$.require(Predef.scala:233)
 * 	...
 * </pre>
 * 
 * <p>
 * People write simply:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; require(idx &gt;= 0 &amp;&amp; idx &lt;= length)
 * java.lang.IllegalArgumentException: <strong>requirement failed</strong>
 * 	at scala.Predef$.require(Predef.scala:221)
 * 	...
 * </pre>
 * 
 * <p>
 * Note that the detail message of the <code>IllegalArgumentException</code> thrown by the previous line of code is simply, <code>"requirement failed"</code>.
 * Such messages often end up in a log file or bug report, where a better error message can save time in debugging the problem.
 * By importing the members of <code>Requirements</code> (or mixing in its companion trait), you'll get a more helpful error message
 * extracted by a macro, whether or not a clue message is provided:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 * 
 * scala&gt; import Requirements._
 * import Requirements._
 * 
 * scala&gt; require(idx &gt;= 0 &amp;&amp; idx &lt;= length)
 * java.lang.IllegalArgumentException: <strong>6 was greater than or equal to 0, but 6 was not less than or equal to 5</strong>
 * 	at org.scalactic.Requirements$RequirementsHelper.macroRequire(Requirements.scala:56)
 * 	...
 * 
 * scala&gt; require(idx &gt;= 0 &amp;&amp; idx &lt;= length, "(hopefully that helps)")
 * java.lang.IllegalArgumentException: <strong>6 was greater than or equal to 0, but 6 was not less than or equal to 5 (hopefully that helps)</strong>
 * 	at org.scalactic.Requirements$RequirementsHelper.macroRequire(Requirements.scala:56)
 * 	...
 * </pre>
 * 
 * <p>
 * The <code>requireState</code> method provides identical error messages to <code>require</code>, but throws
 * <code>IllegalStateException</code> instead of <code>IllegalArgumentException</code>:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; val connectionOpen = false
 * connectionOpen: Boolean = false
 * 
 * scala&gt; requireState(connectionOpen)
 * java.lang.IllegalStateException: <strong>connectionOpen was false</strong>
 * 	at org.scalactic.Requirements$RequirementsHelper.macroRequireState(Requirements.scala:71)
 * 	...
 * </pre>
 * 
 * <p>
 * Thus, whereas the <code>require</code> methods throw the Java platform's standard exception indicating a passed argument
 * violated a precondition, <code>IllegalArgumentException</code>, the <code>requireState</code> methods throw the standard
 * exception indicating an object's method was invoked when the object was in an inappropriate state for that method,
 * <code>IllegalStateException</code>.
 * </p>
 * 
 * <p>
 * The <code>requireNonNull</code> method takes one or more variables as arguments and throws <code>NullPointerException</code>
 * with an error messages that includes the variable names if any are <code>null</code>. Here's an example:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; val e: String = null
 * e: String = null
 * 
 * scala&gt; val f: java.util.Date = null
 * f: java.util.Date = null
 * 
 * scala&gt; requireNonNull(a, b, c, d, e, f)
 * java.lang.NullPointerException: <strong>e and f were null</strong>
 * 	at org.scalactic.Requirements$RequirementsHelper.macroRequireNonNull(Requirements.scala:101)
 * 	...
 * </pre>
 * 
 * <p>
 * Although trait <code>Requirements</code> can help you debug problems that occur in production, bear in mind that a much
 * better alternative is to make it impossible for such events to occur at all. Use the type system to ensure that all
 * pre-conditions are met so that the compiler can find broken pre-conditions and point them out with compiler error messages.
 * When this is not possible or practical, however, trait <code>Requirements</code> is helpful.
 * </p>
 */
trait Requirements {

  import language.experimental.macros

  /**
   * Helper class used by code generated by the <code>require</code> macro.
   */
  class RequirementsHelper {

    private def append(currentMessage: String, clue: Any): String = {
      val clueStr = clue.toString
      if (clueStr.isEmpty)
        currentMessage
      else {
        val firstChar = clueStr.head
        if (firstChar.isWhitespace || firstChar == '.' || firstChar == ',' || firstChar == ';' || currentMessage.isEmpty)
          currentMessage + clueStr
        else
          currentMessage + " " + clueStr
      }
    }

    /**
     * Require that the passed in <code>Bool</code> is <code>true</code>, else fail with <code>IllegalArgumentException</code>.
     *
     * @param bool the <code>Bool</code> to check as requirement
     * @param clue optional clue to be included in <code>IllegalArgumentException</code>'s error message when the requirement failed
     */
    def macroRequire(bool: Bool, clue: Any) {
      if (clue == null)
        throw new NullPointerException("clue was null")
      if (!bool.value) {
        val failureMessage = if (Bool.isSimpleWithoutExpressionText(bool)) append("", clue) else append(bool.failureMessage, clue)
        throw new IllegalArgumentException(if (failureMessage.isEmpty) FailureMessages("expressionWasFalse") else failureMessage)
      }
    }

    /**
     * Require that the passed in <code>Bool</code> is <code>true</code>, else fail with <code>IllegalStateException</code>.
     *
     * @param bool the <code>Bool</code> to check as requirement
     * @param clue optional clue to be included in <code>IllegalStateException</code>'s error message when the requirement failed
     */
    def macroRequireState(bool: Bool, clue: Any) {
      if (clue == null)
        throw new NullPointerException("clue was null")
      if (!bool.value) {
        val failureMessage = if (Bool.isSimpleWithoutExpressionText(bool)) append("", clue) else append(bool.failureMessage, clue)
        throw new IllegalStateException(if (failureMessage.isEmpty) FailureMessages("expressionWasFalse") else failureMessage)
      }
    }

    /**
     * Require that all of the passed in arguments are not <code>null</code>, else fail with <code>NullPointerException</code>.
     *
     * @param variableNames names of variable passed as appear in source
     * @param arguments arguments to check for <code>null</code> value
     */
    def macroRequireNonNull(variableNames: Array[String], arguments: Array[Any]) {
      val nullList: Array[(Any, Int)] = arguments.zipWithIndex.filter { case (e, idx) =>
        e == null
      }
      val nullCount = nullList.size
      if (nullCount > 0) {
        val nullVariableNames = nullList.map { case (e, idx) =>
          variableNames(idx)
        }
        val errorMessage =
          if (nullCount == 1)
            FailureMessages("wasNull", UnquotedString(nullVariableNames(0)))
          else if (nullCount == 2) {
            val combinedVariableNames = Resources("and", nullVariableNames.head, nullVariableNames.last)
            FailureMessages("wereNull", UnquotedString(combinedVariableNames))
          }
          else {
            val combinedVariableNames = Resources("commaAnd", nullVariableNames.dropRight(1).mkString(Resources("comma")), nullVariableNames.last)
            FailureMessages("wereNull", UnquotedString(combinedVariableNames))
          }
        throw new NullPointerException(errorMessage)
      }
    }

  }

  /**
   * Helper instance used by code generated by macro assertion.
   */
  val requirementsHelper = new RequirementsHelper

  /**
   * Require that a boolean condition is true about an argument passed to a method, function, or constructor.
   *
   * <p>
   * If the condition is <code>true</code>, this method returns normally.
   * Else, it throws <code>IllegalArgumentException</code>.
   * </p>
   *
   * <p>
   * This method is implemented in terms of a Scala macro that will generate an error message.
   * See the main documentation for this trait for examples.
   * </p>
   *
   * @param condition the boolean condition to check as requirement
   * @throws IllegalArgumentException if the condition is <code>false</code>.
   */
  def require(condition: Boolean): Unit = macro RequirementsMacro.require

  /**
   * Require that a boolean condition about an argument passed to a method, function, or constructor,
   * and described in the given <code>clue</code>, is true.
   *
   * If the condition is <code>true</code>, this method returns normally.
   * Else, it throws <code>IllegalArgumentException</code> with the
   * <code>String</code> obtained by invoking <code>toString</code> on the
   * specified <code>clue</code> and appending that to the macro-generated
   * error message as the exception's detail message.
   *
   * @param condition the boolean condition to check as requirement
   * @param clue an objects whose <code>toString</code> method returns a message to include in a failure report.
   * @throws IllegalArgumentException if the condition is <code>false</code>.
   * @throws NullPointerException if <code>message</code> is <code>null</code>.
   */
  def require(condition: Boolean, clue: Any): Unit = macro RequirementsMacro.requireWithClue

  /**
   * Require that a boolean condition is true about the state of an object on which a method has been invoked.
   *
   * <p>
   * If the condition is <code>true</code>, this method returns normally.
   * Else, it throws <code>IllegalStateException</code>.
   * </p>
   *
   * <p>
   * This method is implemented in terms of a Scala macro that will generate an error message.
   * </p>
   *
   * @param condition the boolean condition to check as requirement
   * @throws IllegalStateException if the condition is <code>false</code>.
   */
  def requireState(condition: Boolean): Unit = macro RequirementsMacro.requireState

  /**
   * Require that a boolean condition about the state of an object on which a method has been
   * invoked, and described in the given <code>clue</code>, is true.
   *
   * <p>
   * If the condition is <code>true</code>, this method returns normally.
   * Else, it throws <code>IllegalStateException</code> with the
   * <code>String</code> obtained by invoking <code>toString</code> on the
   * specified <code>clue</code> appended to the macro-generated error message
   * as the exception's detail message.
   * </p>
   *
   * @param condition the boolean condition to check as a requirement
   * @param clue an object whose <code>toString</code> method returns a message to include in a failure report.
   * @throws IllegalStateException if the condition is <code>false</code>.
   * @throws NullPointerException if <code>message</code> is <code>null</code>.
   */
  def requireState(condition: Boolean, clue: Any): Unit = macro RequirementsMacro.requireStateWithClue

  /**
   * Require that all passed arguments are non-null.
   *
   * <p>
   * If none of the passed arguments are <code>null</code>, this method returns normally.
   * Else, it throws <code>NullPointerException</code> with an error message that includes the name
   * (as it appeared in the source) of each argument that was <code>null</code>.
   * </p>
   *
   * @param arguments arguments to check for <code>null</code> value
   * @throws NullPointerException if any of the arguments are <code>null</code>.
   */
  def requireNonNull(arguments: Any*): Unit = macro RequirementsMacro.requireNonNull
}

/**
 * Macro implementation that provides rich error message for boolean expression requirements.
 */
private[scalactic] object RequirementsMacro {

  /**
   * Provides requirement implementation for <code>Requirements.require(booleanExpr: Boolean)</code>, with rich error message.
   *
   * @param context macro context
   * @param condition original condition expression
   * @return transformed expression that performs the requirement check and throw <code>IllegalArgumentException</code> with rich error message if requirement failed
   */
  def require(context: Context)(condition: context.Expr[Boolean]): context.Expr[Unit] =
    new BooleanMacro[context.type](context, "requirementsHelper").genMacro(condition, "macroRequire", context.literal(""))

  /**
   * Provides requirement implementation for <code>Requirements.require(booleanExpr: Boolean, clue: Any)</code>, with rich error message.
   *
   * @param context macro context
   * @param condition original condition expression
   * @param clue original clue expression
   * @return transformed expression that performs the requirement check and throw <code>IllegalArgumentException</code> with rich error message (clue included) if requirement failed
   */
  def requireWithClue(context: Context)(condition: context.Expr[Boolean], clue: context.Expr[Any]): context.Expr[Unit] =
    new BooleanMacro[context.type](context, "requirementsHelper").genMacro(condition, "macroRequire", clue)

  /**
   * Provides requirement implementation for <code>Requirements.requireState(booleanExpr: Boolean)</code>, with rich error message.
   *
   * @param context macro context
   * @param condition original condition expression
   * @return transformed expression that performs the requirement check and throw <code>IllegalStateException</code> with rich error message if requirement failed
   */
  def requireState(context: Context)(condition: context.Expr[Boolean]): context.Expr[Unit] =
    new BooleanMacro[context.type](context, "requirementsHelper").genMacro(condition, "macroRequireState", context.literal(""))

  /**
   * Provides requirement implementation for <code>Requirements.requireState(booleanExpr: Boolean, clue: Any)</code>, with rich error message.
   *
   * @param context macro context
   * @param condition original condition expression
   * @param clue original clue expression
   * @return transformed expression that performs the requirement check and throw <code>IllegalStateException</code> with rich error message (clue included) if requirement failed
   */
  def requireStateWithClue(context: Context)(condition: context.Expr[Boolean], clue: context.Expr[Any]): context.Expr[Unit] =
    new BooleanMacro[context.type](context, "requirementsHelper").genMacro(condition, "macroRequireState", clue)

  /**
   * Provides requirement implementation for <code>Requirements.requireNonNull(arguments: Any*)</code>, with rich error message.
   *
   * @param context macro context
   * @param arguments original arguments expression(s)
   * @return transformed expression that performs the requirement check and throw <code>NullPointerException</code> with rich error message if requirement failed
   */
  def requireNonNull(context: Context)(arguments: context.Expr[Any]*): context.Expr[Unit] = {
    import context.universe._

    // generate AST that create array containing the argument name in source (get from calling 'show')
    // for example, if you have:
    // val a = "1"
    // val b = null
    // val c = "3"
    // requireNonNull(a, b, c)
    // it will generate the following code:
    //
    // Array("a", "b", "c")
    val variablesNamesArray =
      Apply(
        Select(
          Ident("Array"),
          newTermName("apply")
        ),
        List(arguments.map(e => context.literal(show(e.tree)).tree): _*)
      )

    // generate AST that create array containing the argument values
    // for example, if you have:
    // val a = "1"
    // val b = null
    // val c = "3"
    // requireNonNull(a, b, c)
    // it will generate the following code:
    //
    // Array(a, b, c)
    val argumentsArray =
      Apply(
        Select(
          Ident("Array"),
          newTermName("apply")
        ),
        List(arguments.map(e => e.tree): _*)
      )

    // Generate AST to call requirementsHelper.macroRequireNonNull and pass in both variable names and values array:
    //
    // requirementsHelper.macroRequireNonNull(variableNamesArray, valuesArray)
    context.Expr(
      Apply(
        Select(
          Ident("requirementsHelper"),
          newTermName("macroRequireNonNull")
        ),
        List(variablesNamesArray, argumentsArray)
      )
    )
  }
}

/**
 * Companion object that facilitates the importing of <code>Requirements</code> members as
 * an alternative to mixing it in. One use case is to import <code>Requirements</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $scala -classpath scalatest.jar
 * Welcome to Scala version 2.10.3.final (Java HotSpot(TM) Client VM, Java xxxxxx).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 * &nbsp;
 * scala&gt; import org.scalactic.Requirements._
 * import org.scalactic.Requirements._
 * &nbsp;
 * scala&gt; val a = 1
 * a: Int = 1
 * &nbsp;
 * scala&gt; require(a == 2)
 * java.lang.IllegalArgumentException: 1 did not equal 2
 *      at org.scalactic.Requirements$RequirementsHelper.macroRequire(Requirements.scala:56)
 *      at .&lt;init&gt;(&lt;console&gt;:20)
 *      at .&lt;clinit&gt;(&lt;console&gt;)
 *      at .&lt;init&gt;(&lt;console&gt;:7)
 *      at .&lt;clinit&gt;(&lt;console&gt;)
 *      at $print(&lt;console&gt;)
 *      at sun.reflect.NativeMethodAccessorImpl.invoke...
 */
object Requirements extends Requirements

