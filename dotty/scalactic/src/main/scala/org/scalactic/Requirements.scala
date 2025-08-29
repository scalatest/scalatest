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
package org.scalactic

import exceptions.NullArgumentException

import scala.quoted._

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
 * The <code>requireNonNull</code> method takes one or more variables as arguments and throws <code>NullArgumentException</code>
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
 * org.scalactic.exceptions.NullArgumentException: <strong>e and f were null</strong>
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

  import Requirements.requirementsHelper

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
  inline def require(inline condition: Boolean)(implicit prettifier: Prettifier): Unit =
    ${ RequirementsMacro.require('{condition}, '{prettifier}, '{""}) }

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
  inline def require(inline condition: Boolean, clue: Any)(implicit prettifier: Prettifier): Unit =
    ${ RequirementsMacro.require('{condition}, '{prettifier}, '{clue}) }

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
  inline def requireState(inline condition: Boolean)(implicit prettifier: Prettifier): Unit =
    ${ RequirementsMacro.requireState('{condition}, '{prettifier}, '{""}) }

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
  inline def requireState(inline condition: Boolean, clue: Any)(implicit prettifier: Prettifier): Unit =
    ${ RequirementsMacro.requireState('{condition}, '{prettifier}, '{clue}) }

  /**
   * Require that all passed arguments are non-null.
   *
   * <p>
   * If none of the passed arguments are <code>null</code>, this method returns normally.
   * Else, it throws <code>NullArgumentException</code> with an error message that includes the name
   * (as it appeared in the source) of each argument that was <code>null</code>.
   * </p>
   *
   * @param arguments arguments to check for <code>null</code> value
   * @throws NullArgumentException if any of the arguments are <code>null</code>.
   */
  inline def requireNonNull(arguments: Any*)(implicit prettifier: Prettifier, pos: source.Position): Unit =
    ${ RequirementsMacro.requireNonNull('{arguments}, '{prettifier}, '{pos}) }
}

// /**
//  * Macro implementation that provides rich error message for boolean expression requirements.
//  */
object RequirementsMacro {

  /**
   * Provides requirement implementation for <code>Requirements.require(booleanExpr: Boolean)</code>, with rich error message.
   *
   * @param context macro context
   * @param condition original condition expression
   * @return transformed expression that performs the requirement check and throw <code>IllegalArgumentException</code> with rich error message if requirement failed
   */
  def require(condition: Expr[Boolean], prettifier: Expr[Prettifier], clue: Expr[Any])(using Quotes): Expr[Unit] = {
    val bool = BooleanMacro.parse(condition, prettifier)
    '{ Requirements.requirementsHelper.macroRequire($bool, $clue) }
  }

  /**
   * Provides requirement implementation for <code>Requirements.requireState(booleanExpr: Boolean)</code>, with rich error message.
   *
   * @param context macro context
   * @param condition original condition expression
   * @return transformed expression that performs the requirement check and throw <code>IllegalStateException</code> with rich error message if requirement failed
   */
  def requireState(condition: Expr[Boolean], prettifier: Expr[Prettifier], clue: Expr[Any])(using Quotes): Expr[Unit] = {
    val bool = BooleanMacro.parse(condition, prettifier)
    '{ Requirements.requirementsHelper.macroRequireState($bool, $clue) }
  }

  /**
   * Provides requirement implementation for <code>Requirements.requireNonNull(arguments: Any*)</code>, with rich error message.
   *
   * @param arguments original arguments expression(s)
   * @param prettifier <code>Prettifier</code> to be used for error message
   * @return transformed expression that performs the requirement check and throw <code>NullArgumentException</code> with rich error message if requirement failed
   */
  def requireNonNull(arguments: Expr[Seq[Any]], prettifier: Expr[Prettifier], pos: Expr[source.Position])(using Quotes): Expr[Unit] = {
    import quotes.reflect._

    def liftSeq(args: Seq[Expr[String]]): Expr[Seq[String]] = args match {
      case x :: xs  => '{ ($x) +: ${ liftSeq(xs) }  }
      case Nil => '{ Seq(): Seq[String] }
    }

    val argStr: List[Expr[String]] = arguments.asTerm.underlyingArgument match {
      case Typed(Repeated(args, _), _) => // only sequence literal
        args.map(arg => Expr(arg.asExprOf[Any].show))
      case _ =>
        report.throwError("requireNonNull can only be used with sequence literal, not `seq : _*`")
    }

    // generate AST that create array containing the argument name in source (get from calling 'show')
    // for example, if you have:
    // val a = "1"
    // val b = null
    // val c = "3"
    // requireNonNull(a, b, c)
    // it will generate the following code:
    //
    // Array("a", "b", "c")
    val argumentsS: Expr[Seq[String]] = liftSeq(argStr)

    // generate AST that create array containing the argument values
    // for example, if you have:
    // val a = "1"
    // val b = null
    // val c = "3"
    // requireNonNull(a, b, c)
    // it will generate the following code:
    //
    // Array(a, b, c)
    // val argumentsArray = '{ $arguments.toArray }

    // Generate AST to call requirementsHelper.macroRequireNonNull and pass in both variable names and values array:
    //
    // requirementsHelper.macroRequireNonNull(variableNamesArray, valuesArray)
    '{ Requirements.requirementsHelper.macroRequireNonNull(($argumentsS).toArray, ($arguments).toArray, $prettifier, $pos) }
  }
}

/**
 * Companion object that facilitates the importing of <code>Requirements</code> members as
 * an alternative to mixing it in. One use case is to import <code>Requirements</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $scala -classpath scalatest.jar
 * Welcome to Scala 2.13.6 (OpenJDK 64-Bit Server VM, Java yyy).
 * Type in expressions for evaluation. Or try :help.
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
object Requirements extends Requirements {

  /**
    * Helper class used by code generated by the <code>require</code> macro.
    */
  class RequirementsHelper extends Serializable {

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
    def macroRequire(bool: Bool, clue: Any): Unit = {
      if (clue == null)
        throw new NullPointerException("clue was null")
      if (!bool.value) {
        val failureMessage = if (Bool.isSimpleWithoutExpressionText(bool)) append("", clue) else append(bool.failureMessage, clue)
        throw new IllegalArgumentException(if (failureMessage.isEmpty) FailureMessages.expressionWasFalse else failureMessage)
      }
    }

    /**
      * Require that the passed in <code>Bool</code> is <code>true</code>, else fail with <code>IllegalStateException</code>.
      *
      * @param bool the <code>Bool</code> to check as requirement
      * @param clue optional clue to be included in <code>IllegalStateException</code>'s error message when the requirement failed
      */
    def macroRequireState(bool: Bool, clue: Any): Unit = {
      if (clue == null)
        throw new NullPointerException("clue was null")
      if (!bool.value) {
        val failureMessage = if (Bool.isSimpleWithoutExpressionText(bool)) append("", clue) else append(bool.failureMessage, clue)
        throw new IllegalStateException(if (failureMessage.isEmpty) FailureMessages.expressionWasFalse else failureMessage)
      }
    }

    /**
      * Require that all of the passed in arguments are not <code>null</code>, else fail with <code>NullArgumentException</code>.
      *
      * @param variableNames names of variable passed as appear in source
      * @param arguments arguments to check for <code>null</code> value
      */
    def macroRequireNonNull(variableNames: Array[String], arguments: Array[Any], prettifier: Prettifier, pos: source.Position): Unit = {
      val nullList = arguments.zipWithIndex.filter { case (e, idx) =>
        e == null
      }
      val nullCount = nullList.size
      if (nullCount > 0) {
        val nullVariableNames = nullList.map { case (e, idx) =>
          variableNames(idx)
        }
        val errorMessage =
          if (nullCount == 1)
            FailureMessages.wasNull(prettifier, UnquotedString(nullVariableNames(0)))
          else if (nullCount == 2) {
            val combinedVariableNames = Resources.and(nullVariableNames.head, nullVariableNames.last)
            FailureMessages.wereNull(prettifier, UnquotedString(combinedVariableNames))
          }
          else {
            val combinedVariableNames = Resources.commaAnd(nullVariableNames.dropRight(1).mkString(Resources.comma), nullVariableNames.last)
            FailureMessages.wereNull(prettifier, UnquotedString(combinedVariableNames))
          }
        throw new NullArgumentException(errorMessage)
      }
    }

  }

  /**
    * Helper instance used by code generated by macro assertion.
    */
  val requirementsHelper = new RequirementsHelper

}

