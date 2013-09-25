/*
 * Copyright 2001-2013 Artima, Inc.
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
package org.scalautils

import scala.collection.mutable.WrappedArray

/**
 * A function that given any object will produce a &ldquo;pretty&rdquo; string representation of that object,
 * where &ldquo;pretty&rdquo; is in the eye of the implementer. 
 *
 * <p>
 * Scala's <code>Any</code> type declares a <code>toString</code> that will convert any object to a <code>String</code>
 * representation. This <code>String</code> representation is primarily intended for programmers, and is usually sufficient.
 * However, sometimes it can be helpful to provide an alternative implementation of <code>toString</code> for certain types.
 * For example, the <code>toString</code> implementation on <code>String</code> prints out the value of the <code>String</code>:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; "1".toString
 * res0: String = 1
 * </pre>
 *
 * <p>
 * If the error message that resulted from comparing <code>Int</code> 1 with <code>String</code> <code>"1"</code>
 * in a ScalaTest assertion used <code>toString</code>, therefore, the error message would be:
 * </p>
 *
 * <pre>
 * 1 did not equal 1
 * </pre>
 *
 * <p>
 * To make it quicker to figure out why the assertion failed, ScalaTest <em>prettifies</em> the objects involved in 
 * the error message. The default <code>Prettifier</code> will place double quotes on either side of a <code>String</code>s
 * <code>toString</code> result:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalautils._
 * import org.scalautils._
 *
 * scala&gt; Prettifier.default("1")
 * res1: String = "1"
 * </pre>
 *
 * <p>
 * Thus the error message resulting from comparing <code>Int</code> 1 with <code>String</code> <code>"1"</code>,
 * in a ScalaTest assertion is:
 * </p>
 *
 * <pre>
 * 1 did not equal "1"
 * </pre>
 *
 * <p>
 * If you wish to prettify an object in production code, for example, to issue a profoundly clear debug message, you can use
 * <code>PrettyMethods</code> and invoke <code>pretty</code>. Here's an example:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import PrettyMethods._
 * import PrettyMethods._
 *
 * scala&gt; 1.pretty
 * res2: String = 1
 *
 * scala&gt; "1".pretty
 * res3: String = "1"
 * </pre>
 *
 * <p>
 * Note: <code>Prettifier</code> is not parameterized (<em>i.e.</em>, <code>Prettifier[T]</code>, where <code>T</code> is the type
 * to prettify) because assertions (including matcher expressions) in ScalaTest would then need to look up <code>Prettifier</code>s implicitly by type. This would slow
 * compilation even though most (let's guess 99.9%) of the time in practice assertions do not fail, and thus 99.9% of the time no error messages need to be generated.
 * If no error messages are needed 99.9% of the time, no prettification is needed 99.9% of the time, so the slow down in compile time for the implicit
 * look ups is unlikely to be worth the benefit. Only a few types in practice usually need prettification for testing error message purposes, and those will be covered
 * by the default <code>Prettifier</code>. A future version of ScalaTest will provide a simple mechanism to replace the default <code>Prettifier</code> with a
 * custom one when a test actually fails.
 * </p>
 */
trait Prettifier extends (Any => String)

/**
 * Companion object for <code>Prettifier</code> that provides a default <code>Prettifier</code> implementation.
 */
object Prettifier {

  /**
   * A default <code>Prettifier</code>. 
   *
   * <p>
   * This default <code>Prettifier</code> is used in ScalaTest to clarify error messages.
   * </p>
   *
   * <p>
   * It transforms:
   * </p>
   *
   * <ul>
   * <li><code>Null</code> to: <code>null</code></li>
   * <li><code>Unit</code> to: <code>&lt;() the Unit value&gt;</code></li>
   * <li><code>String</code> to: <code>"string"</code> (the <code>toString</code> result surrounded by double quotes)</li>
   * <li><code>Char</code> to: <code>'c'</code> (the <code>toString</code> result surrounded by single quotes)</li>
   * <li><code>Array</code> to: <code>Array(1, 2, 3)</code></li>
   * </ul>
   *
   * <p>
   * For anything else, it returns the result of invoking <code>toString</code>.
   * </p>
   */
  val default: Prettifier =
    new Prettifier {
      def apply(o: Any): String =
        o match {
          case null => "null"
          case aUnit: Unit => "<(), the Unit value>"
          case aString: String => "\"" + aString + "\""
          case aChar: Char =>  "\'" + aChar + "\'"
          case anArray: Array[_] =>  prettifyArrays(anArray)
          case aWrappedArray: WrappedArray[_] => prettifyArrays(aWrappedArray)
          case anythingElse => anythingElse.toString
        }
    }

  private def prettifyArrays(o: Any): String = {
    o match {
      case arr: Array[_] => "Array(" + (arr map (a => prettifyArrays(a))).mkString(", ") + ")"
      case wrappedArr: WrappedArray[_] => "Array(" + (wrappedArr map (a => prettifyArrays(a))).mkString(", ") + ")"
      case _ => if (o != null) o.toString else "null"
    }
  }
}
