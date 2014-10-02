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
package org.scalactic

import scala.collection._
import mutable.WrappedArray
import scala.util.Success
import scala.xml

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
 * scala&gt; import org.scalactic._
 * import org.scalactic._
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
 * For example, the default <code>Prettifier</code>, <a href="Prettifier$.html"><code>Prettifier.default</code></a>, transforms:
 * </p>
 *
 * <ul>
 * <li><code>Null</code> to: <code>null</code></li>
 * <li><code>Unit</code> to: <code>&lt;() the Unit value&gt;</code></li>
 * <li><code>String</code> to: <code>"string"</code> (the <code>toString</code> result surrounded by double quotes)</li>
 * <li><code>Char</code> to: <code>'c'</code> (the <code>toString</code> result surrounded by single quotes)</li>
 * <li><code>Array</code> to: <code>Array("1", "2", "3")</code></li>
 * <li><code>scala.Some</code> to: <code>Some("3")</code></li>
 * <li><code>scala.util.Left</code> to: <code>Left("3")</code></li>
 * <li><code>scala.util.Right</code> to: <code>Right("3")</code></li>
 * <li><code>scala.util.Success</code> to: <code>Success("3")</code></li>
 * <li><code>org.scalactic.Good</code> to: <code>Good("3")</code></li>
 * <li><code>org.scalactic.Bad</code> to: <code>Bad("3")</code></li>
 * <li><code>org.scalactic.One</code> to: <code>One("3")</code></li>
 * <li><code>org.scalactic.Many</code> to: <code>Many("1", "2", "3")</code></li>
 * <li><code>scala.collection.GenTraversable</code> to: <code>List("1", "2", "3")</code></li>
 * <li><code>java.util.Collection</code> to: <code>["1", "2", "3"]</code></li>
 * <li><code>java.util.Map</code> to: <code>{1="one", 2="two", 3="three"}</code></li>
 * </ul>
 *
 * <p>
 * For anything else, the default <code>Prettifier</code> returns the result of invoking <code>toString</code>.
 * </p>
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
   * <li><code>Array</code> to: <code>Array("1", "2", "3")</code></li>
   * <li><code>scala.Some</code> to: <code>Some("3")</code></li>
   * <li><code>scala.util.Left</code> to: <code>Left("3")</code></li>
   * <li><code>scala.util.Right</code> to: <code>Right("3")</code></li>
   * <li><code>scala.util.Success</code> to: <code>Success("3")</code></li>
   * <li><code>org.scalactic.Good</code> to: <code>Good("3")</code></li>
   * <li><code>org.scalactic.Bad</code> to: <code>Bad("3")</code></li>
   * <li><code>org.scalactic.One</code> to: <code>One("3")</code></li>
   * <li><code>org.scalactic.Many</code> to: <code>Many("1", "2", "3")</code></li>
   * <li><code>scala.collection.GenTraversable</code> to: <code>List("1", "2", "3")</code></li>
   * <li><code>java.util.Collection</code> to: <code>["1", "2", "3"]</code></li>
   * <li><code>java.util.Map</code> to: <code>{1="one", 2="two", 3="three"}</code></li>
   * </ul>
   *
   *
   * <p>
   * For anything else, it returns the result of invoking <code>toString</code>.
   * </p>
   */
  val default: Prettifier =
    new Prettifier {
      def apply(o: Any): String = {
        try {
          o match {
            case null => "null"
            case aUnit: Unit => "<(), the Unit value>"
            case aString: String => "\"" + aString + "\""
            case aStringWrapper: scala.collection.immutable.StringOps => "\"" + aStringWrapper + "\""
            case aChar: Char =>  "\'" + aChar + "\'"
            case Some(e) => "Some(" + apply(e) + ")"
            case Success(e) => "Success(" + apply(e) + ")"
            case Left(e) => "Left(" + apply(e) + ")"
            case Right(e) => "Right(" + apply(e) + ")"
            case Good(e) => "Good(" + apply(e) + ")"
            case Bad(e) => "Bad(" + apply(e) + ")"
            case One(e) => "One(" + apply(e) + ")"
            case many: Many[_] => "Many(" + many.toIterator.map(apply(_)).mkString(", ") + ")"
            case equaBox: EquaSets[_]#EquaBox => "EquaBox(" + apply(equaBox.value) + ")"
            case equaSet: EquaSets[_]#EquaSet =>
              val isSelf =
                if (equaSet.size == 1) {
                  equaSet.head match {
                    case ref: AnyRef => ref eq equaSet
                    case other => other == equaSet
                  }
                }
                else
                  false
              if (isSelf)
                equaSet.toString
              else
                equaSet.stringPrefix + "(" + equaSet.toIterator.map(apply(_)).mkString(", ") + ")" // toIterator is needed for consistent ordering

            case anArray: Array[_] =>  "Array(" + (anArray map apply).mkString(", ") + ")"
            case aWrappedArray: WrappedArray[_] => "Array(" + (aWrappedArray map apply).mkString(", ") + ")"
            case aGenMap: GenMap[_, _] =>
              aGenMap.stringPrefix + "(" +
              (aGenMap.toIterator.map { case (key, value) => // toIterator is needed for consistent ordering
                apply(key) + " -> " + apply(value)
              }).mkString(", ") + ")"
            case anXMLNodeSeq: xml.NodeSeq => anXMLNodeSeq.toString
            case anXMLNodeBuffer: xml.NodeBuffer =>
              xml.NodeSeq.fromSeq(anXMLNodeBuffer).toString
            case aGenTraversable: GenTraversable[_] =>
              val isSelf =
                if (aGenTraversable.size == 1) {
                  aGenTraversable.head match {
                    case ref: AnyRef => ref eq aGenTraversable
                    case other => other == aGenTraversable
                  }
                }
                else
                  false
              if (isSelf)
                aGenTraversable.toString
              else
                aGenTraversable.stringPrefix + "(" + aGenTraversable.toIterator.map(apply(_)).mkString(", ") + ")" // toIterator is needed for consistent ordering
            case javaCol: java.util.Collection[_] =>
              // By default java collection follows http://download.java.net/jdk7/archive/b123/docs/api/java/util/AbstractCollection.html#toString()
              // let's do our best to prettify its element when it is not overriden
              import scala.collection.JavaConverters._
              val theToString = javaCol.toString
              if (theToString.startsWith("[") && theToString.endsWith("]"))
                "[" + javaCol.iterator().asScala.map(apply(_)).mkString(", ") + "]"
              else
                theToString
            case javaMap: java.util.Map[_, _] =>
              // By default java map follows http://download.java.net/jdk7/archive/b123/docs/api/java/util/AbstractMap.html#toString()
              // let's do our best to prettify its element when it is not overriden
              import scala.collection.JavaConverters._
              val theToString = javaMap.toString
              if (theToString.startsWith("{") && theToString.endsWith("}"))
                "{" + javaMap.entrySet.iterator.asScala.map { entry =>
                  apply(entry.getKey) + "=" + apply(entry.getValue)
                }.mkString(", ") + "}"
              else
                theToString
            case anythingElse => anythingElse.toString
          }
        }
        catch {
          // This is in case of crazy designs like the one for scala.xml.Node. We handle Node
          // specially above, but in case someone else creates a collection whose iterator
          // returns itself, which will cause infinite recursion, at least we'll pop out and
          // give them a string back.
          case _: StackOverflowError => o.toString
        }
      }
    }

  /**
   * A basic <code>Prettifier</code>.
   *
   * <p>
   * This was the default <code>Prettifier</code> used in ScalaTest 2.0 release.
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
   * <li><code>Array</code> to: <code>Array("1", "2", "3")</code></li>
   * <li><code>scala.util.Some</code> to: <code>Some("3")</code></li>
   * </ul>
   *
   * <p>
   * For anything else, it returns the result of invoking <code>toString</code>.
   * </p>
   */
  val basic = new BasicPrettifier

  private[org] def diffStrings(s: String, t: String): Tuple2[String, String] = {
    def findCommonPrefixLength(s: String, t: String): Int = {
      val max = s.length.min(t.length) // the maximum potential size of the prefix
      var i = 0
      var found = false
      while (i < max & !found) {
        found = (s.charAt(i) != t.charAt(i))
        if (!found)
          i = i + 1
      }
      i
    }
    def findCommonSuffixLength(s: String, t: String): Int = {
      val max = s.length.min(t.length) // the maximum potential size of the suffix
      var i = 0
      var found = false
      while (i < max & !found) {
        found = (s.charAt(s.length - 1 - i) != t.charAt(t.length - 1 - i))
        if (!found)
          i = i + 1
      }
      i
    }
    if (s != t) {
      val commonPrefixLength = findCommonPrefixLength(s, t)
      val commonSuffixLength = findCommonSuffixLength(s.substring(commonPrefixLength), t.substring(commonPrefixLength))
      val prefix = s.substring(0, commonPrefixLength)
      val suffix = if (s.length - commonSuffixLength < 0) "" else s.substring(s.length - commonSuffixLength)
      val sMiddleEnd = s.length - commonSuffixLength
      val tMiddleEnd = t.length - commonSuffixLength
      val sMiddle = s.substring(commonPrefixLength, sMiddleEnd)
      val tMiddle = t.substring(commonPrefixLength, tMiddleEnd)
      val MaxContext = 20
      val shortPrefix = if (commonPrefixLength > MaxContext) "..." + prefix.substring(prefix.length - MaxContext) else prefix
      val shortSuffix = if (commonSuffixLength > MaxContext) suffix.substring(0, MaxContext) + "..." else suffix
      (shortPrefix + "[" + sMiddle + "]" + shortSuffix, shortPrefix + "[" + tMiddle + "]" + shortSuffix)
    }
    else
      (s, t)
  }

  private[org] def getObjectsForFailureMessage(a: Any, b: Any) =
    a match {
      case aStr: String => {
        b match {
          case bStr: String => {
            diffStrings(aStr, bStr)
          }
          case _ => (a, b)
        }
      }
      case _ => (a, b)
    }
	
  private[org] val lineSeparator: String = System.getProperty("line.separator")
}

private[scalactic] class BasicPrettifier extends Prettifier {

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

  private def prettifyArrays(o: Any): String =
    o match {
      case arr: Array[_] => "Array(" + (arr map (a => prettifyArrays(a))).mkString(", ") + ")"
      case wrappedArr: WrappedArray[_] => "Array(" + (wrappedArr map (a => prettifyArrays(a))).mkString(", ") + ")"
      case _ => if (o != null) o.toString else "null"
    }
}
