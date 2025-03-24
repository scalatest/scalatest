/*
 * Copyright 2001-2024 Artima, Inc.
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

/**
 * A function that given any object will produce a &ldquo;pretty&rdquo; string representation of that object,
 * where &ldquo;pretty&rdquo; is in the eye of the implementer. 
 *
 * <p>
 * Scala's `Any` type declares a `toString` that will convert any object to a `String`
 * representation. This `String` representation is primarily intended for programmers, and is usually sufficient.
 * However, sometimes it can be helpful to provide an alternative implementation of `toString` for certain types.
 * For example, the `toString` implementation on `String` prints out the value of the `String`:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; "1".toString
 * res0: String = 1
 * </pre>
 *
 * <p>
 * If the error message that resulted from comparing `Int` 1 with `String` `"1"`
 * in a ScalaTest assertion used `toString`, therefore, the error message would be:
 * </p>
 *
 * <pre>
 * 1 did not equal 1
 * </pre>
 *
 * <p>
 * To make it quicker to figure out why the assertion failed, ScalaTest ''prettifies'' the objects involved in 
 * the error message. The default `Prettifier` will place double quotes on either side of a `String`s
 * `toString` result:
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
 * Thus the error message resulting from comparing `Int` 1 with `String` `"1"`,
 * in a ScalaTest assertion is:
 * </p>
 *
 * <pre>
 * 1 did not equal "1"
 * </pre>
 *
 * <p>
 * If you wish to prettify an object in production code, for example, to issue a profoundly clear debug message, you can use
 * `PrettyMethods` and invoke `pretty`. Here's an example:
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
 * For example, the default `Prettifier`, <a href="Prettifier$.html">`Prettifier.default`</a>, transforms:
 * </p>
 *
 * <ul>
 * <li>`Null` to: `null`</li>
 * <li>`Unit` to: `&lt;() the Unit value&gt;`</li>
 * <li>`String` to: `"string"` (the `toString` result surrounded by double quotes)</li>
 * <li>`Char` to: `'c'` (the `toString` result surrounded by single quotes)</li>
 * <li>`Array` to: `Array("1", "2", "3")`</li>
 * <li>`scala.Some` to: `Some("3")`</li>
 * <li>`scala.util.Left` to: `Left("3")`</li>
 * <li>`scala.util.Right` to: `Right("3")`</li>
 * <li>`scala.util.Success` to: `Success("3")`</li>
 * <li>`org.scalactic.Good` to: `Good("3")`</li>
 * <li>`org.scalactic.Bad` to: `Bad("3")`</li>
 * <li>`org.scalactic.One` to: `One("3")`</li>
 * <li>`org.scalactic.Many` to: `Many("1", "2", "3")`</li>
 * <li>`scala.collection.Iterable` to: `List("1", "2", "3")`</li>
 * <li>`java.util.Collection` to: `["1", "2", "3"]`</li>
 * <li>`java.util.Map` to: `{1="one", 2="two", 3="three"}`</li>
 * </ul>
 *
 * <p>
 * For anything else, the default `Prettifier` returns the result of invoking `toString`.
 * </p>
 *
 * <p>
 * Note: `Prettifier` is not parameterized (''i.e.'', `Prettifier[T]`, where `T` is the type
 * to prettify) because assertions (including matcher expressions) in ScalaTest would then need to look up `Prettifier`s implicitly by type. This would slow
 * compilation even though most (let's guess 99.9%) of the time in practice assertions do not fail, and thus 99.9% of the time no error messages need to be generated.
 * If no error messages are needed 99.9% of the time, no prettification is needed 99.9% of the time, so the slow down in compile time for the implicit
 * look ups is unlikely to be worth the benefit. Only a few types in practice usually need prettification for testing error message purposes, and those will be covered
 * by the default `Prettifier`. A future version of ScalaTest will provide a simple mechanism to replace the default `Prettifier` with a
 * custom one when a test actually fails.
 * </p>
 */
trait Prettifier extends Serializable { // I removed the extends (Any => String), now that we are making this implicit.

  /**
   * Prettifies the passed object.
   */
  def apply(o: Any): String

  /**
   * Prettifies the passed <code>left</code> and <code>right</code>.
   * 
   * @param left the left object.
   * @param right the right object.
   * @return a <code>PrettyPair</code> that contains the prettified <code>left</code> and <code>right</code>, with optional analysis.
   */
  def apply(left: Any, right: Any): PrettyPair = {
    AnyDiffer.difference(left, right, this)
  }
}

private[scalactic] class DefaultPrettifier extends Prettifier {

  protected def prettifyCollection(o: Any, processed: Set[Any]): String = 
    o match {
      case many: Many[_] => "Many(" + many.toIterator.map(prettify(_, processed + many)).mkString(", ") + ")"
      case anArray: Array[_] =>  "Array(" + anArray.map(prettify(_, processed + anArray)).mkString(", ") + ")"
      case aWrappedArray: WrappedArray[_] => "Array(" + aWrappedArray.map(prettify(_, processed + aWrappedArray)).mkString(", ") + ")"
      case a if ArrayHelper.isArrayOps(a) => 
        val anArrayOps = ArrayHelper.asArrayOps(a).iterator
        "Array(" + anArrayOps.map(prettify(_, processed + anArrayOps)).mkString(", ") + ")"
      case i: Iterable[_] =>
        val className = i.getClass.getName
        if (className.startsWith("scala.xml.NodeSeq$") || className == "scala.xml.NodeBuffer" || className == "scala.xml.Elem")
          i.mkString  
        else
          ColCompatHelper.className(i) + "(" + i.toIterator.map { 
            case (key, value) if className.contains("Map") => prettify(key, processed + i) + " -> " + prettify(value, processed + i)
            case other => prettify(other, processed + i) 
          }.mkString(", ") + ")" // toIterator is needed for consistent ordering                      
      case javaCol: java.util.Collection[_] =>
        // By default java collection follows http://download.java.net/jdk7/archive/b123/docs/api/java/util/AbstractCollection.html#toString()
        // let's do our best to prettify its element when it is not overriden
        import scala.collection.JavaConverters._
        val theToString = javaCol.toString
        if (theToString.startsWith("[") && theToString.endsWith("]")) {
          val itr = javaCol.iterator().asScala
          "[" + itr.map(prettify(_, processed + javaCol)).mkString(", ") + "]"
        }
        else
          theToString
      case javaMap: java.util.Map[_, _] =>
        // By default java map follows http://download.java.net/jdk7/archive/b123/docs/api/java/util/AbstractMap.html#toString()
        // let's do our best to prettify its element when it is not overriden
        import scala.collection.JavaConverters._
        val theToString = javaMap.toString
        if (theToString.startsWith("{") && theToString.endsWith("}")) {
          val itr = javaMap.entrySet.iterator.asScala
          "{" + itr.map { entry =>
            prettify(entry.getKey, processed + javaMap) + "=" + prettify(entry.getValue, processed + javaMap)
          }.mkString(", ") + "}"
        }
        else
          theToString
      case caseClazz: Product if caseClazz.productArity != 0 =>
        // If the case class's toString starts with .productPrefix its likely the .toString hasn't been
        // overridden so lets use our custom prettifying otherwise we just use .toString.
        if (caseClazz.toString.startsWith(s"${caseClazz.productPrefix}("))
          s"${caseClazz.productPrefix}(" + caseClazz.productIterator.map(prettify(_, processed + caseClazz)).mkString(", ") + ")"
        else if (caseClazz.productPrefix.startsWith("Tuple"))
          "(" + caseClazz.productIterator.map(prettify(_, processed + caseClazz)).mkString(", ") + ")"
        else
          caseClazz.toString
      case anythingElse => anythingElse.toString
    }

  protected def prettify(o: Any, processed: Set[Any]): String = 
    if (processed.contains(o))
      throw new StackOverflowError("Cyclic relationship detected, let's fail early!")
    else  
      o match {
        case null => "null"
        case aUnit: Unit => "<(), the Unit value>"
        case aString: String => "\"" + aString + "\""
        case aStringWrapper: org.scalactic.ColCompatHelper.StringOps => "\"" + aStringWrapper.mkString + "\""
        case aChar: Char =>  "\'" + aChar + "\'"
        case Some(e) => "Some(" + prettify(e, processed) + ")"
        case Success(e) => "Success(" + prettify(e, processed) + ")"
        case Left(e) => "Left(" + prettify(e, processed) + ")"
        case Right(e) => "Right(" + prettify(e, processed) + ")"
        case s: Symbol => "'" + s.name
        case Good(e) => "Good(" + prettify(e, processed) + ")"
        case Bad(e) => "Bad(" + prettify(e, processed) + ")"
        case One(e) => "One(" + prettify(e, processed) + ")"
        case other => prettifyCollection(other, processed)
      } 

  def apply(o: Any): String = {
    try {
      prettify(o, Set.empty)
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

private[scalactic] class TruncatingPrettifier(private[scalactic] val sizeLimit: SizeLimit) extends DefaultPrettifier {

  private def dotDotDotIfTruncated(value: Boolean): String =
    if (value) ", ..." else ""

  override protected def prettifyCollection(o: Any, processed: Set[Any]): String = {
    o match {
      case many: Many[_] => 
        val (taken, truncated) = if (!many.hasDefiniteSize || many.size > sizeLimit.value) (many.toIterator.take(sizeLimit.value), true) else (many.toIterator, false)  
        "Many(" + taken.map(prettify(_, processed + many)).mkString(", ") + dotDotDotIfTruncated(truncated) + ")"
      case anArray: Array[_] =>  
        val (taken, truncated) = if (!anArray.hasDefiniteSize || anArray.size > sizeLimit.value) (anArray.take(sizeLimit.value), true) else (anArray, false)
        "Array(" + taken.map(prettify(_, processed + anArray)).mkString(", ") + dotDotDotIfTruncated(truncated) + ")"
      case aWrappedArray: WrappedArray[_] => 
        val (taken, truncated) = if (!aWrappedArray.hasDefiniteSize || aWrappedArray.size > sizeLimit.value) (aWrappedArray.take(sizeLimit.value), true) else (aWrappedArray, false)
        "Array(" + taken.map(prettify(_, processed + aWrappedArray)).mkString(", ") + dotDotDotIfTruncated(truncated) + ")"
      case a if ArrayHelper.isArrayOps(a) => 
        val anArrayOps = ArrayHelper.asArrayOps(a)//
        val (taken, truncated) = if (anArrayOps.size > sizeLimit.value) (anArrayOps.iterator.take(sizeLimit.value), true) else (anArrayOps.iterator, false)
        "Array(" + taken.map(prettify(_, processed + anArrayOps)).mkString(", ") + dotDotDotIfTruncated(truncated) + ")"
      case aGenMap: scala.collection.GenMap[_, _] =>
        val (taken, truncated) = if (!aGenMap.hasDefiniteSize || aGenMap.size > sizeLimit.value) (aGenMap.toIterator.take(sizeLimit.value), true) else (aGenMap.toIterator, false)
        ColCompatHelper.className(aGenMap) + "(" +
        (taken.map { case (key, value) => // toIterator is needed for consistent ordering
          prettify(key, processed + aGenMap) + " -> " + prettify(value, processed + aGenMap)
        }).mkString(", ") + dotDotDotIfTruncated(truncated) + ")"
      case aIterable: Iterable[_] =>
        val (taken, truncated) = if (!aIterable.hasDefiniteSize || aIterable.size > sizeLimit.value) (aIterable.take(sizeLimit.value), true) else (aIterable, false)
        val className = aIterable.getClass.getName
        if (className.startsWith("scala.xml.NodeSeq$") || className == "scala.xml.NodeBuffer" || className == "scala.xml.Elem")
          aIterable.mkString
        else
          ColCompatHelper.className(aIterable) + "(" + taken.toIterator.map(prettify(_, processed + aIterable)).mkString(", ") + dotDotDotIfTruncated(truncated) + ")" // toIterator is needed for consistent ordering                
      case javaCol: java.util.Collection[_] =>
        // By default java collection follows http://download.java.net/jdk7/archive/b123/docs/api/java/util/AbstractCollection.html#toString()
        // let's do our best to prettify its element when it is not overriden
        import scala.collection.JavaConverters._
        val theToString = javaCol.toString
        if (theToString.startsWith("[") && theToString.endsWith("]")) {
          val itr = javaCol.iterator().asScala
          val (taken, truncated) = if (javaCol.size > sizeLimit.value) (itr.take(sizeLimit.value), true) else (itr, false)
          "[" + taken.map(prettify(_, processed + javaCol)).mkString(", ") + dotDotDotIfTruncated(truncated) + "]"
        }
        else
          theToString
      case javaMap: java.util.Map[_, _] =>
        // By default java map follows http://download.java.net/jdk7/archive/b123/docs/api/java/util/AbstractMap.html#toString()
        // let's do our best to prettify its element when it is not overriden
        import scala.collection.JavaConverters._
        val theToString = javaMap.toString
        if (theToString.startsWith("{") && theToString.endsWith("}")) {
          val itr = javaMap.entrySet.iterator.asScala
          val (taken, truncated) = if (javaMap.size > sizeLimit.value) (itr.take(sizeLimit.value), true) else (itr, false)
          "{" + taken.map { entry =>
            prettify(entry.getKey, processed + javaMap) + "=" + prettify(entry.getValue, processed + javaMap)
          }.mkString(", ") + dotDotDotIfTruncated(truncated) + "}"
        }
        else
          theToString
      case caseClazz: Product if caseClazz.productArity != 0 =>
        // Unlike in DefaultPrettifier where we check if a custom `.toString` has been overridden, with
        // TruncatingPrettifier the priority is truncating the enclosed data at all costs hence why we always
        // truncate the inner fields.
        if (caseClazz.productPrefix.startsWith("Tuple"))
          s"(" + caseClazz.productIterator.map(prettify(_, processed + caseClazz)).mkString(", ") + ")"
        else
          s"${caseClazz.productPrefix}(" + caseClazz.productIterator.map(prettify(_, processed + caseClazz)).mkString(", ") + ")"
      case anythingElse => anythingElse.toString
    }
  }
}

/**
 * Companion object for `Prettifier` that provides a default `Prettifier` implementation.
 */
object Prettifier {

  /**
   * Constract a new `Prettifier` from a given partial function.
   *
   * @param fun a partial function with which to implement the apply method of the returned `Prettifier`.
   */
  def apply(fun: PartialFunction[Any, String]): Prettifier =
    new Prettifier {
      def apply(o: Any): String = fun(o)
    }

  /**
   * Construct a new `Prettifier` with given <code>prettifier</code> and <code>customDiffer</code>.
   * @param prettifier the base prettifer that the new prettifier will delegate apply(o: Any) to.
   * @param fun a partial function with which to implement the apply method of the returned `Prettifier`.
   */
  def apply(prettifier: Prettifier, customDiffer: Differ): Prettifier = 
    new Prettifier {
      def apply(o: Any): String = prettifier.apply(o)
      override def apply(left: Any, right: Any): PrettyPair = {
        customDiffer.difference(left, right, this)
      }
    }  

  /**
   * A default `Prettifier`. 
   *
   * <p>
   * This default `Prettifier` is used in ScalaTest to clarify error messages.
   * </p>
   *
   * <p>
   * It transforms:
   * </p>
   *
   * <ul>
   * <li>`Null` to: `null`</li>
   * <li>`Unit` to: `&lt;() the Unit value&gt;`</li>
   * <li>`String` to: `"string"` (the `toString` result surrounded by double quotes)</li>
   * <li>`Char` to: `'c'` (the `toString` result surrounded by single quotes)</li>
   * <li>`Array` to: `Array("1", "2", "3")`</li>
   * <li>`scala.Some` to: `Some("3")`</li>
   * <li>`scala.util.Left` to: `Left("3")`</li>
   * <li>`scala.util.Right` to: `Right("3")`</li>
   * <li>`scala.util.Success` to: `Success("3")`</li>
   * <li>`org.scalactic.Good` to: `Good("3")`</li>
   * <li>`org.scalactic.Bad` to: `Bad("3")`</li>
   * <li>`org.scalactic.One` to: `One("3")`</li>
   * <li>`org.scalactic.Many` to: `Many("1", "2", "3")`</li>
   * <li>`scala.collection.Iterable` to: `List("1", "2", "3")`</li>
   * <li>`java.util.Collection` to: `["1", "2", "3"]`</li>
   * <li>`java.util.Map` to: `{1="one", 2="two", 3="three"}`</li>
   * </ul>
   *
   *
   * <p>
   * For anything else, it returns the result of invoking `toString`.
   * </p>
   */
  implicit val default: Prettifier = new DefaultPrettifier()

  def withEscapingDiffer(p: Prettifier): Prettifier = 
    new Prettifier {
      def apply(o: Any): String = p.apply(o)
      override def apply(left: Any, right: Any): PrettyPair = {
        val prettyPair = p.apply(left, right)
        val escapingStringDiffer = new EscapingStringDiffer()
        val escapingPrettyPair = escapingStringDiffer.difference(left, right, p)
        prettyPair.copy(analysis = escapingPrettyPair.analysis)
      }
    }

  /**
   * Create a default prettifier instance with collection size limit.
   */
  def truncateAt(limit: SizeLimit): Prettifier = new TruncatingPrettifier(limit)

  /**
   * A basic `Prettifier`.
   *
   * <p>
   * This was the default `Prettifier` used in ScalaTest 2.0 release.
   * </p>
   *
   * <p>
   * It transforms:
   * </p>
   *
   * <ul>
   * <li>`Null` to: `null`</li>
   * <li>`Unit` to: `&lt;() the Unit value&gt;`</li>
   * <li>`String` to: `"string"` (the `toString` result surrounded by double quotes)</li>
   * <li>`Char` to: `'c'` (the `toString` result surrounded by single quotes)</li>
   * <li>`Array` to: `Array("1", "2", "3")`</li>
   * <li>`scala.util.Some` to: `Some("3")`</li>
   * </ul>
   *
   * <p>
   * For anything else, it returns the result of invoking `toString`.
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
	
  private[org] val lineSeparator: String = System.lineSeparator
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
