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
package org.scalatest.matchers

import scala.reflect.ClassTag

// T is the type of the object that has a Boolean property to verify with an instance of this trait
// This is not a subtype of BeMatcher, because BeMatcher only works after "be", but 
// BePropertyMatcher will work after "be", "be a", or "be an"
/**
 * Trait extended by matcher objects, which may appear after the word <code>be</code>, that can match against a <code>Boolean</code>
 * property. The match will succeed if and only if the <code>Boolean</code> property equals <code>true</code>.
 * The object containing the property, which must be of the type specified by the <code>BePropertyMatcher</code>'s type
 * parameter <code>T</code>, is passed to the <code>BePropertyMatcher</code>'s
 * <code>apply</code> method. The result is a <code>BePropertyMatchResult</code>.
 * A <code>BePropertyMatcher</code> is, therefore, a function from the specified type, <code>T</code>, to
 * a <code>BePropertyMatchResult</code>.
 *
 * <p>
 * Although <code>BePropertyMatcher</code>
 * and <code>Matcher</code> represent similar concepts, they have no inheritance relationship
 * because <code>Matcher</code> is intended for use right after <code>should</code> or <code>must</code>
 * whereas <code>BePropertyMatcher</code> is intended for use right after <code>be</code>.
 * </p>
 *
 * <p>
 * A <code>BePropertyMatcher</code> essentially allows you to write statically typed <code>Boolean</code>
 * property assertions similar to the dynamic ones that use symbols:
 * </p>
 *
 * <pre class="stHighlight">
 * tempFile should be a ('file) // dynamic: uses reflection
 * tempFile should be a (file)  // type safe: only works on Files; no reflection used
 * </pre>
 *
 * <p>
 * One good way to organize custom matchers is to place them inside one or more traits that
 * you can then mix into the suites or specs that need them. Here's an example that
 * includes two <code>BePropertyMatcher</code>s:
 * </p>
 *
 * <pre class="stHighlight">
 * trait CustomMatchers {
 * 
 *   class FileBePropertyMatcher extends BePropertyMatcher[java.io.File] {
 *     def apply(left: java.io.File) = BePropertyMatchResult(left.isFile, "file")
 *   }
 * 
 *   class DirectoryBePropertyMatcher extends BePropertyMatcher[java.io.File] {
 *     def apply(left: java.io.File) = BePropertyMatchResult(left.isDirectory, "directory")
 *   }
 * 
 *   val file = new FileBePropertyMatcher
 *   val directory = new DirectoryBePropertyMatcher
 * }
 * </pre>
 * 
 * <p>
 * Because the type parameter of these two <code>BePropertyMatcher</code>s is <code>java.io.File</code>, they 
 * can only be used with instances of that type. (The compiler will enforce this.) All they do is create a
 * <code>BePropertyMatchResult</code> whose <code>matches</code> field is <code>true</code> if and only if the <code>Boolean</code> property
 * is <code>true</code>. The second field, <code>propertyName</code>, is simply the string name of the property.
 * The <code>file</code> and <code>directory</code> <code>val</code>s create variables that can be used in
 * matcher expressions that test whether a <code>java.io.File</code> is a file or a directory. Here's an example:
 * </p>
 *
 * <pre class="stHighlight">
 * class ExampleSpec extends RefSpec with Matchers with CustomMatchers {
 * 
 *   describe("A temp file") {
 * 
 *     it("should be a file, not a directory") {
 * 
 *       val tempFile = java.io.File.createTempFile("delete", "me")
 * 
 *       try {
 *         tempFile should be a (file)
 *         tempFile should not be a (directory)
 *       }
 *       finally {
 *         tempFile.delete()
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * These matches should succeed, but if for example the first match, <code>tempFile should be a (file)</code>, were to fail, you would get an error message like:
 * </p>
 *
 * <pre class="stExamples">
 * /tmp/delme1234me was not a file
 * </pre>
 *
 * <p>
 * For more information on <code>BePropertyMatchResult</code> and the meaning of its fields, please
 * see the documentation for <a href="BePropertyMatchResult.html"><code>BePropertyMatchResult</code></a>. To understand why <code>BePropertyMatcher</code>
 * is contravariant in its type parameter, see the section entitled "Matcher's variance" in the
 * documentation for <a href="../Matcher.html"><code>Matcher</code></a>.
 * </p>
 *
 * @author Bill Venners
*/
trait BePropertyMatcher[-T] extends Function1[T, BePropertyMatchResult] {

  thisBePropertyMatcher => 

  /**
   * Check to see if a <code>Boolean</code> property on the specified object, <code>objectWithProperty</code>, matches its
   * expected value, and report the result in
   * the returned <code>BePropertyMatchResult</code>. The <code>objectWithProperty</code> is
   * usually the value to the left of a <code>should</code> or <code>must</code> invocation. For example, <code>tempFile</code>
   * would be passed as the <code>objectWithProperty</code> in:
   *
   * <pre class="stHighlight">
   * tempFile should be a (file)
   * </pre>
   *
   * @param objectWithProperty the object with the <code>Boolean</code> property against which to match
   * @return the <code>BePropertyMatchResult</code> that represents the result of the match
   */
  def apply(objectWithProperty: T): BePropertyMatchResult

  /**
   * Compose this <code>BePropertyMatcher</code> with the passed function, returning a new <code>BePropertyMatcher</code>.
   *
   * <p>
   * This method overrides <code>compose</code> on <code>Function1</code> to
   * return a more specific function type of <code>BePropertyMatcher</code>.
   * </p>
   */
  override def compose[U](g: U => T): BePropertyMatcher[U] =
    new BePropertyMatcher[U] {
      def apply(u: U) = thisBePropertyMatcher.apply(g(u))
    }
}

/**
 * Companion object for trait <code>BePropertyMatcher</code> that provides a
 * factory method that creates a <code>BePropertyMatcher[T]</code> from a
 * passed function of type <code>(T => BePropertyMatchResult)</code>.
 *
 * @author Bill Venners
 */
object BePropertyMatcher {

  /**
   * Factory method that creates a <code>BePropertyMatcher[T]</code> from a
   * passed function of type <code>(T => BePropertyMatchResult)</code>.
   *
   * @author Bill Venners
   */
  def apply[T](fun: T => BePropertyMatchResult)(implicit ev: ClassTag[T]): BePropertyMatcher[T] =
    new BePropertyMatcher[T] {
      def apply(left: T) = fun(left)
      override def toString: String = "BePropertyMatcher[" + ev.runtimeClass.getName + "](" + ev.runtimeClass.getName + " => BePropertyMatchResult)"
    }
}
