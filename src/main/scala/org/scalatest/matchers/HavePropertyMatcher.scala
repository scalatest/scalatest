/* * Copyright 2001-2008 Artima, Inc.
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

// T is the type of the object that has a property to verify with an instance of this trait, P is the type of that particular property
// Since I should be able to pass 
/**
 * Trait extended by matcher objects, which may appear after the word <code>have</code>, that can match against a 
 * property of the type specified by the <code>HavePropertyMatcher</code>'s second type parameter <code>P</code>.
 * <code>HavePropertyMatcher</code>'s first type parameter, <code>T</code>, specifies the type that declares the property. The match will succeed if and
 * only if the value of the property equals the specified value.
 * The object containing the property
 * is passed to the <code>HavePropertyMatcher</code>'s
 * <code>apply</code> method. The result is a <code>HavePropertyMatchResult[P]</code>.
 * A <code>HavePropertyMatcher</code> is, therefore, a function from the specified type, <code>T</code>, to
 * a <code>HavePropertyMatchResult[P]</code>.
 *
 * <p>
 * Although <code>HavePropertyMatcher</code>
 * and <code>Matcher</code> represent similar concepts, they have no inheritance relationship
 * because <code>Matcher</code> is intended for use right after <code>should</code> or <code>must</code>
 * whereas <code>HavePropertyMatcher</code> is intended for use right after <code>have</code>.
 * </p>
 *
 * <p>
 * A <code>HavePropertyMatcher</code> essentially allows you to write statically typed
 * property assertions similar to the dynamic ones that use symbols:
 * </p>
 *
 * <pre class="stHighlight">
 * book should have ('title ("Moby Dick")) // dynamic: uses reflection
 * book should have (title ("Moby Dick"))  // type safe: only works on Books; no reflection used
 * </pre>
 *
 * <p>
 * One good way to organize custom matchers is to place them inside one or more traits that
 * you can then mix into the suites or specs that need them. Here's an example that
 * includes two methods that produce <code>HavePropertyMatcher</code>s:
 * </p>
 *
 * <pre class="stHighlight">
 * case class Book(val title: String, val author: String)
 *
 * trait CustomMatchers {
 * 
 *   def title(expectedValue: String) =
 *     new HavePropertyMatcher[Book, String] {
 *       def apply(book: Book) =
 *         HavePropertyMatchResult(
 *           book.title == expectedValue,
 *           "title",
 *           expectedValue,
 *           book.title
 *         )
 *     }
 *
 *   def author(expectedValue: String) = 
 *     new HavePropertyMatcher[Book, String] {
 *       def apply(book: Book) =
 *         HavePropertyMatchResult(
 *           book.author == expectedValue,
 *           "author",
 *           expectedValue,
 *           book.author
 *         )
 *     }
 * }
 * </pre>
 * 
 * <p>
 * Each time the <code>title</code> method is called, it returns a new <code>HavePropertyMatcher[Book, String]</code> that
 * can be used to match against the <code>title</code> property of the <code>Book</code> passed to its <code>apply</code>
 * method. Because the type parameter of these two <code>HavePropertyMatcher</code>s is <code>Book</code>, they 
 * can only be used with instances of that type. (The compiler will enforce this.) The match will succeed if the
 * <code>title</code> property equals the value passed as <code>expectedValue</code>.
 * If the match succeeds, the <code>matches</code> field of the returned <code>HavePropertyMatchResult</code> will be <code>true</code>.
 * The second field, <code>propertyName</code>, is simply the string name of the property.
 * The third and fourth fields, <code>expectedValue</code> and <code>actualValue</code> indicate the expected and actual
 * values, respectively, for the property.
 * Here's an example that uses these <code>HavePropertyMatchers</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * class ExampleSpec extends Spec with ShouldMatchers with CustomMatchers {
 * 
 *   describe("A book") {
 * 
 *     it("should have the correct title and author") {
 * 
 *       val book = Book("Moby Dick", "Melville")
 * 
 *       book should have (
 *         title ("Moby Dick"),
 *         author ("Melville")
 *       )
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * These matches should succeed, but if for example the first property, <code>title ("Moby Dick")</code>, were to fail, you would get an error message like:
 * </p>
 *
 * <pre class="stExamples">
 * The title property had value "A Tale of Two Cities", instead of its expected value "Moby Dick",
 * on object Book(A Tale of Two Cities,Dickens)
 * </pre>
 *
 * <p>
 * For more information on <code>HavePropertyMatchResult</code> and the meaning of its fields, please
 * see the documentation for <a href="HavePropertyMatchResult.html"><code>HavePropertyMatchResult</code></a>. To understand why <code>HavePropertyMatcher</code>
 * is contravariant in its type parameter, see the section entitled "Matcher's variance" in the
 * documentation for <a href="../Matcher.html"><code>Matcher</code></a>.
 * </p>
 *
 * @author Bill Venners
*/
trait HavePropertyMatcher[-T, P] extends Function1[T, HavePropertyMatchResult[P]] {

  thisHavePropertyMatcher =>

  /**
   * Check to see if a property on the specified object, <code>objectWithProperty</code>, matches its
   * expected value, and report the result in
   * the returned <code>HavePropertyMatchResult</code>. The <code>objectWithProperty</code> is
   * usually the value to the left of a <code>should</code> or <code>must</code> invocation. For example, <code>book</code>
   * would be passed as the <code>objectWithProperty</code> in:
   *
   * <pre class="stHighlight">
   * book should have (title ("Moby Dick"))
   * </pre>
   *
   * @param objectWithProperty the object with the property against which to match
   * @return the <code>HavePropertyMatchResult</code> that represents the result of the match
   */
  def apply(objectWithProperty: T): HavePropertyMatchResult[P]

  /**
   * Compose this <code>HavePropertyMatcher</code> with the passed function, returning a new <code>HavePropertyMatcher</code>.
   *
   * <p>
   * This method overrides <code>compose</code> on <code>Function1</code> to
   * return a more specific function type of <code>HavePropertyMatcher</code>.
   * </p>
   */
  override def compose[U](g: U => T): HavePropertyMatcher[U, P] =
    new HavePropertyMatcher[U, P] {
      def apply(u: U) = thisHavePropertyMatcher.apply(g(u))
    }
}

/**
 * Companion object for trait <code>HavePropertyMatcher</code> that provides a
 * factory method that creates a <code>HavePropertyMatcher[T]</code> from a
 * passed function of type <code>(T => HavePropertyMatchResult)</code>.
 *
 * @author Bill Venners
 */
object HavePropertyMatcher {

  /**
   * Factory method that creates a <code>HavePropertyMatcher[T]</code> from a
   * passed function of type <code>(T => HavePropertyMatchResult)</code>.
   *
   * <p>
   * This allows you to create a <code>HavePropertyMatcher</code> in a slightly
   * more concise way, for example:
   * </p>
   *
   * <pre class="stHighlight">
   *  case class Person(name: String)
   *  def name(expectedName: String) = {
   *    HavePropertyMatcher { 
   *      (person: Person) => HavePropertyMatchResult(
   *        person.name == expectedName,
   *        "name",
   *        expectedName,
   *        person.name
   *      ) 
   *    } 
   * </pre>
   *
   * @author Bill Venners
   */
  def apply[T, P](fun: T => HavePropertyMatchResult[P]): HavePropertyMatcher[T, P] =
    new HavePropertyMatcher[T, P] {
      def apply(left: T) = fun(left)
    }
}
