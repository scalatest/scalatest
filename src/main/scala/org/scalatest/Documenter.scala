/*
 * Copyright 2001-2008 Artima, Inc.
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

 /*
 * <p>
 * A <code>Documenter</code> is essentially
 * used to wrap a <code>Reporter</code> and provide easy ways to send markup text
 * to that <code>Reporter</code> via a <code>MarkupProvided</code> event.
 * <code>Documenter</code> contains an <code>apply</code> method that takes a string.
 * The <code>Documenter</code> will forward the passed string to the <code>Reporter</code> as the <code>text</code>
 * parameter of an <code>MarkupProvided</code> event.
 * </p>
 *
 * <p>
 * Here's an example of using an <code>Documenter</code> in a <code>Suite</code>
 * subclass:
 * </p>
 * 
 * <pre class="stHighlight">
 * import org.scalatest._
 * 
 * class MySuite extends Suite {
 *   def testAddition(markup: Documenter) {
 *     assert(1 + 1 === 2)
 *     markup("Addition *seems* to work")
 *   }
 * }
 * </pre>
 *
 * <p>
 * As of 2.0, the only built-in reporter that presents markup text is the HTML reporter. 
 * If you run this <code>Suite</code> and specify the HTML reporter, you will see the message
 * included in the HTML report:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; (new MySuite).execute()
 * <span class="stGreen">- testAddition(Informer)
 *   + Addition <em>seems</em> to work</span>
 * </pre>
 *
 * <p>
 * Traits <code>FunSuite</code>, <code>Spec</code>, <code>FlatSpec</code>, <code>WordSpec</code>, <code>FeatureSpec</code>, and 
 * their sister traits in <code>org.scalatest.fixture</code> package declare an implicit <code>markup</code> method that returns
 * an <code>Documenter</code>.
 * Here's an example of a <code>Spec</code> that uses <code>markup</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * import org.scalatest.Spec
 * import scala.collection.mutable.Stack
 *
 * class StackSpec extends Spec {
 *
 *   markup("""
 *
 *     Stack Specification
 *     ===================
 *
 *     A  &#96;Stack&#96; is a data structure that allows you to store and retrieve objects in
 *     a last-in-first-out (LIFO) fashion. &#96;Stack&#96;s (both this class and its immutable
 *     cousin) are not commonly used in Scala, because a &#96;List&#96; gives you
 *     the same basic functionality. Pushing an object onto a &#96;Stack&#96; maps to consing
 *     a new element onto the front of a &#96;List&#96;. Peaking at the top of the &#96;Stack&#96; maps to
 *     to a &#96;head&#96;. Popping an object off of a &#96;Stack&#96; maps to a &#96;head&#96; followed by a &#96;tail&#96;. 
 *     Nevertheless, using a &#96;Stack&#96; instead of a &#96;List&#96; can clarify your intent
 *     to readers of your code.
 *
 *   """)
 *
 *   describe("A Stack") {
 *
 *     it("should pop values in last-in-first-out order") {
 *       val stack = new Stack[Int]
 *       stack.push(1)
 *       stack.push(2)
 *       assert(stack.pop() === 2)
 *       assert(stack.pop() === 1)
 *     }
 *
 *     it("should throw NoSuchElementException if an empty stack is popped") {
 *       val emptyStack = new Stack[String]
 *       intercept[NoSuchElementException] {
 *         emptyStack.pop()
 *       }
 *     }
 *   }
 * }
 * </pre>
 *
 * <p>
 * Were you to run this <code>FeatureSpec</code> in the interpreter, you would see the following output:
 * </p>
 *
 * <pre class="stREPL">
 * scala> (new ArithmeticFeatureSpec).run()
 * <span class="stGreen">Feature: Integer arithmetic 
 *   Scenario: addition
 *     Given two integers 
 *     When they are added 
 *     Then the result is the sum of the two numbers 
 *   Scenario: subtraction
 *     Given two integers 
 *     When one is subtracted from the other 
 *     Then the result is the difference of the two numbers</span> 
 * </pre>
 * 
 * @author Bill Venners
 */
/**
 * Trait to which markup text tests can be reported.
 * 
 * <p>
 * Note: <code>Documenter</code> will be described in more detail in a future 2.0 milestone release. As of this release
 * you can't see its effects yet.
 * </p>
 * 
 * @author Bill Venners
 */
trait Documenter extends (String => Unit) {

  /**
   * Provide documentation to the <code>Reporter</code>.
   *
   * @param text an string of markup text that will be forwarded to the wrapped <code>Reporter</code>
   *   via a <code>MarkupProvided</code> event.
   *
   * @throws NullPointerException if <code>message</code> reference is <code>null</code>
   */
  def apply(text: String): Unit
}
