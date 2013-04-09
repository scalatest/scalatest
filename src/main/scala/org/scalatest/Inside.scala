/*
 * Copyright 2001-2011 Artima, Inc.
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

import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun

/**
 * Trait containing the <code>inside</code> construct, which allows you to make statements about nested object graphs using pattern matching.
 *
 * <p>
 * For example, given the following case classes:
 *
 * <pre class="stHighlight">
 * case class Address(street: String, city: String, state: String, zip: String)
 * case class Name(first: String, middle: String, last: String)
 * case class Record(name: Name, address: Address, age: Int)
 * </pre>
 *
 * You could write:
 *
 * <pre class="stHighlight">
 * inside (rec) { case Record(name, address, age) =&gt;
 *   inside (name) { case Name(first, middle, last) =&gt;
 *     first should be ("Sally")
 *     middle should be ("Ann")
 *     last should be ("Jones")
 *   }
 *   inside (address) { case Address(street, city, state, zip) =&gt;
 *     street should startWith ("25")
 *     city should endWith ("Angeles")
 *     state should equal ("CA")
 *     zip should be ("12345")
 *   }
 *   age should be &lt; 99
 * }
 * </pre>
 *
 * <p>
 * If an assertion fails, the error message will include the <code>toString</code> of each value passed
 * to <code>inside</code> clauses enclosing the failed assertion. For example, if <code>rec</code> in 
 * the previous expression was defined like this:
 * </p>
 * 
 * <pre class="stHighlight">
 * val rec = Record(
 *   Name("Sally", "Anna", "Jones"),
 *   Address("25 Main St", "Los Angeles", "CA", "12345"),
 *   38
 * )
 * </pre>
 *
 * <p>
 * The error message will read:
 * </p>
 *
 * <pre>
 * "Ann[a]" was not equal to "Ann[]", inside Name(Sally,Anna,Jones),
 * inside Record(Name(Sally,Anna,Jones),Address(25 Main St,Los Angeles,CA,12345),38)
 * </pre>
 *
 */
trait Inside {

  /**
   * Inspects inside the passed value using the passed partial function.
   *
   * <p>
   * The <code>inside</code> method checks to see whether the partial function passed as the second curried
   * parameter is defined at the value passed as the first parameter, and if so, passes that value to the
   * partial function.
   * </p>
   *
   * <p>
   * If the partial function is not defined at the passed value, <code>inside</code> will throw a 
   * <code>TestFailedException</code> with a detail message describing the problem. Otherwise, if the
   * partial function returns normally, <code>inside</code> will return normally. If the partial function
   * completes abruptly with an exception that mixes in <code>ModifiableMessage</code> (such as
   * <code>TestFailedException</code>), <code>inside</code> will append the value's <code>toString</code> of
   * to the exception's detail message, and rethrow it. If the exception thrown by the partial function does not mix
   * in <code>ModifiableMessage</code>, <code>inside</code> completes abruptly with that same exception.
   * </p>
   *
   * @param value the value inside of which to inspect
   * @param pf the partial function to use to inspect inside the passed value
   * @throws TestFailedException if the passed partial function is not defined at the passed value
   */
  def inside[T](value: T)(pf: PartialFunction[T, Unit]) {
    def appendInsideMessage(currentMessage: Option[String]) =
      currentMessage match {
        case Some(msg) => Some(Resources("insidePartialFunctionAppendSomeMsg", msg.trim, value.toString()))
        case None => Some(Resources("insidePartialFunctionAppendNone", value.toString()))
      }
    if (pf.isDefinedAt(value)) {
      try {
        pf(value)
      }
      catch {
        case e: org.scalatest.exceptions.ModifiableMessage[_] =>
          throw e.modifyMessage(appendInsideMessage)
      }
    }
    else
      throw new TestFailedException(sde => Some(Resources("insidePartialFunctionNotDefined", value.toString())), None, getStackDepthFun("Inside.scala", "inside"))
      //throw new TestFailedException(Resources("insidePartialFunctionNotDefined", value.toString()), 2)
  }
}

/**
 * Companion object that facilitates the importing of the <code>inside</code> construct as 
 * an alternative to mixing it in. One use case is to import the <code>inside</code> construct so you can use
 * it in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $ scala -cp scalatest-1.8.jar
 * Welcome to Scala version 2.9.1.final (Java HotSpot(TM) 64-Bit Server VM, Java 1.6.0_29).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 * 
 * scala&gt; import org.scalatest._
 * import org.scalatest._
 * 
 * scala&gt; import matchers.ShouldMatchers._
 * import matchers.ShouldMatchers._
 * 
 * scala&gt; import Inside._
 * import Inside._
 * 
 * scala&gt; inside (List(1, 2, 3)) { case List(x, y, z) =>
 *     |   y should equal (2)
 *     | }
 *
 * scala&gt; inside (List(1, 2, 3)) { case List(x, y, z) =>
 *      |   x should equal (2)
 *      | }
 * org.scalatest.TestFailedException: 1 did not equal 2, inside List(1, 2, 3)
 *   at org.scalatest.matchers.Matchers$class.newTestFailedException(Matchers.scala:150)
 *   at org.scalatest.matchers.ShouldMatchers$.newTestFailedException(ShouldMatchers.scala:2331)
 *   at org.scalatest.matchers.ShouldMatchers$ShouldMethodHelper$.shouldMatcher(ShouldMatchers.scala:873)
 *   ...
 * </pre>
 */
object Inside extends Inside
