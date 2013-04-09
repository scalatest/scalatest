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

/**
 * Trait to which custom information about a running suite of tests can be reported.
 * 
 * <p>
 * An <code>Informer</code> is essentially
 * used to wrap a <code>Reporter</code> and provide easy ways to send custom information
 * to that <code>Reporter</code> via an <code>InfoProvided</code> event.
 * <code>Informer</code> contains an <code>apply</code> method that takes a string and
 * an optional payload object of type <code>Any</code>.
 * The <code>Informer</code> will forward the passed <code>message</code> string to the
 * <code>Reporter</code> as the <code>message</code> parameter, and the optional
 * payload object as the <code>payload</code> parameter, of an <code>InfoProvided</code> event.
 * </p>
 *
 * <p>
 * Here's an example in which the <code>Informer</code> is used both directly via <code>info</code>
 * method of trait <a href="FlatSpec.html"><code>FlatSpec</code></a> and indirectly via the methods of
 * trait <a href="GivenWhenThen.html"><code>GivenWhenThen</code></a>:
 * </p>
 * 
 * <pre class="stHighlight">
 * package org.scalatest.examples.flatspec.info
 * 
 * import collection.mutable
 * import org.scalatest._
 * 
 * class SetSpec extends FlatSpec with GivenWhenThen {
 *   
 *   "A mutable Set" should "allow an element to be added" in {
 *     given("an empty mutable Set")
 *     val set = mutable.Set.empty[String]
 * 
 *     when("an element is added")
 *     set += "clarity"
 * 
 *     then("the Set should have size 1")
 *     assert(set.size === 1)
 * 
 *     and("the Set should contain the added element")
 *     assert(set.contains("clarity"))
 * 
 *     info("That's all folks!")
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this <code>FlatSpec</code> from the interpreter, you will see the following output:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; new SetSpec execute
 * <span class="stGreen">A mutable Set
 * - should allow an element to be added
 *   + Given an empty mutable Set 
 *   + When an element is added 
 *   + Then the Set should have size 1 
 *   + And the Set should contain the added element 
 *   + That's all folks! </span>
 * </pre>
 *
 * @author Bill Venners
 */
trait Informer {
       // TODO: Make sure all the informer implementations check for null
  /**
   * Provide information and optionally, a payload, to the <code>Reporter</code> via an
   * <code>InfoProvided</code> event.
   *
   * @param message a string that will be forwarded to the wrapped <code>Reporter</code>
   *   via an <code>InfoProvided</code> event.
   * @param payload an optional object which will be forwarded to the wrapped <code>Reporter</code>
   *   as a payload via an <code>InfoProvided</code> event.
   *
   * @throws NullPointerException if <code>message</code> or <code>payload</code> reference is <code>null</code>
   */
  def apply(message: String, payload: Option[Any] = None): Unit
  
  /**
   * Provide information and additional payload to the <code>Reporter</code> as the .
   *
   * @param message an object whose <code>toString</code> result will be forwarded to the wrapped <code>Reporter</code>
   *   via an <code>InfoProvided</code> event.
   * @param payload an object which will be forwarded to the wrapped <code>Reporter</code> 
   *   via an <code>InfoProvided</code> event.
   *
   * @throws NullPointerException if <code>message</code> reference is <code>null</code>
   */
  //def apply(message: String, payload: Any): Unit
}
