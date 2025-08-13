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
package org.scalatest

import org.scalactic.source

/**
 * Trait that contains methods named <code>given</code>, <code>when</code>, <code>then</code>, and <code>and</code>,
 * which take a string message and implicit <a href="Informer.html"><code>Informer</code></a>, and forward the message to the informer.
 *
 * <p>
 * Here's an example:
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
 *     Given("an empty mutable Set")
 *     val set = mutable.Set.empty[String]
 * 
 *     When("an element is added")
 *     set += "clarity"
 * 
 *     Then("the Set should have size 1")
 *     assert(set.size === 1)
 * 
 *     And("the Set should contain the added element")
 *     assert(set.contains("clarity"))
 * 
 *     info("That's all folks!")
 *   }
 * }
 * </pre>
 *
 * <p>
 * If you run this <code>SetSpec</code> from the interpreter, you will see the following output:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; org.scalatest.run(new SetSpec)
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
trait GivenWhenThen { this: Informing =>

  /**
   * Forwards a message to an implicit <code>Informer</code>, preceded by "Given."
   *
   * @param message the message to forward to the passed informer
   */
  def Given(message: String)(implicit pos: source.Position): Unit = {
    info(Resources.givenMessage(message))(pos)
  }
  
  /**
   * Forwards a message to an implicit <code>Informer</code>, preceded by "When ".
   *
   * @param message the message to forward to the passed informer
   */
  def When(message: String)(implicit pos: source.Position): Unit = {
    info(Resources.whenMessage(message))(pos)
  }
  
  /**
   * Forwards a message to an implicit <code>Informer</code>, preceded by "Then ".
   *
   * @param message the message to forward to the passed informer
   */
  def Then(message: String)(implicit pos: source.Position): Unit = {
    info(Resources.thenMessage(message))(pos)
  }
  
   /**
   * Forwards a message to an implicit <code>Informer</code>, preceded by "And ".
   *
   * @param message the message to forward to the passed informer
   */
  def And(message: String)(implicit pos: source.Position): Unit = {
    info(Resources.andMessage(message))(pos)
  }
}
