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
 * Trait that contains methods named <code>given</code>, <code>when</code>, <code>then</code>, and <code>and</code>,
 * which take a string message and implicit <code>Informer</code>, and forward the message to the informer.
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
 * <p>
 * <strong>Note: The lower case forms of <code>given</code>, <code>when</code>, <code>then</code> and <code>and</code> were
 * deprecated in ScalaTest because using <code>then</code> as an identifier was deprecated in Scala 2.10.</strong>
 * <p>
 *
 * @author Bill Venners
 */
trait GivenWhenThen {

  /**
   * <p>
   * <strong>The lower case form of <code>given</code> was
   * deprecated because using <code>then</code> as an identifier was deprecated in Scala 2.10.
   * Please use the upper case form, <code>Given</code>, instead.</strong>
   * <p>
   */
  @deprecated("Please use Given(message: String)(implicit info: Informer) instead.")
  def given(message: String)(implicit info: Informer) {
    Given(message)
  }
  
  /**
   * Forwards a message to an implicit <code>Informer</code>, preceded by "Given."
   *
   * @param message the message to forward to the passed informer
   * @param info the <code>Informer</code> to which to forward the message
   */
  def Given(message: String)(implicit info: Informer) {
    info(Resources("givenMessage", message))
  }

  /**
   * <p>
   * <strong>The lower case form of <code>when</code> was
   * deprecated because using <code>then</code> as an identifier was deprecated in Scala 2.10.
   * Please use the upper case form, <code>When</code>, instead.</strong>
   * <p>
   */
  @deprecated("Please use When(message: String)(implicit info: Informer) instead.")
  def when(message: String)(implicit info: Informer) {
    When(message)
  }
  
  /**
   * Forwards a message to an implicit <code>Informer</code>, preceded by "When ".
   *
   * @param message the message to forward to the passed informer
   * @param info the <code>Informer</code> to which to forward the message
   */
  def When(message: String)(implicit info: Informer) {
    info(Resources("whenMessage", message))
  }

  /**
   * <p>
   * <strong>The lower case form of <code>then</code> was
   * deprecated because using <code>then</code> as an identifier was deprecated in Scala 2.10.
   * Please use the upper case form, <code>Then</code>, instead.</strong>
   * <p>
   */
  @deprecated("Please use Then(message: String)(implicit info: Informer) instead.")
  def then(message: String)(implicit info: Informer) {
    Then(message)
  }
  
  /**
   * Forwards a message to an implicit <code>Informer</code>, preceded by "Then ".
   *
   * @param message the message to forward to the passed informer
   * @param info the <code>Informer</code> to which to forward the message
   */
  def Then(message: String)(implicit info: Informer) {
    info(Resources("thenMessage", message))
  }

  /**
   * <p>
   * <strong>The lower case form of <code>and</code> was
   * deprecated because using <code>then</code> as an identifier was deprecated in Scala 2.10.
   * Please use the upper case form, <code>And</code>, instead.</strong>
   * <p>
   */
  @deprecated("Please use And(message: String)(implicit info: Informer) instead.")
  def and(message: String)(implicit info: Informer) {
    And(message)
  }
  
   /**
   * Forwards a message to an implicit <code>Informer</code>, preceded by "And ".
   *
   * @param message the message to forward to the passed informer
   * @param info the <code>Informer</code> to which to forward the message
   */
  def And(message: String)(implicit info: Informer) {
    info(Resources("andMessage", message))
  }
}

/**
 * Companion object that facilitates the importing of <code>GivenWhenThen</code> members as
 * an alternative to mixing it in.
 *
 * @author Bill Venners
 */
object GivenWhenThen extends GivenWhenThen
