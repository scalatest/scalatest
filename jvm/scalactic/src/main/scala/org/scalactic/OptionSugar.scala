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
package org.scalactic

/**
 * Trait providing an implicit class that adds a <code>toOr</code> method to
 * <code>Option</code>, which converts <code>Some</code> to <code>Good</code>,
 * <code>None</code> to <code>Bad</code>.
 *
 * <p>
 * You can use the <code>toOr</code> method to record information about why
 * a processing of nested <code>Option</code>s resulted in <code>None</code>.
 * For example, the following <code>for</code> expression results in
 * <code>None</code> if either the passed optional <code>Person</code> is
 * <code>None</code> or else if the contained optional age is <code>None</code>:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; case class Person(name: String, age: Option[Int])
 * defined class Person
 *
 * scala&gt; def ageOf(person: Option[Person]) =
     |   for {
     |     per &lt;- person
     |     age &lt;- per.age
     |   } yield age
 * ageOf: (person: Option[Person])Option[Int]
 *
 * scala&gt; ageOf(Some(Person("Ralph", Some(32))))
 * res0: Option[Int] = Some(32)
 *
 * scala&gt; ageOf(Some(Person("Curt", None)))
 * res3: Option[Int] = None
 *
 * scala&gt; ageOf(None)
 * res2: Option[Int] = None
 * </pre>
 *
 * <p>
 * If you instead populate the <code>for</code> expression with <code>Or</code>s,
 * supplying an error message or other "bad" value to the <code>toOr</code> method
 * in case of <code>None</code>, you'll get an indication of which part
 * failed if a <code>None</code> is encountered:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import OptionSugar._
 * import OptionSugar._
 *
 * scala&gt; def ageOf(person: Option[Person]) =
 *      |   for {
 *      |     per &lt;- person toOr "no person here"
 *      |     age &lt;- per.age toOr "ageless person"
 *      |   } yield age
 * ageOf: (person: Option[Person])org.scalactic.Or[Int,String]
 *
 * scala&gt; ageOf(Some(Person("Ralph", Some(32))))
 * res1: org.scalactic.Or[Int,String] = Good(32)
 *
 * scala&gt; ageOf(Some(Person("Curt", None)))
 * res2: org.scalactic.Or[Int,String] = Bad(ageless person)
 *
 * scala&gt; ageOf(None)
 * res3: org.scalactic.Or[Int,String] = Bad(no person here)
 * </pre>
 */
trait OptionSugar {

  /**
   * Implicit class that adds a <code>toOr</code> method to
   * <code>Option</code>, which converts <code>Some</code> to <code>Good</code>,
   * <code>None</code> to <code>Bad</code>.
   */
  implicit class Optionizer[G](option: Option[G]) {

    /**
     * Converts the wrapped <code>Option</code> to an <code>Or</code>.
     */
    def toOr[B](orElse: => B): G Or B = Or.from(option, orElse)
  }
} 

/**
 * Companion object for <code>OptionSugar</code> enabling its members to be
 * imported as an alternative to mixing them in.
 */
object OptionSugar extends OptionSugar

