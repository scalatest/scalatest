/*
 * Copyright 2001-2012 Artima, Inc.
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
 * Trait providing an implicit conversion that adds a <code>times</code> method to <code>Int</code>s that
 * will repeat a given side-effecting operation multiple times.
 * 
 * <p>
 * Here's an example in which a friendly greeting is printed three times:
 * </p>
 *
 * <pre class="stHighlight">
 * 3 times println("Hello again, world!")
 * </pre>
 *
 * <p>
 * Running the above code would yield this output:
 * </p>
 *
 * <pre>
 * Hello again, world!
 * Hello again, world!
 * Hello again, world!
 * </pre>
 *
 * <p>
 * If you need to repeat a block of statements multiple times, just enclose them in parentheses, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * 2 times {
 *  print("Hello ")
 *  print("again, ")
 *  println("world!")
 * }
 * </pre>
 *
 * <p>
 * Running the above code would yield:
 * </p>
 *
 * <pre>
 * Hello again, world!
 * Hello again, world!
 * </pre>
 *
 * <p>
 * This trait enables <code>times</code> to be invoked on 0 and any positive integer,
 * but attempting to invoke <code>times</code> on a negative integer will result in an <code>IllegalArgumentException</code>.
 * </p>
 *
 * @author Bill Venners
 */
trait TimesOnInt {

  /**
   * Class used via an implicit conversion to enable a <code>times</code> method to be invoked
   * on <code>Int</code>s to repeat a given side-effecting operation multiple times.
   *
   * <p>
   * When an instance of this class is constructed, 0 and any positive number may be passed as <code>num</code>,
   * but a negative number will result in an <code>IllegalArgumentException</code>. If constructed with 0, the <code>times</code> method
   * on the resulting instance will return without invoking the function passed to it. If constructed with 1, the <code>times</code> method
   * will invoke the function passed to it once then return. 
   * </p>
   *
   * @param num the integer to which the <code>times</code> method will be added.
   * @throws IllegalArgumentException if <code>num</code> is less than zero.
   */
  class Repeater(num: Int) {

    require(num >= 0, "The integer on which times was invoked was less than zero: " + num)

    /**
     * Executes the passed by-name parameter <code>num</code> number of times.
     *
     * <p>
     * If the function completes abruptly with an exception, this method will complete abruptly with the same
     * exception immediately. Thus in the case of an exception, this method may actually invoke the 
     * passed function fewer than <code>num</code> times.
     * </p>
     *
     * @param fun the by-name parameter to execute <code>num</code> times
     */
    def times(fun: => Unit) {
      var i = 0
      while (i < num) {
        fun
        i += 1
      }
    }
  }

  /**
   * Implicit conversion that adds a <code>times</code> method to <code>Int</code>s that
   * will repeat a given side-effecting operation multiple times.
   *
   * @param num the integer to which the <code>times</code> method will be added.
   */
  implicit def convertIntToRepeater(num: Int): Repeater = new Repeater(num)
}

/**
 * Companion object that facilitates the importing of <code>TimesOnInt</code> members as an alternative to mixing it in.
 *
 * <p>
 * One use case of this companion object is to import <code>TimesOnInt</code> members so you can use them in the Scala interpreter.
 * Here's an example: 
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; import org.scalatest.TimesOnInt._
 * import org.scalatest.TimesOnInt._
 * 
 * scala&gt; 3 times println("Hello again, world!")
 * Hello again, world!
 * Hello again, world!
 * Hello again, world!
 * </pre>
 */
object TimesOnInt extends TimesOnInt

