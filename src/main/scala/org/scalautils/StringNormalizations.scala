/*
 * Copyright 2001-2013 Artima, Inc.
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
package org.scalautils

/**
 * Provides methods that produce <code>Normalization[String]</code> instances for various
 * ways to normalize strings.
 *
 * @author Bill Venners
 */
trait StringNormalizations {

  /**
   * Produces a <code>Normalization[String]</code> whose <code>normalized</code> method
   * returns the result of invoking <code>toLowerCase</code> on the passed string.
   *
   * @return a <code>Normalization[String]</code> that normalizes by transforming strings to lower case.
   */
  val lowerCased: Normalization[String] =
    new Normalization[String] {

      /**
       * Indicates whether the passed object is an instance of <code>String</code>.
       *
       * @return true if the passed object is a <code>String</code>.
       */
      def isInstanceOfA(b: Any) = b.isInstanceOf[String]

      /**
       * Returns the result of invoking <code>toLowerCase</code> on the passed string.
       *
       * @return the passed string transformed to lower case.
       */
      def normalized(s: String): String = s.toLowerCase
    }

  /**
   * Produces a <code>Normalization[String]</code> whose <code>normalized</code> method
   * returns the result of invoking <code>trim</code> on the passed string.
   *
   * @return a <code>Normalization[String]</code> that normalizes strings by trimming them.
   */
  val trimmed: Normalization[String] =
    new Normalization[String] {

      /**
       * Indicates whether the passed object is an instance of <code>String</code>.
       *
       * @return true if the passed object is a <code>String</code>.
       */
      def isInstanceOfA(b: Any) = b.isInstanceOf[String]

      /**
       * Returns the result of invoking <code>trimmed</code> on the passed string.
       *
       * @return the passed string with any white space trimmed off either end.
       */
      def normalized(s: String): String = s.trim
    }
} 

/**
 * Companion object to trait <code>StringNormalizations</code> that provides an 
 * alternative to mixing it in.
 */
object StringNormalizations extends StringNormalizations
