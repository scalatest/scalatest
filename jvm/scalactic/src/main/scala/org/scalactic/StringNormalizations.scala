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
 * Provides methods that produce <code>Uniformity[String]</code> instances for various
 * ways to normalize strings for equality comparisons.
 *
 * @author Bill Venners
 */
trait StringNormalizations {

  /**
   * Produces a <code>Uniformity[String]</code> whose <code>normalized</code> method
   * returns the result of invoking <code>toLowerCase</code> on the passed string.
   *
   * @return a <code>Uniformity[String]</code> that normalizes by transforming strings to lower case.
   */
  val lowerCased: Uniformity[String] =
    new AbstractStringUniformity {

      /**
       * Returns the result of invoking <code>toLowerCase</code> on the passed string.
       *
       * @return the passed string transformed to lower case.
       */
      def normalized(s: String): String = s.toLowerCase

      override def toString: String = "lowerCased"
    }

  /**
   * Produces a <code>Uniformity[String]</code> whose <code>normalized</code> method
   * returns the result of invoking <code>toUpperCase</code> on the passed string.
   *
   * @return a <code>Uniformity[String]</code> that normalizes by transforming strings to upper case.
   */
  val upperCased: Uniformity[String] =
    new AbstractStringUniformity {

      /**
       * Returns the result of invoking <code>toUpperCase</code> on the passed string.
       *
       * @return the passed string transformed to upper case.
       */
      def normalized(s: String): String = s.toUpperCase

      override def toString: String = "upperCased"
    }

  /**
   * Produces a <code>Uniformity[String]</code> whose <code>normalized</code> method
   * returns the result of invoking <code>trim</code> on the passed string.
   *
   * @return a <code>Uniformity[String]</code> that normalizes strings by trimming them.
   */
  val trimmed: Uniformity[String] =
    new AbstractStringUniformity {

      /**
       * Returns the result of invoking <code>trimmed</code> on the passed string.
       *
       * @return the passed string with any white space trimmed off either end.
       */
      def normalized(s: String): String = s.trim

      override def toString: String = "trimmed"
    }
} 

/**
 * Companion object to trait <code>StringNormalizations</code> that provides an 
 * alternative to mixing it in.
 */
object StringNormalizations extends StringNormalizations


