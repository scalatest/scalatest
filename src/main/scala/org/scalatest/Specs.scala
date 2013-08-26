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
package org.scalatest

/**
 * <strong>Class <code>Specs</code> has been deprecated and will be removed in a future version of ScalaTest. Please
 * change all uses of <code>Specs</code> to <a href="Suites.html"><code>Suites</code></a> instead.</code></strong>
 */
@deprecated("Specs has been deprecated and will be removed in a future version of ScalaTest. Please use Suites instead.")
class Specs(specsToNest: Suite*) extends Suite { thisSuite => 

  for (s <- specsToNest) {
    if (s == null)
      throw new NullPointerException("A passed suite was null")
  }

  /**
   * Returns an immutable <code>IndexedSeq</code> containing the suites passed to the constructor in
   * the order they were passed.
   */
  override val nestedSuites: collection.immutable.IndexedSeq[Suite] = Vector.empty ++ specsToNest

  /**
   * Returns a user friendly string for this suite, composed of the
   * simple name of the class (possibly simplified further by removing dollar signs if added by the Scala interpeter) and, if this suite
   * contains nested suites, the result of invoking <code>toString</code> on each
   * of the nested suites, separated by commas and surrounded by parentheses.
   *
   * @return a user-friendly string for this suite
   */
  override def toString: String = Suite.suiteToString(None, thisSuite)
}

/**
 * <strong>Object <code>Specs</code> has been deprecated and will be removed in a future version of ScalaTest. Please
 * change all uses of <code>Specs</code> to <a href="Suites.html"><code>Suites</code></a> instead.</code></strong>
 */
@deprecated("Specs has been deprecated and will be removed in a future version of ScalaTest. Please use Suites instead.")
object Specs {

  /**
   * Factory method for creating a <code>Suites</code> instance.
   */
  def apply(specsToNest: Suite*): Specs = new Specs(specsToNest: _*)
}

