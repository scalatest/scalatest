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
package org.scalatest.concurrent

/**
 * Trait that defines an abstract <code>patienceConfig</code> method that is implemented in <a href="PatienceConfiguration.html"><code>PatienceConfiguration</code></a> and can
 * be overriden in stackable modification traits such as <a href="IntegrationPatience.html"><code>IntegrationPatience</code></a>.
 *
 * <p>
 * The main purpose of <code>AbstractPatienceConfiguration</code> is to differentiate core <code>PatienceConfiguration</code>
 * traits, such as <a href="Eventually.html"><code>Eventually</code></a> and <a href="Waiters.html"><code>Waiters</code></a>, from stackable
 * modification traits for <code>PatienceConfiguration</code>s such as <code>IntegrationPatience</code>.
 * Because these stackable traits extend <code>AbstractPatienceConfiguration</code> 
 * instead of <a href="../Suite.html"><code>Suite</code></a>, you can't simply mix in a stackable trait:
 * </p>
 *
 * <pre class="stHighlight">
 * class ExampleSpec extends FunSpec with IntegrationPatience // Won't compile
 * </pre>
 *
 * <p>
 * The previous code is undesirable because <code>IntegrationPatience</code> would have no affect on the class. Instead, you need to mix
 * in a core <code>PatienceConfiguration</code> trait and mix the stackable <code>IntegrationPatience</code> trait
 * into that, like this:
 * </p>
 *
 * <pre class="stHighlight">
 * class ExampleSpec extends FunSpec with Eventually with IntegrationPatience // Compiles fine
 * </pre>
 *
 * <p>
 * The previous code is better because <code>IntegrationPatience</code> does have an effect: it modifies the behavior
 * of <code>Eventually</code>.
 * </p>
 *
 * @author Bill Venners
 */
trait AbstractPatienceConfiguration extends ScaledTimeSpans {
  /**
   * Returns a <code>PatienceConfig</code> value providing default configuration values if implemented and made implicit in subtraits.
   */
  def patienceConfig: PatienceConfig
}
