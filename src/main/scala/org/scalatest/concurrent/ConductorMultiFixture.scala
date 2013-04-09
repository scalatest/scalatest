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
package org.scalatest.concurrent

import org.scalatest._
import fixture.ConfigMapFixture
import org.scalatest.fixture

/**
 * <strong>ConductorMultiFixture has been deprecated and will be removed in a future version of ScalaTest. Please
 * copy the withConductorFixture method into your own trait instead.</strong>
 *
 * <p>
 * This trait was originally intended for use with the "MultiFixture" traits in the <code>org.scalatest.fixture</code>
 * package. Those traits were deprecated in favor of using explicit method calls (rather than implicit ones). So this
 * trait is no longer relevant. You can still use the same approach if you want, of course, but since this will be
 * going away in a future version of ScalaTest, please just copy the source code of the <code>withConductorFixture</code>
 * method and place it into your own trait, then use that trait instead.
 * </p>
 *
 * @author Bill Venners
 */

@deprecated("Please copy the withConductorFixture method into your own trait instead.")
trait ConductorMultiFixture { this: fixture.Suite with ConfigMapFixture =>

  /**
   * Creates a new <code>Conductor</code>, passes the <code>Conductor</code> to the
   * specified test function, and ensures that <code>conduct</code> gets invoked
   * on the <code>Conductor</code>.
   *
   * <p>
   * After the test function returns (so long as it returns normally and doesn't
   * complete abruptly with an exception), this method will determine whether the
   * <code>conduct</code> method has already been called (by invoking
   * <code>conductingHasBegun</code> on the <code>Conductor</code>). If not,
   * this method will invoke <code>conduct</code> to ensure that the
   * multi-threaded test is actually conducted.
   * </p>
   *
   */
  implicit def withConductorFixture(fun: Conductor => Unit): this.FixtureParam => Unit = { configMap =>
    val conductor = new Conductor
    fun(conductor)
    if (!conductor.conductingHasBegun)
      conductor.conduct()
  }
}
