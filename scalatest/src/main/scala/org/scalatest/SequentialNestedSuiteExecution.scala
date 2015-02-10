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
 * Trait that causes the nested suites of any suite it is mixed into to be run sequentially even if
 * a <code>Distributor</code> is passed to <code>runNestedSuites</code>. This trait overrides the 
 * <code>runNestedSuites</code> method and fowards every parameter passed to it to a superclass invocation
 * of <code>runNestedSuites</code>, except it always passes <code>None</code> for the <code>Distributor</code>.
 * Mix in this trait into any suite whose nested suites need to be run sequentially even with the rest of the
 * run is being executed concurrently.
 */
trait SequentialNestedSuiteExecution extends SuiteMixin { this: Suite =>

  /**
   * This trait's implementation of <code>runNestedSuites</code>s invokes <code>runNestedSuites</code> on <code>super</code>,
   * passing in <code>None</code> for the <code>Distributor</code>.
   *
   * @param args the <code>Args</code> for this run
   * @return a <code>Status</code> object that indicates when all nested suites started by this method have completed, and whether or not a failure occurred.
   *
   * @throws NullPointerException if any passed parameter is <code>null</code>.
   */
  abstract override protected def runNestedSuites(args: Args): Status = {
    if (args == null)
      throw new NullPointerException("args was null")

    super.runNestedSuites(args.copy(distributor = None))
  }
}
