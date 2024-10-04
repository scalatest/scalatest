/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest.tagobjects

import org.scalatest.Tag

/**
 * Tag object that indicates a test is CPU-intensive (<em>i.e.</em>, consumes a lot of CPU time when it runs).
 *
 * <p>
 * The corresponding tag annotation for this tag object is <code>org.scalatest.tags.CPU</code>.
 * This tag object can be used to tag test functions (in style traits other than <code>Spec</code>, in which tests are methods not functions) as being CPU-intensive.
 * See the "tagging tests" section in the documentation for your chosen styles to see the syntax. Here's an example for <code>FlatSpec</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * package org.scalatest.examples.tagobjects.cpu
 * 
 * import org.scalatest._
 * import tagobjects.CPU
 * 
 * class SetSpec extends FlatSpec {
 * 
 *   "An empty Set" should "have size 0" taggedAs(CPU) in {
 *     assert(Set.empty.size === 0)
 *   }
 * }
 * </pre>
 */
object CPU extends Tag("org.scalatest.tags.CPU") 
