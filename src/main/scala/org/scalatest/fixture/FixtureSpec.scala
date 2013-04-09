/*
 * Copyright 2001-2011 Artima, Inc.
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

package org.scalatest.fixture

/**
 * <strong>FixtureSpec has been deprecated and will be removed in a future
 * release of ScalaTest. Please change any uses of <code>org.scalatest.fixture.FixtureSpec</code>
 * to a corresponding use of <a href="FunSpec.html"><code>org.scalatest.fixture.FunSpec</code></a>.</strong>
 * 
 * <p>
 * <strong> This is just
 * a rename, so the only thing you need to do is change the name. However, the recommended way to
 * write it is to import <code>org.scalatest.fixture</code> and then write <code>fixture.FunSpec</code> when
 * you use it, to differentiate it more clearly from <code>org.scalatest.FunSpec</code>. For example:
 * </strong>
 * </p>
 *
 * <pre>
 * import org.scalatest.fixture
 *
 * class ExampleSpec extends fixture.FunSpec {
 *   // ...
 * }
 * </pre>
 */
@deprecated("Please use org.scalatest.fixture.FunSpec instead.")
trait FixtureSpec extends FunSpec {
  override private[scalatest] val sourceFileName = "FixtureSpec.scala"
}
