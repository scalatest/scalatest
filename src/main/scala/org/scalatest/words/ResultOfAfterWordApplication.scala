/*
 * Copyright 2001-2009 Artima, Inc.
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
package org.scalatest.words

import org.scalatest._

/**
 * Class that supports the use of <em>after words</em> in <code>WordSpec</code>
 * and <code>fixture.WordSpec</code>.
 *
 * <p>
 * A <code>ResultOfAfterWordApplication</code>, which encapsulates the text of the after word
 * and a block,
 * is accepted by <code>when</code>, <code>should</code>, <code>must</code>, <code>can</code>, and <code>that</code>
 * methods.  For more information, see the
 * <a href="../WordSpec.html#AfterWords">main documentation</code></a> for trait <code>WordSpec</code>.
 * </p>
 */
final class ResultOfAfterWordApplication(val text: String, val f: () => Unit)

