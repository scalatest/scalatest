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
package org.scalatest

/**
 * Trait mixed into the result type of the <code>pending</code> statement of trait <code>Assertions</code>, which always throws <code>TestPendingException</code>. 
 *
 * <p>
 * This type is used primarily to facilitate the <code>is (pending)</code> syntax of
 * traits <a href="FlatSpec.html"><code>FlatSpec</code></a>, <a href="WordSpec.html"><code>WordSpec</code></a>, and
 * <a href="FlatSpec.html"><code>FLatSpec</code></a> as well the
 * <code>is (pending)</code> or <code>(pending)</code> syntax of sibling traits
 * in the <code>org.scalatest.fixture</code> package. Because the <code>pending</code>
 * method in <code>Assertions</code> always completes abruptly with an exception, its
 * type would be inferred to be <code>Nothing</code>, which is a relatively common
 * type. To make sure syntax like <code>is (pending)</code> only works with
 * method <code>pending</code>, it is helpful to have a specially named
 * "<code>Nothing</code>" type.
 * </p>
 */
sealed trait PendingStatement

