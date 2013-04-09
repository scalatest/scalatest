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
package org.scalatest.matchers

/**
 * The result of a <code>Boolean</code> property match operation, such as one performed by a
 * <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>,
 * which contains one field that indicates whether the match succeeded (<em>i.e.</em>, the <code>Boolean</code>
 * property was <code>true</code>) and one field that provides
 * the name of the property.
 *
 * <p>
 * For an example of a <code>BePropertyMatchResult</code> in action, see the documentation for
 * <a href="BePropertyMatcher.html"><code>BePropertyMatcher</code></a>.
 * </p>
 *
 * @param matches indicates whether or not the matcher matched (if the <code>Boolean</code> property was true, it was a match)
 * @param propertyName the name of the <code>Boolean</code> property that was matched against
 *
 * @author Bill Venners
 */
final case class BePropertyMatchResult(
  val matches: Boolean, // true if the Boolean property was true
  val propertyName: String
)

/**
 * Companion object for the <code>BePropertyMatchResult</code> case class.
 *
 * @author Bill Venners
 */
object BePropertyMatchResult
