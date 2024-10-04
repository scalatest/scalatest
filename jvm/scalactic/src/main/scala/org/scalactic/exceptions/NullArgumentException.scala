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
package org.scalactic.exceptions

/**
 * Exception that indicates a passed argument was <code>null</code>.
 *
 * <p>
 * Prior to ScalaTest and Scalactic 3.0.0, a <code>null</code> argument (where non-<code>null</code> was required)
 * would result in a <code>NullPointerException</code>. Although throwing <code>NullPointerException</code> is the
 * tradition in Java APIs, Scala.js does not support a <code>NullPointerException</code>. To make the behavior
 * the same on both the JVM and Scala.js, the change to throwing <code>NullArgumentException</code> was made throughout 
 * the ScalaTest and Scalactic, including the <code>requireNonNull</code> method of trait [[org.scalactic.Requirements <code>Requirements</code>]].
 * </p>
 *
 * @param message the detail message
 */
class NullArgumentException(message: String) extends IllegalArgumentException(message)

