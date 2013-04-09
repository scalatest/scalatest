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
package org.scalautils

/**
 * Facilitates the &ldquo;<code>should ===</code>&rdquo; and  &ldquo;<code>should !==</code>&rdquo; syntax of ScalaTest's matchers DSL. 
 *  
 * <p>
 * Instances of this class are created and returned by the <code>===</code> and <code>!==</code> methods of
 * trait <a href="EqualityConstraints.html"><code>EqualityConstraints</code></a>.
 * </p>
 *
 * @param right the right-hand side value for an equality assertion
 * @param expectingEqual <code>true</code> if the result of a <code>===</code> invocation; <code>false</code> if the result of a <code>!==</code> invocation.
 */
case class TripleEqualsInvocation[T](right: T, expectingEqual: Boolean)

