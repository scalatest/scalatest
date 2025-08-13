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
package org.scalatest.exceptions

/**
 * Wrapper exception that wraps an non-serializable exception encountered in <a href="../events/Event.html"><code>Event</code></a>.
 *
 * @param msg a string that explains the problem
 * @param exceptionClassName the class name of the exception being wrapped
 * @param exceptionStackTrace the stack trace of the exception being wrapped
 *
 * @throws NullArgumentException if either <code>message</code> or <code>failedCodeStackDepthFun</code> is <code>null</code>
 */
case class NotSerializableWrapperException(msg: String, exceptionClassName: String, exceptionStackTrace: Array[StackTraceElement]) extends Exception with Serializable