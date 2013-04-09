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
package org.scalatest.exceptions

/**
 * Exception that indicates an evaluation of a property should be discarded, because a condition
 * required by a property was not met by the passed values.
 *
 * This exception is thrown by the <code>whenever</code> method defined in trait <code>Whenever</code>
 * when the given condition is false. The <code>forAll</code> methods defined in trait <code>PropertyChecks</code>
 * catch the <code>DiscardedEvaluationException</code> and ignore it, moving on to try the next set of parameter values it is checking
 * th property against.
 */
class DiscardedEvaluationException extends RuntimeException

