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
package org.scalactic.exceptions

/**
 * Exception that indicates a validation failed.
 *
 * <p>
 * This exception is used to indicate and describe a failed validation by the <code>validating</code> methods
 * of [[org.scalactic.TrySugar <code>TrySugar</code>]] and [[org.scalactic.FutureSugar <code>FutureSugar</code>]].
 * </p>
 *
 * @param errorMessage a string describing the validation failure
 */
case class ValidationFailedException(errorMessage: String) extends Exception(errorMessage) {
  if (errorMessage == null) throw new NullPointerException("errorMessage was null")
}

