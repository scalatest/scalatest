/*
 * Copyright 2001-2019 Artima, Inc.
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
package org.scalatestplus.scalacheck

private[scalacheck] object FailureMessages {

   def propertyException(prettifier: org.scalactic.Prettifier, param0: Any): String = Resources.propertyException(prettifier.apply(param0))

   def propCheckExhaustedAfterOne(prettifier: org.scalactic.Prettifier, param0: Any): String = Resources.propCheckExhaustedAfterOne(prettifier.apply(param0))

   def propCheckExhausted(prettifier: org.scalactic.Prettifier, param0: Any, param1: Any): String = Resources.propCheckExhausted(prettifier.apply(param0), prettifier.apply(param1))

   def propertyFailed(prettifier: org.scalactic.Prettifier, param0: Any): String = Resources.propertyFailed(prettifier.apply(param0)) 

   def propertyCheckSucceeded(): String = Resources.propertyCheckSucceeded()

   def thrownExceptionsLocation(prettifier: org.scalactic.Prettifier, param0: Any): String = Resources.thrownExceptionsLocation(prettifier.apply(param0))

   def occurredOnValues(): String = Resources.occurredOnValues()

   def thrownExceptionsMessage(prettifier: org.scalactic.Prettifier, param0: Any): String = Resources.thrownExceptionsMessage(prettifier.apply(param0))
}