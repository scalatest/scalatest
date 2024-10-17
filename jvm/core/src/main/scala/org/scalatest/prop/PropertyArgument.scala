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
package org.scalatest.prop

/**
  * Describes one argument passed into a check.
  *
  * This is returned as part of [[PropertyCheckResult]], so that you can find out what
  * arguments were passed into the check.
  *
  * @param label The label provided for the argument, if any.
  * @param value The value of the argument.
  */
case class PropertyArgument(label: Option[String], value: Any)
