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
package org.scalactic.source

import scala.quoted._

class TypeInfo[T](val name: String)

/**
  * Companion object for <code>Position</code> that defines an implicit
  * method that uses a macro to grab the enclosing position.
  */
object TypeInfo {
  def apply[T](name: String): TypeInfo[T] = new TypeInfo[T](name)

  implicit inline def gen[T]: TypeInfo[T] = ${ TypeInfoMacro.genTypeInfo[T] }
}
