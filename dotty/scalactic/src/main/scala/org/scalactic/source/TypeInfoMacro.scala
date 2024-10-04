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

/**
  * Helper class for Position macro. (Will be removed from the public API if possible in a subsequent 3.0.0-RCx release.)
  */
object TypeInfoMacro {

  /**
    * Helper method for TypeInfo macro.
    */
  def genTypeInfo[T: Type](using Quotes): Expr[TypeInfo[T]] = {
    val name = Expr(Type.show[T])
    '{ TypeInfo[T]($name) }
  }
}