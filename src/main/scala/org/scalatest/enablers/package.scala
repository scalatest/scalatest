/*
 * Copyright 2001-2014 Artima, Inc.
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
package org.scalatest

package object enablers {

  @deprecated("Please use org.scalactic.enablers.Collecting instead.")
  type Collecting[E, C] = org.scalactic.enablers.Collecting[E, C]

  @deprecated("Please use org.scalactic.enablers.Definition instead.")
  type Definition[-T] = org.scalactic.enablers.Definition[T]

  type EvidenceThat[T] = org.scalactic.enablers.EvidenceThat[T] // TODO, just change all these

  @deprecated("Please use org.scalactic.enablers.Emptiness instead.")
  type Emptiness[-T] = org.scalactic.enablers.Emptiness[T]

  @deprecated("Please use org.scalactic.enablers.Existence instead.")
  type Existence[-S] = org.scalactic.enablers.Existence[S]

  @deprecated("Please use org.scalactic.enablers.Length instead.")
  type Length[T] = org.scalactic.enablers.Length[T]

  @deprecated("Please use org.scalactic.enablers.Messaging instead.")
  type Messaging[T] = org.scalactic.enablers.Messaging[T]

  @deprecated("Please use org.scalactic.enablers.Readability instead.")
  type Readability[-T] = org.scalactic.enablers.Readability[T]

  @deprecated("Please use org.scalactic.enablers.Size instead.")
  type Size[T] = org.scalactic.enablers.Size[T]

  @deprecated("Please use org.scalactic.enablers.Sortable instead.")
  type Sortable[-S] = org.scalactic.enablers.Sortable[S]

  @deprecated("Please use org.scalactic.enablers.Writability instead.")
  type Writability[-T] = org.scalactic.enablers.Writability[T]
}


