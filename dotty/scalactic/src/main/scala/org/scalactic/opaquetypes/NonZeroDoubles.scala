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
package org.scalactic.opaquetypes

import org.scalactic.Resources

object NonZeroDoubles {

  opaque type NonZeroDouble = Double
  object NonZeroDouble {
    def ensuringValid(d: Double): NonZeroDouble = 
      if (d == 0.0) 
        throw new AssertionError(Resources.invalidNonZeroLong)
      else d
    def from(d: Double): Option[NonZeroDouble] =
      if (d == 0.0) None else Some(d)  
  }
}