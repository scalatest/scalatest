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

object NegDoubles {
  opaque type NegZDouble = Double
  object NegZDouble {
    def ensuringValid(d: Double): NegZDouble = 
      if (d > 0) 
        throw new AssertionError(Resources.invalidNegZDouble)
      else d
  }
  opaque type NegDouble = Double
  object NegDouble {
    def ensuringValid(d: Double): NegDouble = 
      if (d >= 0) 
        throw new AssertionError(Resources.invalidNegDouble)
      else d
  }
}