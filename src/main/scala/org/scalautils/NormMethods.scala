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
package org.scalautils

trait NormMethods {

  final class Normalizer[T](o: T)(implicit normalization: Normalization[T]) {
    def norm: T = normalization.normalized(o)
  }

  implicit def convertToNormalizer[T](o: T)(implicit normalization: Normalization[T]): Normalizer[T] = new Normalizer[T](o)
} 

object NormMethods extends NormMethods

