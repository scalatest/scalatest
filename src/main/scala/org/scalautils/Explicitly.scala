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

/**
 * Provides <code>decidedBy</code> and <code>whenBothAre</code> syntax, which facilitates the
 * explicit specification of <code>Equality[T]</code> and/or <code>Uniformity[T]</code> where
 * <code>Equality[T]</code> is taken implicitly.
 *
 * @author Bill Venners
 */
trait Explicitly {

  class DecidedWord {
    def by[A](equality: Equality[A]): DecidedByEquality[A] = new DecidedByEquality[A](equality)
  }

  val decided = new DecidedWord

  class DeterminedWord {
    def by[A](equivalence: Equivalence[A]): DeterminedByEquivalence[A] = new DeterminedByEquivalence[A](equivalence)
  }

  val determined = new DeterminedWord

  class TheAfterWord {
    def being[N](uniformity: Uniformity[N])(implicit equality: Equality[N]): NormalizingEquality[N] =
      new ComposedNormalizingEquality[N](equality, uniformity)
    def being[N](normalization: Normalization[N])(implicit equivalence: Equivalence[N]): NormalizingEquivalence[N] =
      new ComposedNormalizingEquivalence[N](equivalence, normalization)
  }

  val after = new TheAfterWord
}

object Explicitly extends Explicitly

