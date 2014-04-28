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
package org

/**
 * ScalaUtils's aliases kept for deprecation period.
 */
package object scalautils {

  @deprecated("Please use org.scalactic.Accumulation instead.")
  type Accumulation = org.scalactic.Accumulation

  @deprecated("Please use org.scalactic.Accumulation instead.")
  val Accumulation = org.scalactic.Accumulation

  @deprecated("Please use org.scalactic.TypeCheckedTripleEquals instead.")
  type TypeCheckedTripleEquals = org.scalactic.TypeCheckedTripleEquals

  @deprecated("Please use org.scalactic.TypeCheckedTripleEquals instead.")
  val TypeCheckedTripleEquals = org.scalactic.TypeCheckedTripleEquals

  @deprecated("Please use org.scalactic.Or instead.")
  type Or[+G,+B] = org.scalactic.Or[G, B]

  @deprecated("Please use org.scalactic.Or instead.")
  val Or = org.scalactic.Or

  @deprecated("Please use org.scalactic.Bad instead.")
  type Bad[+G,+B] = org.scalactic.Bad[G, B]

  @deprecated("Please use org.scalactic.Bad instead.")
  val Bad = org.scalactic.Bad

  @deprecated("Please use org.scalactic.Good instead.")
  type Good[+G,+B] = org.scalactic.Good[G, B]

  @deprecated("Please use org.scalactic.Good instead.")
  val Good = org.scalactic.Good

  @deprecated("Please use org.scalactic.Every instead.")
  type Every[+T] = org.scalactic.Every[T]

  @deprecated("Please use org.scalactic.Every instead.")
  val Every = org.scalactic.Every

  @deprecated("Please use org.scalactic.One instead.")
  type One[+T] = org.scalactic.One[T]

  @deprecated("Please use org.scalactic.One instead.")
  val One = org.scalactic.One

  @deprecated("Please use org.scalactic.Many instead.")
  type Many[+T] = org.scalactic.One[T]

  @deprecated("Please use org.scalactic.Many instead.")
  val Many = org.scalactic.Many

  @deprecated("Please use org.scalactic.ErrorMessage instead.")
  type ErrorMessage = org.scalactic.ErrorMessage

  @deprecated("Please use org.scalactic.Catcher instead.")
  type Catcher = org.scalactic.Catcher

  @deprecated("Please use org.scalactic.Catcher instead.")
  val Catcher = org.scalactic.Catcher

  @deprecated("Please use org.scalactic.ConversionCheckedLegacyTripleEquals instead.")
  type ConversionCheckedLegacyTripleEquals = org.scalactic.ConversionCheckedLegacyTripleEquals

  @deprecated("Please use org.scalactic.ConversionCheckedLegacyTripleEquals instead.")
  val ConversionCheckedLegacyTripleEquals = org.scalactic.ConversionCheckedLegacyTripleEquals

  @deprecated("Please use org.scalactic.Equality instead.")
  type Equality[A] = org.scalactic.Equality[A]

  @deprecated("Please use org.scalactic.Equality instead.")
  val Equality = org.scalactic.Equality

  @deprecated("Please use org.scalactic.Equivalence instead.")
  type Equivalence[T] = org.scalactic.Equivalence[T]

  @deprecated("Please use org.scalactic.Equivalence instead.")
  val Equivalence = org.scalactic.Equivalence

  @deprecated("Please use org.scalactic.Normalization instead.")
  type Normalization[A] = org.scalactic.Normalization[A]

  @deprecated("Please use org.scalactic.ConversionCheckedTripleEquals instead.")
  type ConversionCheckedTripleEquals = org.scalactic.ConversionCheckedTripleEquals

  @deprecated("Please use org.scalactic.ConversionCheckedTripleEquals instead.")
  val ConversionCheckedTripleEquals = org.scalactic.ConversionCheckedTripleEquals

  @deprecated("Please use org.scalactic.MapEqualityConstraints instead.")
  type MapEqualityConstraints = org.scalactic.MapEqualityConstraints

  @deprecated("Please use org.scalactic.MapEqualityConstraints instead.")
  val MapEqualityConstraints = org.scalactic.MapEqualityConstraints

  @deprecated("Please use org.scalactic.SeqEqualityConstraints instead.")
  type SeqEqualityConstraints = org.scalactic.SeqEqualityConstraints

  @deprecated("Please use org.scalactic.SeqEqualityConstraints instead.")
  val SeqEqualityConstraints = org.scalactic.SeqEqualityConstraints

  @deprecated("Please use org.scalactic.SetEqualityConstraints instead.")
  type SetEqualityConstraints = org.scalactic.SetEqualityConstraints

  @deprecated("Please use org.scalactic.SetEqualityConstraints instead.")
  val SetEqualityConstraints = org.scalactic.SetEqualityConstraints

  @deprecated("Please use org.scalactic.TraversableEqualityConstraints instead.")
  type TraversableEqualityConstraints = org.scalactic.TraversableEqualityConstraints

  @deprecated("Please use org.scalactic.TraversableEqualityConstraints instead.")
  val TraversableEqualityConstraints = org.scalactic.TraversableEqualityConstraints

  @deprecated("Please use org.scalactic.LegacyTripleEquals instead.")
  type LegacyTripleEquals = org.scalactic.LegacyTripleEquals

  @deprecated("Please use org.scalactic.LegacyTripleEquals instead.")
  val LegacyTripleEquals = org.scalactic.LegacyTripleEquals

  @deprecated("Please use org.scalactic.Tolerance instead.")
  type Tolerance = org.scalactic.Tolerance

  @deprecated("Please use org.scalactic.Tolerance instead.")
  val Tolerance = org.scalactic.Tolerance

  @deprecated("Please use org.scalactic.Explicitly instead.")
  type Explicitly = org.scalactic.Explicitly

  @deprecated("Please use org.scalactic.Explicitly instead.")
  val Explicitly = org.scalactic.Explicitly

  @deprecated("Please use org.scalactic.StringNormalizations instead.")
  type StringNormalizations = org.scalactic.StringNormalizations

  @deprecated("Please use org.scalactic.StringNormalizations instead.")
  val StringNormalizations = org.scalactic.StringNormalizations

  @deprecated("Please use org.scalactic.Uniformity instead.")
  type Uniformity[A] = org.scalactic.Uniformity[A]

  @deprecated("Please use org.scalactic.NormMethods instead.")
  type NormMethods = org.scalactic.NormMethods

  @deprecated("Please use org.scalactic.NormMethods instead.")
  val NormMethods = org.scalactic.NormMethods

  @deprecated("Please use org.scalactic.NormalizingEquality instead.")
  type NormalizingEquality[A] = org.scalactic.NormalizingEquality[A]

  @deprecated("Please use org.scalactic.Validation instead.")
  type Validation[+E] = org.scalactic.Validation[E]

  @deprecated("Please use org.scalactic.Pass instead.")
  val Pass = org.scalactic.Pass

  @deprecated("Please use org.scalactic.Fail instead.")
  type Fail[E] = org.scalactic.Fail[E]

  @deprecated("Please use org.scalactic.Fail instead.")
  val Fail = org.scalactic.Fail

  @deprecated("Please use org.scalactic.attempt instead.")
  def attempt[R](f: => R): R Or Throwable = org.scalactic.attempt(f)

  @deprecated("Please use org.scalactic.Prettifier instead.")
  type Prettifier = org.scalactic.Prettifier

  @deprecated("Please use org.scalactic.Prettifier instead.")
  val Prettifier = org.scalactic.Prettifier

  @deprecated("Please use org.scalactic.PrettyMethods instead.")
  type PrettyMethods = org.scalactic.PrettyMethods

  @deprecated("Please use org.scalactic.PrettyMethods instead.")
  val PrettyMethods = org.scalactic.PrettyMethods

  @deprecated("Please use org.scalactic.TripleEqualsSupport instead.")
  type TripleEqualsSupport = org.scalactic.TripleEqualsSupport

  @deprecated("Please use org.scalactic.TripleEqualsSupport instead.")
  val TripleEqualsSupport = org.scalactic.TripleEqualsSupport

  @deprecated("Please use org.scalactic.TripleEquals instead.")
  type TripleEquals = org.scalactic.TripleEquals

  @deprecated("Please use org.scalactic.TripleEquals instead.")
  val TripleEquals = org.scalactic.TripleEquals

  @deprecated("Please use org.scalactic.TolerantNumerics instead.")
  type TolerantNumerics = org.scalactic.TolerantNumerics

  @deprecated("Please use org.scalactic.TolerantNumerics instead.")
  val TolerantNumerics = org.scalactic.TolerantNumerics

  @deprecated("Please use org.scalactic.TypeCheckedLegacyTripleEquals instead.")
  type TypeCheckedLegacyTripleEquals = org.scalactic.TypeCheckedLegacyTripleEquals

  @deprecated("Please use org.scalactic.TypeCheckedLegacyTripleEquals instead.")
  val TypeCheckedLegacyTripleEquals = org.scalactic.TypeCheckedLegacyTripleEquals
}
