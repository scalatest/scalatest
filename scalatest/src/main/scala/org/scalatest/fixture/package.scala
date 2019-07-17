/*
 * Copyright 2001-2018 Artima, Inc.
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

/**
 * Package fixture deprecated types.
 */
package object fixture {

  @deprecated("The org.scalatest.fixture.FunSuiteLike trait has been moved and renamed. Please use org.scalatest.funsuite.FixtureAnyFunSuiteLike instead.")
  type FunSuiteLike = org.scalatest.funsuite.FixtureAnyFunSuiteLike

  @deprecated("The org.scalatest.fixture.FunSuite trait has been moved and renamed. Please use org.scalatest.funsuite.FixtureAnyFunSuite instead.")
  type FunSuite = org.scalatest.funsuite.FixtureAnyFunSuite

  // SKIP-SCALATESTNATIVE-START
  @deprecated("The org.scalatest.fixture.AsyncFunSuiteLike trait has been moved and renamed. Please use org.scalatest.funsuite.FixtureAsyncFunSuiteLike instead.")
  type AsyncFunSuiteLike = org.scalatest.funsuite.FixtureAsyncFunSuiteLike

  @deprecated("The org.scalatest.fixture.AsyncFunSuite trait has been moved and renamed. Please use org.scalatest.funsuite.FixtureAsyncFunSuite instead.")
  type AsyncFunSuite = org.scalatest.funsuite.FixtureAsyncFunSuite
  // SKIP-SCALATESTNATIVE-END

  @deprecated("The org.scalatest.fixture.FeatureSpecLike trait has been moved and renamed. Please use org.scalatest.featurespec.FixtureAnyFeatureSpecLike instead.")
  type FeatureSpecLike = org.scalatest.featurespec.FixtureAnyFeatureSpecLike

  @deprecated("The org.scalatest.fixture.FeatureSpec trait has been moved and renamed. Please use org.scalatest.featurespec.FixtureAnyFeatureSpec instead.")
  type FeatureSpec = org.scalatest.featurespec.FixtureAnyFeatureSpec

  // SKIP-SCALATESTNATIVE-START
  @deprecated("The org.scalatest.fixture.AsyncFeatureSpecLike trait has been moved and renamed. Please use org.scalatest.featurespec.FixtureAsyncFeatureSpecLike instead.")
  type AsyncFeatureSpecLike = org.scalatest.featurespec.FixtureAsyncFeatureSpecLike

  @deprecated("The org.scalatest.fixture.AsyncFeatureSpec trait has been moved and renamed. Please use org.scalatest.featurespec.FixtureAsyncFeatureSpec instead.")
  type AsyncFeatureSpec = org.scalatest.featurespec.FixtureAsyncFeatureSpec
  // SKIP-SCALATESTNATIVE-END
  
  @deprecated("The org.scalatest.fixture.FunSpecLike trait has been moved and renamed. Please use org.scalatest.funspec.FixtureAnyFunSpecLike instead.")
  type FunSpecLike = org.scalatest.funspec.FixtureAnyFunSpecLike

  @deprecated("The org.scalatest.fixture.FunSpec trait has been moved and renamed. Please use org.scalatest.funspec.FixtureAnyFunSpec instead.")
  type FunSpec = org.scalatest.funspec.FixtureAnyFunSpec

  // SKIP-SCALATESTNATIVE-START
  @deprecated("The org.scalatest.fixture.AsyncFunSpecLike trait has been moved and renamed. Please use org.scalatest.funspec.FixtureAsyncFunSpecLike instead.")
  type AsyncFunSpecLike = org.scalatest.funspec.FixtureAsyncFunSpecLike

  @deprecated("The org.scalatest.fixture.AsyncFunSpec trait has been moved and renamed. Please use org.scalatest.funspec.FixtureAsyncFunSpec instead.")
  type AsyncFunSpec = org.scalatest.funspec.FixtureAsyncFunSpec
  // SKIP-SCALATESTNATIVE-END

  @deprecated("The org.scalatest.fixture.FreeSpecLike trait has been moved and renamed. Please use org.scalatest.freespec.FixtureAnyFreeSpecLike instead.")
  type FreeSpecLike = org.scalatest.freespec.FixtureAnyFreeSpecLike

  @deprecated("The org.scalatest.fixture.FreeSpec trait has been moved and renamed. Please use org.scalatest.freespec.FixtureAnyFreeSpec instead.")
  type FreeSpec = org.scalatest.freespec.FixtureAnyFreeSpec

  // SKIP-SCALATESTNATIVE-START
  @deprecated("The org.scalatest.fixture.AsyncFreeSpecLike trait has been moved and renamed. Please use org.scalatest.freespec.FixtureAsyncFreeSpecLike instead.")
  type AsyncFreeSpecLike = org.scalatest.freespec.FixtureAsyncFreeSpecLike

  @deprecated("The org.scalatest.fixture.AsyncFreeSpec trait has been moved and renamed. Please use org.scalatest.freespec.FixtureAsyncFreeSpec instead.")
  type AsyncFreeSpec = org.scalatest.freespec.FixtureAsyncFreeSpec
  // SKIP-SCALATESTNATIVE-END

  @deprecated("The org.scalatest.fixture.FlatSpecLike trait has been moved and renamed. Please use org.scalatest.flatspec.FixtureAnyFlatSpecLike instead.")
  type FlatSpecLike = org.scalatest.flatspec.FixtureAnyFlatSpecLike

  @deprecated("The org.scalatest.fixture.FlatSpec trait has been moved and renamed. Please use org.scalatest.flatspec.FixtureAnyFlatSpec instead.")
  type FlatSpec = org.scalatest.flatspec.FixtureAnyFlatSpec

  // SKIP-SCALATESTNATIVE-START
  @deprecated("The org.scalatest.fixture.AsyncFlatSpecLike trait has been moved and renamed. Please use org.scalatest.flatspec.FixtureAsyncFlatSpecLike instead.")
  type AsyncFlatSpecLike = org.scalatest.flatspec.FixtureAsyncFlatSpecLike

  @deprecated("The org.scalatest.fixture.AsyncFlatSpec trait has been moved and renamed. Please use org.scalatest.flatspec.FixtureAsyncFlatSpec instead.")
  type AsyncFlatSpec = org.scalatest.flatspec.FixtureAsyncFlatSpec
  // SKIP-SCALATESTNATIVE-END

  @deprecated("The org.scalatest.fixture.PropSpecLike trait has been moved and renamed. Please use org.scalatest.flatspec.FixtureAnyPropSpecLike instead.")
  type PropSpecLike = org.scalatest.propspec.FixtureAnyPropSpecLike

  @deprecated("The org.scalatest.fixture.PropSpec trait has been moved and renamed. Please use org.scalatest.flatspec.FixtureAnyPropSpec instead.")
  type PropSpec = org.scalatest.propspec.FixtureAnyPropSpec

  @deprecated("The org.scalatest.fixture.WordSpec trait has been moved and renamed. Please use org.scalatest.wordspec.FixtureAnyWordSpec instead.")
  type WordSpec = org.scalatest.wordspec.FixtureAnyWordSpec

  @deprecated("The org.scalatest.fixture.WordSpecLike trait has been moved and renamed. Please use org.scalatest.wordspec.FixtureAnyWordSpecLike instead.")
  type WordSpecLike = org.scalatest.wordspec.FixtureAnyWordSpecLike

  // SKIP-SCALATESTNATIVE-START
  @deprecated("The org.scalatest.fixture.AsyncWordSpecLike trait has been moved and renamed. Please use org.scalatest.wordspec.FixtureAsyncWordSpecLike instead.")
  type AsyncWordSpecLike = org.scalatest.wordspec.FixtureAsyncWordSpecLike

  @deprecated("The org.scalatest.fixture.AsyncWordSpec trait has been moved and renamed. Please use org.scalatest.wordspec.FixtureAsyncWordSpec instead.")
  type AsyncWordSpec = org.scalatest.wordspec.FixtureAsyncWordSpec
  // SKIP-SCALATESTNATIVE-END
}
