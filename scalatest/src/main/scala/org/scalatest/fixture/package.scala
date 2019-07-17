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

  @deprecated("Please use org.scalatest.funsuite.FixtureAnyFunSuiteLike instead")
  type FunSuiteLike = org.scalatest.funsuite.FixtureAnyFunSuiteLike

  @deprecated("Please use org.scalatest.funsuite.FixtureAnyFunSuite instead")
  type FunSuite = org.scalatest.funsuite.FixtureAnyFunSuite

  // SKIP-SCALATESTNATIVE-START
  @deprecated("Please use org.scalatest.funsuite.FixtureAsyncFunSuiteLike instead")
  type AsyncFunSuiteLike = org.scalatest.funsuite.FixtureAsyncFunSuiteLike

  @deprecated("Please use org.scalatest.funsuite.FixtureAsyncFunSuite instead")
  type AsyncFunSuite = org.scalatest.funsuite.FixtureAsyncFunSuite
  // SKIP-SCALATESTNATIVE-END

  @deprecated("Please use org.scalatest.featurespec.FixtureAnyFeatureSpecLike instead")
  type FeatureSpecLike = org.scalatest.featurespec.FixtureAnyFeatureSpecLike

  @deprecated("Please use org.scalatest.featurespec.FixtureAnyFeatureSpec instead")
  type FeatureSpec = org.scalatest.featurespec.FixtureAnyFeatureSpec

  // SKIP-SCALATESTNATIVE-START
  @deprecated("Please use org.scalatest.featurespec.FixtureAsyncFeatureSpecLike instead")
  type AsyncFeatureSpecLike = org.scalatest.featurespec.FixtureAsyncFeatureSpecLike

  @deprecated("Please use org.scalatest.featurespec.FixtureAsyncFeatureSpec instead")
  type AsyncFeatureSpec = org.scalatest.featurespec.FixtureAsyncFeatureSpec
  // SKIP-SCALATESTNATIVE-END
  
  @deprecated("Please use org.scalatest.funsuite.FixtureAnyFunSpec instead")
  type FunSpecLike = org.scalatest.funspec.FixtureAnyFunSpecLike

  @deprecated("Please use org.scalatest.funsuite.FixtureAnyFunSpecLike instead")
  type FunSpec = org.scalatest.funspec.FixtureAnyFunSpec

  // SKIP-SCALATESTNATIVE-START
  @deprecated("Please use org.scalatest.funspec.FixtureAsyncFunSpecLike instead")
  type AsyncFunSpecLike = org.scalatest.funspec.FixtureAsyncFunSpecLike

  @deprecated("Please use org.scalatest.funspec.FixtureAsyncFunSpec instead")
  type AsyncFunSpec = org.scalatest.funspec.FixtureAsyncFunSpec
  // SKIP-SCALATESTNATIVE-END

  @deprecated("Please use org.scalatest.freespec.FixtureAnyFreeSpecLike instead")
  type FreeSpecLike = org.scalatest.freespec.FixtureAnyFreeSpecLike

  @deprecated("Please use org.scalatest.freespec.FixtureAnyFreeSpec instead")
  type FreeSpec = org.scalatest.freespec.FixtureAnyFreeSpec

  // SKIP-SCALATESTNATIVE-START
  @deprecated("Please use org.scalatest.freespec.FixtureAsyncFreeSpecLike instead")
  type AsyncFreeSpecLike = org.scalatest.freespec.FixtureAsyncFreeSpecLike

  @deprecated("Please use org.scalatest.freespec.FixtureAsyncFreeSpec instead")
  type AsyncFreeSpec = org.scalatest.freespec.FixtureAsyncFreeSpec
  // SKIP-SCALATESTNATIVE-END

  @deprecated("Please use org.scalatest.flatspec.FixtureAnyFlatSpecLike instead")
  type FlatSpecLike = org.scalatest.flatspec.FixtureAnyFlatSpecLike

  @deprecated("Please use org.scalatest.flatspec.FixtureAnyFlatSpec instead")
  type FlatSpec = org.scalatest.flatspec.FixtureAnyFlatSpec

  // SKIP-SCALATESTNATIVE-START
  @deprecated("Please use org.scalatest.flatspec.FixtureAsyncFlatSpecLike instead")
  type AsyncFlatSpecLike = org.scalatest.flatspec.FixtureAsyncFlatSpecLike

  @deprecated("Please use org.scalatest.flatspec.FixtureAsyncFlatSpec instead")
  type AsyncFlatSpec = org.scalatest.flatspec.FixtureAsyncFlatSpec
  // SKIP-SCALATESTNATIVE-END

  @deprecated("Please use org.scalatest.propspec.FixtureAnyPropSpecLike instead")
  type PropSpecLike = org.scalatest.propspec.FixtureAnyPropSpecLike

  @deprecated("Please use org.scalatest.propspec.FixtureAnyPropSpec instead")
  type PropSpec = org.scalatest.propspec.FixtureAnyPropSpec

  @deprecated("Please use org.scalatest.wordspec.FixtureAnyWordSpec instead")
  type WordSpec = org.scalatest.wordspec.FixtureAnyWordSpec

  @deprecated("Please use org.scalatest.wordspec.FixtureAnyWordSpecLike instead")
  type WordSpecLike = org.scalatest.wordspec.FixtureAnyWordSpecLike

  // SKIP-SCALATESTNATIVE-START
  @deprecated("Please use org.scalatest.wordspec.FixtureAsyncWordSpecLike instead")
  type AsyncWordSpecLike = org.scalatest.wordspec.FixtureAsyncWordSpecLike

  @deprecated("Please use org.scalatest.wordspec.FixtureAsyncWordSpec instead")
  type AsyncWordSpec = org.scalatest.wordspec.FixtureAsyncWordSpec
  // SKIP-SCALATESTNATIVE-END
}
