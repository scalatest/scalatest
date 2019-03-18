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

  @deprecated("Please use org.scalatest.funsuite.FixtureAnyFunSuite instead")
  type FunSuiteLike = org.scalatest.funsuite.FixtureAnyFunSuiteLike

  // SKIP-SCALATESTNATIVE-START
  @deprecated("Please use org.scalatest.funsuite.FixtureAsyncFunSuiteLike instead")
  type AsyncFunSuiteLike = org.scalatest.funsuite.FixtureAsyncFunSuiteLike
  // SKIP-SCALATESTNATIVE-END

  @deprecated("Please use org.scalatest.featurespec.FixtureAnyFeatureSpec instead")
  type FeatureSpecLike = org.scalatest.featurespec.FixtureAnyFeatureSpecLike

  // SKIP-SCALATESTNATIVE-START
  @deprecated("Please use org.scalatest.featurespec.FixtureAsyncFeatureSpecLike instead")
  type AsyncFeatureSpecLike = org.scalatest.featurespec.FixtureAsyncFeatureSpecLike
  // SKIP-SCALATESTNATIVE-END
  
  @deprecated("Please use org.scalatest.funsuite.FixtureAnyFunSpec instead")
  type FunSpecLike = org.scalatest.funspec.FixtureAnyFunSpec

  // SKIP-SCALATESTNATIVE-START
  @deprecated("Please use org.scalatest.funspec.FixtureAsyncFunSpecLike instead")
  type AsyncFunSpecLike = org.scalatest.funspec.FixtureAsyncFunSpecLike
  // SKIP-SCALATESTNATIVE-END

  @deprecated("Please use org.scalatest.freespec.FixtureAnyFreeSpec instead")
  type FreeSpecLike = org.scalatest.freespec.FixtureAnyFreeSpec

  // SKIP-SCALATESTNATIVE-START
  @deprecated("Please use org.scalatest.freespec.FixtureAsyncFreeSpecLike instead")
  type AsyncFreeSpecLike = org.scalatest.freespec.FixtureAsyncFreeSpecLike
  // SKIP-SCALATESTNATIVE-END

  @deprecated("Please use org.scalatest.flatspec.FixtureAnyFlatSpec instead")
  type FlatSpecLike = org.scalatest.flatspec.FixtureAnyFlatSpec

  // SKIP-SCALATESTNATIVE-START
  @deprecated("Please use org.scalatest.flatspec.FixtureAsyncFlatSpecLike instead")
  type AsyncFlatSpecLike = org.scalatest.flatspec.FixtureAsyncFlatSpecLike
  // SKIP-SCALATESTNATIVE-END
}
