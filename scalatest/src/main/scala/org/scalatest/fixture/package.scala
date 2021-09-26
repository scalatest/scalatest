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
 * Classes and traits supporting ScalaTest's "fixture" style traits, which
 * allow you to pass fixture objects into tests.
 *
 * This package is released as part of the `scalatest-core` module.
 */
package object fixture {

  /**
   * <strong>The name <code>org.scalatest.fixture.FunSuiteLikeXX</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funsuite.FixtureAnyFunSuiteLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.FunSuiteLike trait has been moved and renamed. Please use org.scalatest.funsuite.FixtureAnyFunSuiteLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FunSuiteLike = org.scalatest.funsuite.FixtureAnyFunSuiteLike

  /**
   * <strong>The name <code>org.scalatest.fixture.FunSuite</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funsuite.FixtureAnyFunSuite</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.FunSuite trait has been moved and renamed. Please use org.scalatest.funsuite.FixtureAnyFunSuite instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FunSuite = org.scalatest.funsuite.FixtureAnyFunSuite

  /**
   * <strong>The name <code>org.scalatest.fixture.AsyncFunSuiteLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funsuite.FixtureAsyncFunSuiteLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.AsyncFunSuiteLike trait has been moved and renamed. Please use org.scalatest.funsuite.FixtureAsyncFunSuiteLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFunSuiteLike = org.scalatest.funsuite.FixtureAsyncFunSuiteLike

  /**
   * <strong>The name <code>org.scalatest.fixture.AsyncFunSuite</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funsuite.FixtureAsyncFunSuite</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.AsyncFunSuite trait has been moved and renamed. Please use org.scalatest.funsuite.FixtureAsyncFunSuite instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFunSuite = org.scalatest.funsuite.FixtureAsyncFunSuite

  /**
   * <strong>The name <code>org.scalatest.fixture.FeatureSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.featurespec.FixtureAnyFeatureSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.FeatureSpecLike trait has been moved and renamed. Please use org.scalatest.featurespec.FixtureAnyFeatureSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FeatureSpecLike = org.scalatest.featurespec.FixtureAnyFeatureSpecLike

  /**
   * <strong>The name <code>org.scalatest.fixture.FeatureSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.featurespec.FixtureAnyFeatureSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.FeatureSpec trait has been moved and renamed. Please use org.scalatest.featurespec.FixtureAnyFeatureSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FeatureSpec = org.scalatest.featurespec.FixtureAnyFeatureSpec

  /**
   * <strong>The name <code>org.scalatest.fixture.AsyncFeatureSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.featurespec.FixtureAsyncFeatureSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.AsyncFeatureSpecLike trait has been moved and renamed. Please use org.scalatest.featurespec.FixtureAsyncFeatureSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFeatureSpecLike = org.scalatest.featurespec.FixtureAsyncFeatureSpecLike

  /**
   * <strong>The name <code>org.scalatest.fixture.AsyncFeatureSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.featurespec.FixtureAsyncFeatureSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.AsyncFeatureSpec trait has been moved and renamed. Please use org.scalatest.featurespec.FixtureAsyncFeatureSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFeatureSpec = org.scalatest.featurespec.FixtureAsyncFeatureSpec
  
  /**
   * <strong>The name <code>org.scalatest.fixture.FunSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funspec.FixtureAnyFunSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.FunSpecLike trait has been moved and renamed. Please use org.scalatest.funspec.FixtureAnyFunSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FunSpecLike = org.scalatest.funspec.FixtureAnyFunSpecLike

  /**
   * <strong>The name <code>org.scalatest.fixture.FunSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funspec.FixtureAnyFunSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.FunSpec trait has been moved and renamed. Please use org.scalatest.funspec.FixtureAnyFunSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FunSpec = org.scalatest.funspec.FixtureAnyFunSpec

  /**
   * <strong>The name <code>org.scalatest.fixture.AsyncFunSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funspec.FixtureAsyncFunSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.AsyncFunSpecLike trait has been moved and renamed. Please use org.scalatest.funspec.FixtureAsyncFunSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFunSpecLike = org.scalatest.funspec.FixtureAsyncFunSpecLike

  /**
   * <strong>The name <code>org.scalatest.fixture.AsyncFunSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funspec.FixtureAsyncFunSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.AsyncFunSpec trait has been moved and renamed. Please use org.scalatest.funspec.FixtureAsyncFunSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFunSpec = org.scalatest.funspec.FixtureAsyncFunSpec

  /**
   * <strong>The name <code>org.scalatest.fixture.FreeSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.freespec.FixtureAnyFreeSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.FreeSpecLike trait has been moved and renamed. Please use org.scalatest.freespec.FixtureAnyFreeSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FreeSpecLike = org.scalatest.freespec.FixtureAnyFreeSpecLike

  /**
   * <strong>The name <code>org.scalatest.fixture.FreeSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.freespec.FixtureAnyFreeSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.FreeSpec trait has been moved and renamed. Please use org.scalatest.freespec.FixtureAnyFreeSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FreeSpec = org.scalatest.freespec.FixtureAnyFreeSpec

  /**
   * <strong>The name <code>org.scalatest.fixture.AsyncFreeSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.freespec.FixtureAsyncFreeSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.AsyncFreeSpecLike trait has been moved and renamed. Please use org.scalatest.freespec.FixtureAsyncFreeSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFreeSpecLike = org.scalatest.freespec.FixtureAsyncFreeSpecLike

  /**
   * <strong>The name <code>org.scalatest.fixture.AsyncFreeSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.freespec.FixtureAsyncFreeSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.AsyncFreeSpec trait has been moved and renamed. Please use org.scalatest.freespec.FixtureAsyncFreeSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFreeSpec = org.scalatest.freespec.FixtureAsyncFreeSpec

  /**
   * <strong>The name <code>org.scalatest.fixture.FlatSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.flatspec.FixtureAnyFlatSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.FlatSpecLike trait has been moved and renamed. Please use org.scalatest.flatspec.FixtureAnyFlatSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FlatSpecLike = org.scalatest.flatspec.FixtureAnyFlatSpecLike

  /**
   * <strong>The name <code>org.scalatest.fixture.FlatSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.flatspec.FixtureAnyFlatSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.FlatSpec trait has been moved and renamed. Please use org.scalatest.flatspec.FixtureAnyFlatSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FlatSpec = org.scalatest.flatspec.FixtureAnyFlatSpec

  /**
   * <strong>The name <code>org.scalatest.fixture.AsyncFlatSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.flatspec.FixtureAsyncFlatSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.AsyncFlatSpecLike trait has been moved and renamed. Please use org.scalatest.flatspec.FixtureAsyncFlatSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFlatSpecLike = org.scalatest.flatspec.FixtureAsyncFlatSpecLike

  /**
   * <strong>The name <code>org.scalatest.fixture.AsyncFlatSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.flatspec.FixtureAsyncFlatSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.AsyncFlatSpec trait has been moved and renamed. Please use org.scalatest.flatspec.FixtureAsyncFlatSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFlatSpec = org.scalatest.flatspec.FixtureAsyncFlatSpec

  /**
   * <strong>The name <code>org.scalatest.fixture.PropSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.propspec.FixtureAnyPropSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.PropSpecLike trait has been moved and renamed. Please use org.scalatest.flatspec.FixtureAnyPropSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type PropSpecLike = org.scalatest.propspec.FixtureAnyPropSpecLike

  /**
   * <strong>The name <code>org.scalatest.fixture.PropSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.propspec.FixtureAnyPropSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.PropSpec trait has been moved and renamed. Please use org.scalatest.flatspec.FixtureAnyPropSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type PropSpec = org.scalatest.propspec.FixtureAnyPropSpec

  /**
   * <strong>The name <code>org.scalatest.fixture.WordSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.wordspec.FixtureAnyWordSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.WordSpec trait has been moved and renamed. Please use org.scalatest.wordspec.FixtureAnyWordSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type WordSpec = org.scalatest.wordspec.FixtureAnyWordSpec

  /**
   * <strong>The name <code>org.scalatest.fixture.WordSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.wordspec.FixtureAnyWordSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.WordSpecLike trait has been moved and renamed. Please use org.scalatest.wordspec.FixtureAnyWordSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type WordSpecLike = org.scalatest.wordspec.FixtureAnyWordSpecLike

  /**
   * <strong>The name <code>org.scalatest.fixture.AsyncWordSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.wordspec.FixtureAsyncWordSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.AsyncWordSpecLike trait has been moved and renamed. Please use org.scalatest.wordspec.FixtureAsyncWordSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncWordSpecLike = org.scalatest.wordspec.FixtureAsyncWordSpecLike

  /**
   * <strong>The name <code>org.scalatest.fixture.AsyncWordSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.wordspec.FixtureAsyncWordSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.AsyncWordSpec trait has been moved and renamed. Please use org.scalatest.wordspec.FixtureAsyncWordSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncWordSpec = org.scalatest.wordspec.FixtureAsyncWordSpec
  
  // SKIP-SCALATESTJS-START
  /**
   * <strong>The name <code>org.scalatest.fixture.AsyncTestRegistration</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.FixtureAsyncTestRegistration</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.AsyncTestRegistration trait has been moved and renamed. Please use org.scalatest.FixtureAsyncTestRegistration instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncTestRegistration = org.scalatest.FixtureAsyncTestRegistration

  /**
   * <strong>The name <code>org.scalatest.fixture.AsyncTestSuite</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.FixtureAsyncTestSuite</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.AsyncTestSuite trait has been moved and renamed. Please use org.scalatest.FixtureAsyncTestSuite instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncTestSuite = org.scalatest.FixtureAsyncTestSuite
  // SKIP-SCALATESTJS-END

  /**
   * <strong>The name <code>org.scalatest.fixture.Suite</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.FixtureSuite</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.Suite trait has been moved and renamed. Please use org.scalatest.FixtureSuite instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type Suite = org.scalatest.FixtureSuite

  /**
   * <strong>The name <code>org.scalatest.fixture.TestRegistration</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.FixtureTestRegistration</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.TestRegistration trait has been moved and renamed. Please use org.scalatest.FixtureTestRegistration instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type TestRegistration = org.scalatest.FixtureTestRegistration

  /**
   * <strong>The name <code>org.scalatest.fixture.TestSuite</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.FixtureTestSuite</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.fixture.TestSuite trait has been moved and renamed. Please use org.scalatest.FixtureTestSuite instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type TestSuite = org.scalatest.FixtureTestSuite
}
