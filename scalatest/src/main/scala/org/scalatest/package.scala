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
package org

/**
 * ScalaTest's main traits, classes, and other members, including members supporting ScalaTest's DSL for the Scala interpreter.
 */
package object scalatest {

   // SKIP-SCALATESTJS,NATIVE-START
  private val defaultShell = ShellImpl()

  /**
   * Returns a copy of this <code>Shell</code> with <code>colorPassed</code> configuration parameter set to <code>true</code>.
   */
  lazy val color: Shell = defaultShell.color

  /**
   * Returns a copy of this <code>Shell</code> with <code>durationsPassed</code> configuration parameter set to <code>true</code>.
   */
  lazy val durations: Shell = defaultShell.durations

  /**
   * Returns a copy of this <code>Shell</code> with <code>shortStacksPassed</code> configuration parameter set to <code>true</code>.
   */
  lazy val shortstacks: Shell = defaultShell.shortstacks

  /**
   * Returns a copy of this <code>Shell</code> with <code>fullStacksPassed</code> configuration parameter set to <code>true</code>.
   */
  lazy val fullstacks: Shell = defaultShell.fullstacks

  /**
   * Returns a copy of this <code>Shell</code> with <code>statsPassed</code> configuration parameter set to <code>true</code>.
   */
  lazy val stats: Shell = defaultShell.stats

  /**
   * Returns a copy of this <code>Shell</code> with <code>colorPassed</code> configuration parameter set to <code>false</code>.
   */
  lazy val nocolor: Shell = defaultShell.nocolor

  /**
   * Returns a copy of this <code>Shell</code> with <code>durationsPassed</code> configuration parameter set to <code>false</code>.
   */
  lazy val nodurations: Shell = defaultShell.nodurations

  /**
   * Returns a copy of this <code>Shell</code> with <code>shortStacksPassed</code> configuration parameter set to <code>false</code>.
   */
  lazy val nostacks: Shell = defaultShell.nostacks

  /**
   * Returns a copy of this <code>Shell</code> with <code>statsPassed</code> configuration parameter set to <code>false</code>.
   */
  lazy val nostats: Shell = defaultShell.nostats
  // SKIP-SCALATESTJS,NATIVE-END

  /**
   * The version number of ScalaTest.
   *
   * @return the ScalaTest version number.
   */
  val ScalaTestVersion: String = ScalaTestVersions.ScalaTestVersion

  private[scalatest] type Expectation = Fact

  /* 
   * Marker trait that serves as the result type of <code>assert</code>, <code>assume</code>, and <code>pending</code> methods of
   * trait <code>Assertions</code>, which return its only instance, the <code>Succeeded</code> singleton, or throw
   * an exception that indicates a failed, canceled, or pending test.
   */
  type Assertion = compatible.Assertion

  /**
   * <strong>The name <code>org.scalatest.FunSuiteLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funsuite.AnyFunSuiteLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.FunSuiteLike trait has been moved and renamed. Please use org.scalatest.funsuite.AnyFunSuiteLike instead.")
  type FunSuiteLike = funsuite.AnyFunSuiteLike

  /**
   * <strong>The name <code>org.scalatest.FunSuite</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funsuite.AnyFunSuite</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.FunSuite trait has been moved and renamed. Please use org.scalatest.funsuite.AnyFunSuite instead.")
  type FunSuite = funsuite.AnyFunSuite

  // SKIP-SCALATESTNATIVE-START
  /**
   * <strong>The name <code>org.scalatest.AsyncFunSuiteLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funsuite.AsyncFunSuiteLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.AsyncFunSuiteLike trait has been moved and renamed. Please use org.scalatest.funsuite.AsyncFunSuiteLike instead.")
  type AsyncFunSuiteLike = funsuite.AsyncFunSuiteLike

  /**
   * <strong>The name <code>org.scalatest.AsyncFunSuite</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funsuite.AsyncFunSuite</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.AsyncFunSuite trait has been moved and renamed. Please use org.scalatest.funsuite.AsyncFunSuite instead.")
  type AsyncFunSuite = funsuite.AsyncFunSuite
  // SKIP-SCALATESTNATIVE-END

  /**
   * <strong>The name <code>org.scalatest.FeatureSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.featurespec.AnyFeatureSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.FeatureSpecLike trait has been moved and renamed. Please use org.scalatest.featurespec.AnyFeatureSpecLike instead.")
  type FeatureSpecLike = featurespec.AnyFeatureSpecLike

  /**
   * <strong>The name <code>org.scalatest.FeatureSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.featurespec.AnyFeatureSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.FeatureSpec trait has been moved and renamed. Please use org.scalatest.featurespec.AnyFeatureSpec instead.")
  type FeatureSpec = featurespec.AnyFeatureSpec

  // SKIP-SCALATESTNATIVE-START
  /**
   * <strong>The name <code>org.scalatest.AsyncFeatureSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.featurespec.AsyncFeatureSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.AsyncFeatureSpecLike trait has been moved and renamed. Please use org.scalatest.featurespec.AsyncFeatureSpecLike instead.")
  type AsyncFeatureSpecLike = featurespec.AsyncFeatureSpecLike

  /**
   * <strong>The name <code>org.scalatest.AsyncFeatureSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.featurespec.AsyncFeatureSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.AsyncFeatureSpec trait has been moved and renamed. Please use org.scalatest.featurespec.AsyncFeatureSpec instead.")
  type AsyncFeatureSpec = featurespec.AsyncFeatureSpec
  // SKIP-SCALATESTNATIVE-END

  /**
   * <strong>The name <code>org.scalatest.FunSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funspec.AnyFunSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.FunSpecLike trait has been moved and renamed. Please use org.scalatest.funspec.AnyFunSpecLike instead.")
  type FunSpecLike = funspec.AnyFunSpecLike

  /**
   * <strong>The name <code>org.scalatest.FunSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funspec.AnyFunSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.FunSpec trait has been moved and renamed. Please use org.scalatest.funspec.AnyFunSpec instead.")
  type FunSpec = funspec.AnyFunSpec

  // SKIP-SCALATESTNATIVE-START
  /**
   * <strong>The name <code>org.scalatest.AsyncFunSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funspec.AsyncFunSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.AsyncFunSpecLike trait has been moved and renamed. Please use org.scalatest.funspec.AsyncFunSpecLike instead.")
  type AsyncFunSpecLike = funspec.AsyncFunSpecLike

  /**
   * <strong>The name <code>org.scalatest.AsyncFunSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funspec.AsyncFunSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.AsyncFunSpec trait has been moved and renamed. Please use org.scalatest.funspec.AsyncFunSpec instead.")
  type AsyncFunSpec = funspec.AsyncFunSpec
  // SKIP-SCALATESTNATIVE-END

  /**
   * <strong>The name <code>org.scalatest.FreeSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.freespec.AnyFreeSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.FreeSpecLike trait has been moved and renamed. Please use org.scalatest.freespec.AnyFreeSpecLike instead.")
  type FreeSpecLike = freespec.AnyFreeSpecLike

  /**
   * <strong>The name <code>org.scalatest.FreeSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.freespec.AnyFreeSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.FreeSpec trait has been moved and renamed. Please use org.scalatest.freespec.AnyFreeSpec instead.")
  type FreeSpec = freespec.AnyFreeSpec

  // SKIP-SCALATESTNATIVE-START
  /**
   * <strong>The name <code>org.scalatest.AsyncFreeSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.freespec.AsyncFreeSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.AsyncFreeSpecLike trait has been moved and renamed. Please use org.scalatest.freespec.AsyncFreeSpecLike instead.")
  type AsyncFreeSpecLike = freespec.AsyncFreeSpecLike

  /**
   * <strong>The name <code>org.scalatest.AsyncFreeSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.freespec.AsyncFreeSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.AsyncFreeSpec trait has been moved and renamed. Please use org.scalatest.freespec.AsyncFreeSpec instead.")
  type AsyncFreeSpec = freespec.AsyncFreeSpec
  // SKIP-SCALATESTNATIVE-END

  /**
   * <strong>The name <code>org.scalatest.FlatSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.flatspec.AnyFlatSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.FlatSpecLike trait has been moved and renamed. Please use org.scalatest.flatspec.AnyFlatSpecLike instead.")
  type FlatSpecLike = flatspec.AnyFlatSpecLike

  /**
   * <strong>The name <code>org.scalatest.FlatSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.flatspec.AnyFlatSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.FlatSpec trait has been moved and renamed. Please use org.scalatest.flatspec.AnyFlatSpec instead.")
  type FlatSpec = flatspec.AnyFlatSpec

  /**
   * <strong>The name <code>org.scalatest.AsyncFlatSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.flatspec.AsyncFlatSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  // SKIP-SCALATESTNATIVE-START
  @deprecated("The org.scalatest.AsyncFlatSpecLike trait has been moved and renamed. Please use org.scalatest.flatspec.AsyncFlatSpecLike instead.")
  type AsyncFlatSpecLike = flatspec.AsyncFlatSpecLike

  /**
   * <strong>The name <code>org.scalatest.AsyncFlatSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.flatspec.AsyncFlatSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.AsyncFlatSpec trait has been moved and renamed. Please use org.scalatest.flatspec.AsyncFlatSpec instead.")
  type AsyncFlatSpec = flatspec.AsyncFlatSpec
  // SKIP-SCALATESTNATIVE-END

  /**
   * <strong>The name <code>org.scalatest.PropSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.propspec.AnyPropSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.PropSpecLike trait has been moved and renamed. Please use org.scalatest.propspec.AnyPropSpecLike instead.")
  type PropSpecLike = propspec.AnyPropSpecLike

  /**
   * <strong>The name <code>org.scalatest.PropSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.propspec.AnyPropSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.PropSpec trait has been moved and renamed. Please use org.scalatest.propspec.AnyPropSpec instead.")
  type PropSpec = propspec.AnyPropSpec

  /**
   * <strong>The name <code>org.scalatest.WordSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.wordspec.AnyWordSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.WordSpecLike trait has been moved and renamed. Please use org.scalatest.wordspec.AnyWordSpecLike instead.")
  type WordSpecLike = wordspec.AnyWordSpecLike

  /**
   * <strong>The name <code>org.scalatest.WordSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.wordspec.AnyWordSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.WordSpec trait has been moved and renamed. Please use org.scalatest.wordspec.AnyWordSpec instead.")
  type WordSpec = wordspec.AnyWordSpec

  // SKIP-SCALATESTNATIVE-START
  /**
   * <strong>The name <code>org.scalatest.AsyncWordSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.wordspec.AsyncWordSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.AsyncWordSpecLike trait has been moved and renamed. Please use org.scalatest.wordspec.AsyncWordSpecLike instead.")
  type AsyncWordSpecLike = wordspec.AsyncWordSpecLike

  /**
   * <strong>The name <code>org.scalatest.AsyncWordSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.wordspec.AsyncWordSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.AsyncWordSpec trait has been moved and renamed. Please use org.scalatest.wordspec.AsyncWordSpec instead.")
  type AsyncWordSpec = wordspec.AsyncWordSpec
  // SKIP-SCALATESTNATIVE-END
}
