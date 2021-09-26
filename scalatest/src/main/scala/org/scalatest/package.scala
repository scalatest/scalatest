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

  /**
   * Marker trait that serves as the result type of <code>assert</code>, <code>assume</code>, and <code>pending</code> methods of
   * trait <code>Assertions</code>, which return its only instance, the <code>Succeeded</code> singleton, or throw
   * an exception that indicates a failed, canceled, or pending test.
   */
  type Assertion = org.scalatest.compatible.Assertion

  /**
   * <strong>The name <code>org.scalatest.FunSuiteLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funsuite.AnyFunSuiteLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.FunSuiteLike trait has been moved and renamed. Please use org.scalatest.funsuite.AnyFunSuiteLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FunSuiteLike = funsuite.AnyFunSuiteLike

  /**
   * <strong>The name <code>org.scalatest.FunSuite</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funsuite.AnyFunSuite</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.FunSuite trait has been moved and renamed. Please use org.scalatest.funsuite.AnyFunSuite instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FunSuite = funsuite.AnyFunSuite

  /**
   * <strong>The name <code>org.scalatest.AsyncFunSuiteLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funsuite.AsyncFunSuiteLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.AsyncFunSuiteLike trait has been moved and renamed. Please use org.scalatest.funsuite.AsyncFunSuiteLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFunSuiteLike = funsuite.AsyncFunSuiteLike

  /**
   * <strong>The name <code>org.scalatest.AsyncFunSuite</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funsuite.AsyncFunSuite</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.AsyncFunSuite trait has been moved and renamed. Please use org.scalatest.funsuite.AsyncFunSuite instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFunSuite = funsuite.AsyncFunSuite
  
  /**
   * <strong>The name <code>org.scalatest.FeatureSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.featurespec.AnyFeatureSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.FeatureSpecLike trait has been moved and renamed. Please use org.scalatest.featurespec.AnyFeatureSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FeatureSpecLike = featurespec.AnyFeatureSpecLike

  /**
   * <strong>The name <code>org.scalatest.FeatureSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.featurespec.AnyFeatureSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.FeatureSpec trait has been moved and renamed. Please use org.scalatest.featurespec.AnyFeatureSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FeatureSpec = featurespec.AnyFeatureSpec

  /**
   * <strong>The name <code>org.scalatest.AsyncFeatureSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.featurespec.AsyncFeatureSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.AsyncFeatureSpecLike trait has been moved and renamed. Please use org.scalatest.featurespec.AsyncFeatureSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFeatureSpecLike = featurespec.AsyncFeatureSpecLike

  /**
   * <strong>The name <code>org.scalatest.AsyncFeatureSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.featurespec.AsyncFeatureSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.AsyncFeatureSpec trait has been moved and renamed. Please use org.scalatest.featurespec.AsyncFeatureSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFeatureSpec = featurespec.AsyncFeatureSpec
  
  /**
   * <strong>The name <code>org.scalatest.FunSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funspec.AnyFunSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.FunSpecLike trait has been moved and renamed. Please use org.scalatest.funspec.AnyFunSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FunSpecLike = funspec.AnyFunSpecLike

  /**
   * <strong>The name <code>org.scalatest.FunSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funspec.AnyFunSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.FunSpec trait has been moved and renamed. Please use org.scalatest.funspec.AnyFunSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FunSpec = funspec.AnyFunSpec

  /**
   * <strong>The name <code>org.scalatest.AsyncFunSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funspec.AsyncFunSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.AsyncFunSpecLike trait has been moved and renamed. Please use org.scalatest.funspec.AsyncFunSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFunSpecLike = funspec.AsyncFunSpecLike

  /**
   * <strong>The name <code>org.scalatest.AsyncFunSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funspec.AsyncFunSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.AsyncFunSpec trait has been moved and renamed. Please use org.scalatest.funspec.AsyncFunSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFunSpec = funspec.AsyncFunSpec
  
  /**
   * <strong>The name <code>org.scalatest.FreeSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.freespec.AnyFreeSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.FreeSpecLike trait has been moved and renamed. Please use org.scalatest.freespec.AnyFreeSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FreeSpecLike = freespec.AnyFreeSpecLike

  /**
   * <strong>The name <code>org.scalatest.FreeSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.freespec.AnyFreeSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.FreeSpec trait has been moved and renamed. Please use org.scalatest.freespec.AnyFreeSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FreeSpec = freespec.AnyFreeSpec

  /**
   * <strong>The name <code>org.scalatest.AsyncFreeSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.freespec.AsyncFreeSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.AsyncFreeSpecLike trait has been moved and renamed. Please use org.scalatest.freespec.AsyncFreeSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFreeSpecLike = freespec.AsyncFreeSpecLike

  /**
   * <strong>The name <code>org.scalatest.AsyncFreeSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.freespec.AsyncFreeSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.AsyncFreeSpec trait has been moved and renamed. Please use org.scalatest.freespec.AsyncFreeSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFreeSpec = freespec.AsyncFreeSpec
  
  /**
   * <strong>The name <code>org.scalatest.FlatSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.flatspec.AnyFlatSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.FlatSpecLike trait has been moved and renamed. Please use org.scalatest.flatspec.AnyFlatSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FlatSpecLike = flatspec.AnyFlatSpecLike

  /**
   * <strong>The name <code>org.scalatest.FlatSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.flatspec.AnyFlatSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.FlatSpec trait has been moved and renamed. Please use org.scalatest.flatspec.AnyFlatSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type FlatSpec = flatspec.AnyFlatSpec

  /**
   * <strong>The name <code>org.scalatest.AsyncFlatSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.flatspec.AsyncFlatSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.AsyncFlatSpecLike trait has been moved and renamed. Please use org.scalatest.flatspec.AsyncFlatSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFlatSpecLike = flatspec.AsyncFlatSpecLike

  /**
   * <strong>The name <code>org.scalatest.AsyncFlatSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.flatspec.AsyncFlatSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.AsyncFlatSpec trait has been moved and renamed. Please use org.scalatest.flatspec.AsyncFlatSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncFlatSpec = flatspec.AsyncFlatSpec
  
  /**
   * <strong>The name <code>org.scalatest.PropSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.propspec.AnyPropSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.PropSpecLike trait has been moved and renamed. Please use org.scalatest.propspec.AnyPropSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type PropSpecLike = propspec.AnyPropSpecLike

  /**
   * <strong>The name <code>org.scalatest.PropSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.propspec.AnyPropSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.PropSpec trait has been moved and renamed. Please use org.scalatest.propspec.AnyPropSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type PropSpec = propspec.AnyPropSpec

  /**
   * <strong>The name <code>org.scalatest.WordSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.wordspec.AnyWordSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.WordSpecLike trait has been moved and renamed. Please use org.scalatest.wordspec.AnyWordSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type WordSpecLike = wordspec.AnyWordSpecLike

  /**
   * <strong>The name <code>org.scalatest.WordSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.wordspec.AnyWordSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.WordSpec trait has been moved and renamed. Please use org.scalatest.wordspec.AnyWordSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type WordSpec = wordspec.AnyWordSpec

  /**
   * <strong>The name <code>org.scalatest.AsyncWordSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.wordspec.AsyncWordSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.AsyncWordSpecLike trait has been moved and renamed. Please use org.scalatest.wordspec.AsyncWordSpecLike instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncWordSpecLike = wordspec.AsyncWordSpecLike

  /**
   * <strong>The name <code>org.scalatest.AsyncWordSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.wordspec.AsyncWordSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.AsyncWordSpec trait has been moved and renamed. Please use org.scalatest.wordspec.AsyncWordSpec instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type AsyncWordSpec = wordspec.AsyncWordSpec
  
  /**
   * <strong>The name <code>org.scalatest.Matchers</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.should.Matchers</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.Matchers trait has been moved and renamed. Please use org.scalatest.matchers.should.Matchers instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type Matchers = matchers.should.Matchers

  /**
   * <strong>The name <code>org.scalatest.Matchers</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.should.Matchers</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.Matchers trait has been moved and renamed. Please use org.scalatest.matchers.should.Matchers instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  val Matchers = matchers.should.Matchers

  /**
   * <strong>The name <code>org.scalatest.MustMatchers</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.must.Matchers</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.MustMatchers trait has been moved and renamed. Please use org.scalatest.matchers.must.Matchers instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type MustMatchers = matchers.must.Matchers

  /**
   * <strong>The name <code>org.scalatest.MustMatchers</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.matchers.must.Matchers</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.MustMatchers trait has been moved and renamed. Please use org.scalatest.matchers.must.Matchers instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  val MustMatchers = matchers.must.Matchers

  /**
   * <strong>The name <code>org.scalatest.DiagrammedAssertions</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.diagrams.Diagrams</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.DiagrammedAssertions trait has been moved and renamed. Please use org.scalatest.diagrams.Diagrams instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  type DiagrammedAssertions = diagrams.Diagrams

  /**
   * <strong>The name <code>org.scalatest.DiagrammedAssertions</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.diagrams.Diagrams</code>, instead.</strong>
   *
   * <p>This type has been renamed to suport future modularization of ScalaTest.</p>
   *
   * <p>This can be rewritten automatically with autofix: <a href="https://github.com/scalatest/autofix/tree/master/3.1.x">https://github.com/scalatest/autofix/tree/master/3.1.x</a>.</p>
   */
  @deprecated("The org.scalatest.DiagrammedAssertions object has been moved and renamed. Please use org.scalatest.diagrams.Diagrams instead. This can be rewritten automatically with autofix: https://github.com/scalatest/autofix/tree/master/3.1.x", "3.1.0")
  val DiagrammedAssertions = diagrams.Diagrams
}
