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

  @deprecated("Please use PendingStatement instead")
  type PendingNothing = PendingStatement

  private[scalatest] type Expectation = Fact

  /* 
   * Marker trait that serves as the result type of <code>assert</code>, <code>assume</code>, and <code>pending</code> methods of
   * trait <code>Assertions</code>, which return its only instance, the <code>Succeeded</code> singleton, or throw
   * an exception that indicates a failed, canceled, or pending test.
   */
  type Assertion = compatible.Assertion

  // SKIP-SCALATESTJS,NATIVE-START
  /**
   * <strong>The name <code>org.scalatest.SpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.refspec.RefSpecLike</code>, instead.</strong>
   *
   * <p>
   * Because this style uses reflection at runtime to discover scopes and tests, it can only be supported on the JVM, not Scala.js.
   * Thus in ScalaTest 3.0.0, class <code>org.scalatest.SpecLike</code> was moved to the <code>org.scalatest.refspec</code> package and renamed
   * <code>RefSpecLike</code>, with the intention of later moving it to a separate module available only on the JVM.
   * </p>
   */
  @deprecated("Please use org.scalatest.refspec.RefSpecLike instead")
  type SpecLike = refspec.RefSpecLike

  /**
   * <strong>The name <code>org.scalatest.Spec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.refspec.RefSpec</code>, instead.</strong>
   *
   * <p>
   * Because this style uses reflection at runtime to discover scopes and tests, it can only be supported on the JVM, not Scala.js.
   * Thus in ScalaTest 3.0.0, class <code>org.scalatest.Spec</code> was moved to the <code>org.scalatest.refspec</code> package and renamed
   * <code>RefSpec</code>, with the intention of later moving it to a separate module available only on the JVM.
   * </p>
   */
  @deprecated("Please use org.scalatest.refspec.RefSpec instead")
  type Spec = refspec.RefSpec
  // SKIP-SCALATESTJS,NATIVE-END

  @deprecated("Please use org.scalatest.funsuite.AnyFunSuiteLike instead")
  type FunSuiteLike = funsuite.AnyFunSuiteLike

  // SKIP-SCALATESTNATIVE-START
  @deprecated("Please use org.scalatest.funsuite.AsyncFunSuiteLike instead")
  type AsyncFunSuiteLike = funsuite.AsyncFunSuiteLike
  // SKIP-SCALATESTNATIVE-END

  @deprecated("Please use org.scalatest.funspec.AnyFeatureSpecLike instead")
  type FeatureSpecLike = featurespec.AnyFeatureSpecLike

  // SKIP-SCALATESTNATIVE-START
  @deprecated("Please use org.scalatest.featurespec.AsyncFeatureSpecLike instead")
  type AsyncFeatureSpecLike = featurespec.AsyncFeatureSpecLike
  // SKIP-SCALATESTNATIVE-END

  @deprecated("Please use org.scalatest.funsuite.AnyFunSpecLike instead")
  type FunSpecLike = funspec.AnyFunSpecLike

  // SKIP-SCALATESTNATIVE-START
  @deprecated("Please use org.scalatest.funspec.AsyncFunSpecLike instead")
  type AsyncFunSpecLike = funspec.AsyncFunSpecLike
  // SKIP-SCALATESTNATIVE-END

  @deprecated("Please use org.scalatest.freespec.AnyFreeSpecLike instead")
  type FreeSpecLike = freespec.AnyFreeSpecLike

  // SKIP-SCALATESTNATIVE-START
  @deprecated("Please use org.scalatest.freespec.AsyncFreeSpecLike instead")
  type AsyncFreeSpecLike = freespec.AsyncFreeSpecLike
  // SKIP-SCALATESTNATIVE-END

  @deprecated("Please use org.scalatest.flatspec.AnyFlatSpecLike instead")
  type FlatSpecLike = flatspec.AnyFlatSpecLike

  // SKIP-SCALATESTNATIVE-START
  @deprecated("Please use org.scalatest.flatspec.AsyncFlatSpecLike instead")
  type AsyncFlatSpecLike = flatspec.AsyncFlatSpecLike
  // SKIP-SCALATESTNATIVE-END

  @deprecated("Please use org.scalatest.propspec.AnyPropSpecLike instead")
  type PropSpecLike = propspec.AnyPropSpecLike

  // SKIP-SCALATESTNATIVE-START
  //@deprecated("Please use org.scalatest.propspec.AsyncPropSpecLike instead")
  //type AsyncPropSpecLike = propspec.AsyncPropSpecLike
  // SKIP-SCALATESTNATIVE-END

  @deprecated("Please use org.scalatest.wordspec.AnyWordSpecLike instead")
  type WordSpecLike = wordspec.AnyWordSpecLike

  // SKIP-SCALATESTNATIVE-START
  @deprecated("Please use org.scalatest.wordspec.AsyncWordSpecLike instead")
  type AsyncWordSpecLike = wordspec.AsyncWordSpecLike
  // SKIP-SCALATESTNATIVE-END
}
