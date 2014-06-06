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

  /**
   * Convenience type alias allowing <code>ShouldMatchers</code> to be used without qualification or another import
   * after a wildcard import of <code>org.scalatest</code>.
   */
  type ShouldMatchers = matchers.ShouldMatchers

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest.exceptions</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.DuplicateTestNameException</code> to <code>org.scalatest.exceptions.DuplicateTestNameException</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.exceptions.DuplicateTestNameException instead.")
  type DuplicateTestNameException = exceptions.DuplicateTestNameException

  /**
   * <p>
   * <strong>This trait has been moved to the <code>org.scalatest.exceptions</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.ModifiableMessageXXX</code> to <code>org.scalatest.exceptions.ModifiableMessageXXX</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.exceptions.ModifiableMessageXXX instead.")
  type ModifiableMessage[T <: Throwable] = exceptions.ModifiableMessage[T]

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest.exceptions</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.NotAllowedException</code> to <code>org.scalatest.exceptions.NotAllowedException</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.exceptions.NotAllowedException instead.")
  type NotAllowedException = exceptions.NotAllowedException

  /**
   * <p>
   * <strong>This trait has been moved to the <code>org.scalatest.exceptions</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.StackDepth</code> to <code>org.scalatest.exceptions.StackDepth</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.exceptions.StackDepth instead.")
  type StackDepth = exceptions.StackDepth

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest.exceptions</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.StackDepthException</code> to <code>org.scalatest.exceptions.StackDepthException</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.exceptions.StackDepthException instead.")
  type StackDepthException = exceptions.StackDepthException

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest.exceptions</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.TestFailedException</code> to <code>org.scalatest.exceptions.TestFailedException</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.exceptions.TestFailedException instead.")
  type TestFailedException = exceptions.TestFailedException

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest.exceptions</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.TestPendingException</code> to <code>org.scalatest.exceptions.TestPendingException</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.exceptions.TestPendingException instead.")
  type TestPendingException = exceptions.TestPendingException

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest.exceptions</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.TestRegistrationClosedException</code> to <code>org.scalatest.exceptions.TestRegistrationClosedException</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.exceptions.TestRegistrationClosedException instead.")
  type TestRegistrationClosedException = exceptions.TestRegistrationClosedException

  @deprecated("Please use org.scalactic.TimesOnInt instead")
  type TimesOnInt = org.scalactic.TimesOnInt

  /**
   * The version number of ScalaTest.
   *
   * @return the ScalaTest version number.
   */
  val ScalaTestVersion: String = ScalaTestVersions.ScalaTestVersion
}
