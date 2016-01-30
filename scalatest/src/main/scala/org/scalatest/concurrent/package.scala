/*
 * Copyright 2001-2016 Artima, Inc.
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
 * ScalaTest's main traits, classes, and other members, including members supporting ScalaTest's DSL for the Scala interpreter.
 */
package object concurrent {

  /**
   * <strong>The name <code>org.scalatest.concurrent.AsyncAssertions</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.concurrent.Waiters</code>, instead.</strong>
   *
   * <p>
   * We deprecate this name because this is blocking while our new asynchronous style traits are not, thus could potentially cause some confusion.
   * Another reason is that this name sounds like it is a subclass of Assertions, while it is not.
   * </p>
   */
  @deprecated("Please use org.scalatest.concurrent.Waiters instead")
  type AsyncAssertions = Waiters

  /**
   * <strong>The name <code>org.scalatest.concurrent.AsyncAssertions</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.concurrent.Waiters</code>, instead.</strong>
   *
   * <p>
   * We deprecate this name because this is blocking while our new asynchronous style traits are not, thus could potentially cause some confusion.
   * Another reason is that this name sounds like it is a subclass of Assertions, while it is not.
   * </p>
   */
  @deprecated("Please use org.scalatest.concurrent.Waiters instead")
  val AsyncAssertions = Waiters

}