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
package org.scalatest

/**
 * ScalaTest's old path package that contains deprecated alias for path style traits.
 */
package object path {

  /**
   * Forces Scaladoc to include the <code>org.scalatest.path</code> package.
   */
  @deprecated("This class is a workaround for a Scaladoc bug. It will be removed in a future version of ScalaTest.")
  final class ForceScaladocGeneration private ()

  /**
   * <strong>The name <code>org.scalatest.path.FreeSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.freespec.PathAnyFreeSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.path.FreeSpecLike trait has been moved and renamed. Please use org.scalatest.freespec.PathAnyFreeSpecLike instead.", "3.1.0")
  type FreeSpecLike = org.scalatest.freespec.PathAnyFreeSpecLike

  /**
   * <strong>The name <code>org.scalatest.path.FunSpecLike</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funspec.PathAnyFunSpecLike</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.path.FunSpecLike trait has been moved and renamed. Please use org.scalatest.funspec.PathAnyFunSpecLike instead.", "3.1.0")
  type FunSpecLike = org.scalatest.funspec.PathAnyFunSpecLike

  /**
   * <strong>The name <code>org.scalatest.path.FreeSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.freespec.PathAnyFreeSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.path.FreeSpec class has been moved and renamed. Please use org.scalatest.freespec.PathAnyFreeSpec instead.", "3.1.0")
  type FreeSpec = org.scalatest.freespec.PathAnyFreeSpec

  /**
   * <strong>The name <code>org.scalatest.path.FunSpec</code> has been deprecated and will be removed in a future version of ScalaTest. Please use
   * its new name, <code>org.scalatest.funspec.PathAnyFunSpec</code>, instead.</strong>
   *
   * <p>This type has been renamed to support future modularization of ScalaTest.</p>
   */
  @deprecated("The org.scalatest.path.FunSpec class has been moved and renamed. Please use org.scalatest.funspec.PathAnyFunSpec instead.", "3.1.0")
  type FunSpec = org.scalatest.funspec.PathAnyFunSpec
}
