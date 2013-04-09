/*
 * Copyright 2001-2011 Artima, Inc.
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
 * Package object to hold deprecated type aliases for exception classes moved from org.scalatest.prop to org.scalatest.exceptions.
 */
package object prop {

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest.exceptions</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.prop.DiscardedEvaluationException</code> to <code>org.scalatest.exceptions.DiscardedEvaluationException</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.exceptions.TestRegistrationClosedException instead.")
  type DiscardedEvaluationException = exceptions.DiscardedEvaluationException

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest.exceptions</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.prop.GeneratorDrivenPropertyCheckFailedException</code> to <code>org.scalatest.exceptions.GeneratorDrivenPropertyCheckFailedException</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.exceptions.TestRegistrationClosedException instead.")
  type GeneratorDrivenPropertyCheckFailedException = exceptions.GeneratorDrivenPropertyCheckFailedException

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest.exceptions</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.prop.PropertyCheckFailedException</code> to <code>org.scalatest.exceptions.PropertyCheckFailedException</code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.exceptions.TestRegistrationClosedException instead.")
  type PropertyCheckFailedException = exceptions.PropertyCheckFailedException

  /**
   * <p>
   * <strong>This exception class has been deprecated and will be removed in a future version of ScalaTest. Please
   * change uses of this class to use <code>org.scalatest.exceptions.GeneratorDrivenPropertyCheckFailedException</code> instead.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.exceptions.TestRegistrationClosedException instead.")
  type PropertyTestFailedException = exceptions.GeneratorDrivenPropertyCheckFailedException

  /**
   * <p>
   * <strong>This class has been moved to the <code>org.scalatest.exceptions</code> package. The deprecated type alias that has been left in its place will
   * be removed in a future version of ScalaTest. Please change any uses of <code>org.scalatest.prop.TableDrivenPropertyCheckFailedException </code> to <code>org.scalatest.exceptions.TableDrivenPropertyCheckFailedException </code>.</strong>
   * </p>
   */
  @deprecated("Please use org.scalatest.exceptions.TestRegistrationClosedException instead.")
  type TableDrivenPropertyCheckFailedException  = exceptions.TableDrivenPropertyCheckFailedException 
}



