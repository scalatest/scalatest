/*
 * Copyright 2001-2008 Artima, Inc.
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
 * Subtrait of <code>Reporter</code> that contains a <code>dispose</code> method for
 * releasing any finite, non-memory resources, such as file handles, held by the
 * <code>Reporter</code>. <code>Runner</code> will invoke <code>dispose</code> on
 * any <code>ResourcefulReporter</code> when it no longer needs the <code>Reporter</code>.
 */
trait ResourcefulReporter extends Reporter {

  /**
   * Release any finite, non-memory resources, such as file handles, held by this
   * <code>Reporter</code>. Clients should call this method when they no longer need
   * the <code>Reporter</code>, before releasing the last reference to the <code>Reporter</code>.
   * After this method is invoked, the <code>Reporter</code> may be defunct, and therefore not
   * usable anymore. If the <code>Reporter</code> holds no resources, it may do nothing when
   * this method is invoked, however, in that case, it probably won't implement <code>ResourcefulReporter</code>.
   */
  def dispose()
}
