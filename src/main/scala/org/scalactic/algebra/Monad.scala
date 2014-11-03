/*
 * Copyright 2001-2014 Artima, Inc.
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
package org.scalactic.algebra

import scala.language.higherKinds

trait MonadProxy[TC[_], T] {
  def map[U](f: T => U): TC[U]
  def flatMap[U](f: T => Option[U]): Option[U]  
}

trait Monad[TC[_]] {
  def apply[T](f: TC[T]): MonadProxy[TC, T]
  def insert[T](o: T): TC[T]
}
