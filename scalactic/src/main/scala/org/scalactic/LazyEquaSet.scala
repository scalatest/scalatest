/*
 * Copyright 2001-2015 Artima, Inc.
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
package org.scalactic

trait LazyEquaSet[T] {
  def map[U](f: T => U): LazyEquaSet[U]
  def toStrict(toPath: EquaPath[T]): toPath.EquaSet
}

trait LazyFastEquaSet[T] extends LazyEquaSet[T] {
  def map[U](f: T => U): LazyFastEquaSet[U]
  def toStrict(toPath: EquaPath[T]): toPath.FastEquaSet
}
class ConcreteLazyFastEquaSet[T](equaSet: EquaPath[T]#EquaSet) extends LazyEquaSet[T] {
  def map[U](f: T => U): LazyFastEquaSet[U] = new MappedLazyFastEquaSet[T, U](equaSet, f)
  def toStrict(toPath: EquaPath[T]): toPath.FastEquaSet = toPath.FastEquaSet(equaSet.toList: _*)
}
class MappedLazyFastEquaSet[T, U](equaSet: EquaPath[T]#EquaSet, f: T => U) extends LazyFastEquaSet[U] {
  def map[V](g: U => V): LazyFastEquaSet[V] = new MappedLazyFastEquaSet[T, V](equaSet, f andThen g)
  def toStrict(toPath: EquaPath[U]): toPath.FastEquaSet = {
    val eles = equaSet.toList.map(f)
    toPath.FastEquaSet(eles: _*)
  }
}

