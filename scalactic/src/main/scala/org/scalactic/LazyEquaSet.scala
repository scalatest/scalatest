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
  def flatMap[U](f: T => LazyEquaSet[U]): LazyEquaSet[U]
  def toStrict(toPath: EquaPath[T]): toPath.EquaSet
  def toList: List[T]
}

object LazyEquaSet {
  def apply[T](args: T*): LazyEquaSet[T] = new BagLazyEquaSet(args.toList)
}

trait LazyFastEquaSet[T] extends LazyEquaSet[T] {
  def map[U](f: T => U): LazyFastEquaSet[U]
  def flatMap[U](f: T => LazyEquaSet[U]): LazyEquaSet[U]
  def toStrict(toPath: EquaPath[T]): toPath.FastEquaSet
}

class ConcreteLazyFastEquaSet[T](elements: List[T]) extends LazyEquaSet[T] { thisLazyEquaSet =>
  def map[U](f: T => U): LazyFastEquaSet[U] = new MappedLazyFastEquaSet[T, U](thisLazyEquaSet, f)

  def flatMap[U](f: T => LazyEquaSet[U]): LazyEquaSet[U] = new FlatMappedLazyFastEquaSet(thisLazyEquaSet, f)

  def toStrict(toPath: EquaPath[T]): toPath.FastEquaSet = toPath.FastEquaSet(elements: _*)
  def toList: List[T] = elements
}

// This is not lazy?
class BagLazyEquaSet[T](private val args: List[T]) extends LazyEquaSet[T] {
  def map[U](f: T => U): BagLazyEquaSet[U] = new BagLazyEquaSet[U](args.map(f))
  def flatMap[U](f: T => LazyEquaSet[U]): LazyEquaSet[U] = ???
  def toStrict(toPath: EquaPath[T]): toPath.FastEquaSet = toPath.FastEquaSet(args: _*)
  def toList: List[T] = args
}

class MappedLazyFastEquaSet[T, U](lazyEquaSet: LazyEquaSet[T], f: T => U) extends LazyFastEquaSet[U] { thisLazyEquaSet => 
  def map[V](g: U => V): LazyFastEquaSet[V] = new MappedLazyFastEquaSet[T, V](lazyEquaSet, f andThen g)
  def flatMap[V](f: U => LazyEquaSet[V]): LazyEquaSet[V] = ???
  def toStrict(toPath: EquaPath[U]): toPath.FastEquaSet = {
    toPath.FastEquaSet(toList: _*)
  }
  def toList: List[U] = lazyEquaSet.toList.map(f)
}

class FlatMappedLazyFastEquaSet[T, U](lazyEquaSet: LazyEquaSet[T], f: T => LazyEquaSet[U]) extends LazyFastEquaSet[U] { thisLazyEquaSet => 
  def map[V](g: U => V): LazyFastEquaSet[V] = new MappedLazyFastEquaSet[U, V](thisLazyEquaSet, g)
  def flatMap[V](f: U => LazyEquaSet[V]): LazyEquaSet[V] = ???
  def toStrict(toPath: EquaPath[U]): toPath.FastEquaSet = {
    toPath.FastEquaSet(toList: _*)
  }
  def toList: List[U] = lazyEquaSet.toList.flatMap(f.andThen(_.toList))
}

