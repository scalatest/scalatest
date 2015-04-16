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

trait LazyBag[T] {
  def map[U](f: T => U): LazyBag[U]
  def flatMap[U](f: T => LazyBag[U]): LazyBag[U]
  def toEquaSet(toPath: EquaPath[T]): toPath.EquaSet
  def toList: List[T]
}

object LazyBag {
  def apply[T](args: T*): LazyBag[T] = new BasicLazyBag(args.toList)
}

class BasicLazyBag[T](private val args: List[T]) extends LazyBag[T] { thisLazyBag =>
  def map[U](f: T => U): BasicLazyBag[U] = new BasicLazyBag[U](args.map(f))
  def flatMap[U](f: T => LazyBag[U]): LazyBag[U] = new FlatMappedLazyBag(thisLazyBag, f)
  def toEquaSet(toPath: EquaPath[T]): toPath.FastEquaSet = toPath.FastEquaSet(args: _*)
  def toList: List[T] = args
}

class MappedLazyBag[T, U](lazyBag: LazyBag[T], f: T => U) extends LazyBag[U] { thisLazyBag => 
  def map[V](g: U => V): LazyBag[V] = new MappedLazyBag[T, V](lazyBag, f andThen g)
  def flatMap[V](f: U => LazyBag[V]): LazyBag[V] = ???
  def toEquaSet(toPath: EquaPath[U]): toPath.FastEquaSet = {
    toPath.FastEquaSet(toList: _*)
  }
  def toList: List[U] = lazyBag.toList.map(f)
}

class FlatMappedLazyBag[T, U](lazyBag: LazyBag[T], f: T => LazyBag[U]) extends LazyBag[U] { thisLazyBag => 
  def map[V](g: U => V): LazyBag[V] = new MappedLazyBag[U, V](thisLazyBag, g)
  def flatMap[V](f: U => LazyBag[V]): LazyBag[V] = ???
  def toEquaSet(toPath: EquaPath[U]): toPath.FastEquaSet = {
    toPath.FastEquaSet(toList: _*)
  }
  def toList: List[U] = lazyBag.toList.flatMap(f.andThen(_.toList))
}

