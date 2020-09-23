/*
 * Copyright 2001-2020 Artima, Inc.
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
package org.scalatest.prop


case class RoseTree[T](value: T, shrinker: T => List[RoseTree[T]]) {
  def map[U](f: T => U): RoseTree[U] = RoseTree(f(value), u => shrinker(value).map(rtt => rtt.map(f))) 
  def flatMap[U](f: T => RoseTree[U]): RoseTree[U] = {
    val rtu: RoseTree[U] = f(value)
    val u: U = rtu.value
    val lrtt: List[RoseTree[T]] = shrinker(value)
    RoseTree(u, u => lrtt.map(rtt => f(rtt.value)))
  }
}

object RoseTree {
  def emptyFun[T]: T => List[RoseTree[T]] = o => List.empty
}


