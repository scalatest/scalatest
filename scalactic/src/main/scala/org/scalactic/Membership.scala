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

final class Membership[-A](predicate: A => Boolean) {
  def apply(ele: A): Boolean = predicate(ele)
  def complement: Membership[A] = {
    new Membership((ele: A) => !apply(ele))
  }
  def diff[B <: A](other: Membership[B]): Membership[B] = {
    new Membership((ele: B) => !(apply(ele) && other.apply(ele)))
  }
  def intersect[B <: A](other: Membership[B]): Membership[B] = {
    new Membership((ele: B) => apply(ele) && other.apply(ele))
  }
  def union[B <: A](other: Membership[B]): Membership[B] = {
    new Membership((ele: B) => apply(ele) || other.apply(ele))
  }
  override def toString = "<membership>"
}

object Membership {
  def apply[A](predicate: A => Boolean): Membership[A] = new Membership(predicate)
}

