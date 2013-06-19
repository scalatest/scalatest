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


case class Entry[K, V](key: K, value: V) extends java.util.Map.Entry[K, V] {

  def getKey: K = key

  def getValue: V = value

  def setValue(v: V): V = throw new UnsupportedOperationException

  override def equals(other: Any): Boolean = {
    other match {
      case that: java.util.Map.Entry[_, _] => 
         (if (key == null) that.getKey == null else key == that.getKey) &&
         (if (value == null) that.getValue == null else value == that.getValue)
      case _ => false
    }
  }

  override def hashCode: Int = {
    (if (key == null) 0 else key.hashCode) ^
    (if (value == null) 0 else value.hashCode)
  }

  override def toString: String = key.toString + "=" + value.toString
}
