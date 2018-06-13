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
package org.scalactic


private[scalactic] object ArrayHelper {

  // The following code is taken from: https://github.com/scala/scala/blob/86e75db7f36bcafdd75302f2c2cca0c68413214d/src/partest/scala/tools/partest/Util.scala

  /*def prettyArray(a: Array[_]): collection.IndexedSeq[Any] = new collection.AbstractSeq[Any] with collection.IndexedSeq[Any] {
    def length = a.length

    def apply(idx: Int): Any = a(idx) match {
      case x: AnyRef if x.getClass.isArray => prettyArray(x.asInstanceOf[Array[_]])
      case x => x
    }

    override def stringPrefix = "Array"
  }*/

  def deep[T](a: Array[T]): collection.IndexedSeq[Any] = a.deep//prettyArray(a)

}