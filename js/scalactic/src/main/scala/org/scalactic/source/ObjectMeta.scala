  /*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalactic.source

trait ObjectMeta {

  def fieldNames: scala.collection.immutable.IndexedSeq[String]

  def hasField(name: String): Boolean = fieldNames.contains(name)

  def value(name: String): Any

  def typeName(name: String): String

  def shortTypeName(name: String): String

}

object ObjectMeta {

  def apply(v: Any): ObjectMeta = {

    def filterDollarNumberAtTheEnd(value: String): String = {
      val lastIdxOfDollar = value.lastIndexOf('$')
      if (lastIdxOfDollar > 0) {
        val afterDollar = value.substring(lastIdxOfDollar + 1)
        try {
          afterDollar.toInt
          value.substring(0, lastIdxOfDollar)
        }
        catch {
          case t: Throwable => value
        }
      }
      else
        value
    }

    val dict: Map[String, Any] = v.asInstanceOf[scala.scalajs.js.Dictionary[Any]].map { case (key, value) =>
      val decodedKey =
        scala.reflect.NameTransformer.decode(key).
          replaceAllLiterally("$$und", "_").
          replaceAllLiterally("$mcI", "").
          replaceAllLiterally("$sp", "").
          replaceAllLiterally("$f", "") 

      val decodedKey10 = {
        val idx = decodedKey.indexOf("__f_")
        if (idx >= 0)
          decodedKey.drop(idx + 4)
        else
          decodedKey
      }    

      Some((filterDollarNumberAtTheEnd(decodedKey10), value))
    }.toList.flatten.toMap

    new ObjectMeta {

      lazy val fieldNames = dict.keys.toVector.filter(k => k != "$$outer" && k != "$outer")

      def value(name: String): Any = {
        dict.get(name) match {
          case Some(value) => value
          case None => throw new IllegalArgumentException("'" + name + "' is not attribute for this instance.")
        }
      }

      def typeName(name: String): String = {
        dict.get(name) match {
          case Some(value) => value.getClass.getName
          case None => throw new IllegalArgumentException("'" + name + "' is not attribute for this instance.")
        }
      }

      def shortTypeName(name: String): String = {
        dict.get(name) match {
          case Some(value) => value.getClass.getSimpleName
          case None => throw new IllegalArgumentException("'" + name + "' is not attribute for this instance.")
        }
      }
    }
  }

}