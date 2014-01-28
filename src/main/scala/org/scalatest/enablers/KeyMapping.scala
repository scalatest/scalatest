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
package org.scalatest.enablers

import org.scalautils.Equality
import org.scalatest.words.ArrayWrapper
import scala.collection.GenTraversable
import org.scalatest.FailureMessages
import scala.annotation.tailrec
import scala.collection.JavaConverters._

/**
 * Supertrait for typeclasses that enable <code>contain key</code> matcher syntax.
 *
 * <p>
 * A <code>KeyMapping[M]</code> provides access to the "key mapping nature" of type <code>M</code> in such
 * a way that <code>contain key</code> matcher syntax can be used with type <code>M</code>. A <code>M</code>
 * can be any type for which <code>contain key</code> syntax makes sense. ScalaTest provides implicit implementations
 * for <code>scala.collection.GenMap</code> and <code>java.util.Map</code>. You can enable the <code>contain key</code>
 * matcher syntax on your own type <code>U</code> by defining a <code>KeyMapping[U]</code> for the type and making it
 * available implicitly.
 * 
 * <p>
 * ScalaTest provides implicit <code>KeyMapping</code> instances for <code>scala.collection.GenMap</code>,
 * and <code>java.util.Map</code> in the <code>KeyMapping</code> companion object.
 * </p>
 */
trait KeyMapping[-M] {

  /**
   * Check if the passed <code>map</code> contains the passed <code>key</code>.
   *
   * @param map a map about which an assertion is being made
   * @param key key of which should be contained in the passed map
   * @return true if the passed map contains the passed key
   */
  def containsKey(map: M, key: Any): Boolean
}

/**
 * Companion object for <code>KeyMapping</code> that provides implicit implementations for <code>scala.collection.GenMap</code> and <code>java.util.Map</code>.
 */
object KeyMapping {

  import scala.language.higherKinds

  /**
   * Enable <code>KeyMapping</code> implementation for <code>scala.collection.GenMap</code>.
   *
   * @param equality <a href="../../scalautils/Equality.html"><code>Equality</code></a> type class that is used to check equality of key in the <code>scala.collection.GenMap</code>
   * @tparam K the type of the key in the <code>scala.collection.GenMap</code>
   * @tparam V the type of the value in the <code>scala.collection.GenMap</code>
   * @tparam MAP any subtype of <code>scala.collection.GenMap</code>
   * @return <code>KeyMapping[MAP[K, V]]</code> that supports <code>scala.collection.GenMap</code> in <code>contain key</code> syntax
   */
  implicit def keyMappingNatureOfGenMap[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](implicit equality: Equality[K]): KeyMapping[MAP[K, V]] = 
    new KeyMapping[MAP[K, V]] {
      def containsKey(map: MAP[K, V], key: Any): Boolean = {
        // map.keySet.exists((k: K) => equality.areEqual(k, key)) go back to this once I'm off 2.9
        map.iterator.map(_._1).exists((k: K) => equality.areEqual(k, key))
      }
    }

  import scala.language.implicitConversions

  /**
   * Implicit conversion that converts an <a href="../../scalautils/Equality.html"><code>Equality</code></a> of type <code>K</code>
   * into <code>KeyMapping</code> of type <code>MAP[K, V]</code>, where <code>MAP</code> is a subtype of <code>scala.collection.GenMap</code>.
   * This is required to support the explicit <a href="../../scalautils/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * (Map("one" -> 1) should contain key "ONE") (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalautils/Equality.html"><code>Equality[String]</code></a>
   * and this implicit conversion will convert it into <code>KeyMapping[Map[String, Int]]</code>.
   *
   * @param equality <a href="../../scalautils/Equality.html"><code>Equality</code></a> of type <code>K</code>
   * @tparam K the type of the key in the <code>scala.collection.GenMap</code>
   * @tparam V the type of the value in the <code>scala.collection.GenMap</code>
   * @tparam MAP any subtype of <code>scala.collection.GenMap</code>
   * @return <code>KeyMapping</code> of type <code>MAP[K, V]</code>
   */
  implicit def convertEqualityToGenMapKeyMapping[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](equality: Equality[K]): KeyMapping[MAP[K, V]] = 
    keyMappingNatureOfGenMap(equality)

  /**
   * Enable <code>KeyMapping</code> implementation for <code>java.util.Map</code>.
   *
   * @param equality <a href="../../scalautils/Equality.html"><code>Equality</code></a> type class that is used to check equality of key in the <code>java.util.Map</code>
   * @tparam K the type of the key in the <code>java.util.Map</code>
   * @tparam V the type of the value in the <code>java.util.Map</code>
   * @tparam JMAP any subtype of <code>java.util.Map</code>
   * @return <code>KeyMapping[JMAP[K, V]]</code> that supports <code>java.util.Map</code> in <code>contain</code> <code>key</code> syntax
   */
  implicit def keyMappingNatureOfJavaMap[K, V, JMAP[k, v] <: java.util.Map[k, v]](implicit equality: Equality[K]): KeyMapping[JMAP[K, V]] = 
    new KeyMapping[JMAP[K, V]] {
      def containsKey(jMap: JMAP[K, V], key: Any): Boolean = {
        jMap.asScala.keySet.exists((k: K) => equality.areEqual(k, key))
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalautils/Equality.html"><code>Equality</code></a> of type <code>K</code>
   * into <code>KeyMapping</code> of type <code>JMAP[K, V]</code>, where <code>JMAP</code> is a subtype of <code>java.util.Map</code>.
   * This is required to support the explicit <a href="../../scalautils/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * val javaMap = new java.util.HashMap[String, Int]()
   * javaMap.put("one", 1)
   * (javaMap should contain key "ONE") (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalautils/Equality.html"><code>Equality[String]</code></a>
   * and this implicit conversion will convert it into <code>KeyMapping[java.util.HashMap[String, Int]]</code>.
   *
   * @param equality <a href="../../scalautils/Equality.html"><code>Equality</code></a> of type <code>K</code>
   * @tparam K the type of the key in the <code>java.util.Map</code>
   * @tparam V the type of the value in the <code>java.util.Map</code>
   * @tparam JMAP any subtype of <code>java.util.Map</code>
   * @return <code>KeyMapping</code> of type <code>JMAP[K, V]</code>
   */
  implicit def convertEqualityToJavaMapKeyMapping[K, V, JMAP[k, v] <: java.util.Map[k, v]](equality: Equality[K]): KeyMapping[JMAP[K, V]] = 
    keyMappingNatureOfJavaMap(equality)
}
