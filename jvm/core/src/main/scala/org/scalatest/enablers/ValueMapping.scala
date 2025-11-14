/*
 * Copyright 2001-2025 Artima, Inc.
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

import scala.collection.JavaConverters._
import org.scalactic.Equality
import org.scalatest.FailureMessages
import scala.annotation.tailrec

/**
 * Supertrait for typeclasses that enable <code>contain value</code> matcher syntax.
 *
 * <p>
 * A <code>ValueMapping[M]</code> provides access to the "value mapping nature" of type <code>M</code> in such
 * a way that <code>contain</code> <code>value</code> matcher syntax can be used with type <code>M</code>. An <code>M</code>
 * can be any type for which <code>contain</code> <code>value</code> syntax makes sense. ScalaTest provides implicit implementations
 * for <code>scala.collection.GenMap</code> and <code>java.util.Map</code>. You can enable the <code>contain</code> <code>value</code>
 * matcher syntax on your own type <code>U</code> by defining a <code>ValueMapping[U]</code> for the type and making it
 * available implicitly.
 *
 * <p>
 * ScalaTest provides implicit <code>ValueMapping</code> instances for <code>scala.collection.GenMap</code>,
 * and <code>java.util.Map</code> in the <a href="ValueMapping$.html"><code>ValueMapping</code> companion object</a>.
 * </p>
 */
trait ValueMapping[-M] {

  /**
   * Implements <code>contain</code> <code>atLeastOneOf</code> syntax for aggregations of type <code>A</code>.
   *
   * @param aggregation an aggregation about which an assertion is being made
   * @param eles elements at least one of which should be contained in the passed aggregation
   * @return true if the passed aggregation contains at least one of the passed elements
   */

  /**
   * Check if the passed <code>map</code> contains the passed <code>value</code>.
   *
   * @param map a map about which an assertion is being made
   * @param value value of which should be contained in the passed map
   * @return true if the passed map contains the passed value
   */
  def containsValue(map: M, value: Any): Boolean
}

/**
 * Companion object for <code>ValueMapping</code> that provides implicit implementations for <code>scala.collection.GenMap</code> and <code>java.util.Map</code>.
 */
object ValueMapping {

  import scala.language.higherKinds

  /**
   * Enable <code>ValueMapping</code> implementation for <code>scala.collection.GenMap</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of value in the <code>scala.collection.GenMap</code>
   * @tparam K the type of the key in the <code>scala.collection.GenMap</code>
   * @tparam V the type of the value in the <code>scala.collection.GenMap</code>
   * @tparam MAP any subtype of <code>scala.collection.GenMap</code>
   * @return <code>ValueMapping[MAP[K, V]]</code> that supports <code>scala.collection.GenMap</code> in <code>contain value</code> syntax
   */
  // SKIP-DOTTY-START
  implicit def valueMappingNatureOfGenMap[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](implicit equality: Equality[V]): ValueMapping[MAP[K, V]] = 
  // SKIP-DOTTY-END
  //DOTTY-ONLY def valueMappingNatureOfGenMap[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](using equality: Equality[V]): ValueMapping[MAP[K, V]] = 
    convertEqualityToGenMapValueMapping(equality)

  // SKIP-DOTTY-START
  import scala.language.implicitConversions
  // SKIP-DOTTY-END

  /**
  // SKIP-DOTTY-START
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>V</code>
  // SKIP-DOTTY-END
  //DOTTY-ONLY  * Converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>V</code>
   * into <code>ValueMapping</code> of type <code>MAP[K, V]</code>, where <code>MAP</code> is a subtype of <code>scala.collection.GenMap</code>.
   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * (Map(1 -> "one") should contain value "ONE") (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
   * and this implicit conversion will convert it into <code>ValueMapping[Map[Int, String]]</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>V</code>
   * @tparam K the type of the key in the <code>scala.collection.GenMap</code>
   * @tparam V the type of the value in the <code>scala.collection.GenMap</code>
   * @tparam MAP any subtype of <code>scala.collection.GenMap</code>
   * @return <code>ValueMapping</code> of type <code>MAP[K, V]</code>
   */
  // SKIP-DOTTY-START
  implicit def convertEqualityToGenMapValueMapping[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](equality: Equality[V]): ValueMapping[MAP[K, V]] = 
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertEqualityToGenMapValueMapping[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](equality: Equality[V]): ValueMapping[MAP[K, V]] = 
    new ValueMapping[MAP[K, V]] {
      def containsValue(map: MAP[K, V], value: Any): Boolean = {
        // map.values.exists((v: V) => equality.areEqual(v, value)) go back to this once I'm off 2.9
        map.iterator.map(_._2).exists((v: V) => equality.areEqual(v, value))
      }
    }

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Given <code>ValueMapping</code> implementation for <code>scala.collection.GenMap</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of value in the <code>scala.collection.GenMap</code>
  //DOTTY-ONLY   * @tparam K the type of the key in the <code>scala.collection.GenMap</code>
  //DOTTY-ONLY   * @tparam V the type of the value in the <code>scala.collection.GenMap</code>
  //DOTTY-ONLY   * @tparam MAP any subtype of <code>scala.collection.GenMap</code>
  //DOTTY-ONLY   * @return <code>ValueMapping[MAP[K, V]]</code> that supports <code>scala.collection.GenMap</code> in <code>contain value</code> syntax
  //DOTTY-ONLY   */
  //DOTTY-ONLY given [K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](using equality: Equality[V]): ValueMapping[MAP[K, V]] =  convertEqualityToGenMapValueMapping(equality)

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Given conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>V</code>
  //DOTTY-ONLY  * into <code>ValueMapping</code> of type <code>MAP[K, V]</code>, where <code>MAP</code> is a subtype of <code>scala.collection.GenMap</code>.
  //DOTTY-ONLY  * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * <pre class="stHighlight">
  //DOTTY-ONLY  * (Map(1 -> "one") should contain value "ONE") (after being lowerCased)
  //DOTTY-ONLY  * </pre>
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
  //DOTTY-ONLY  * and this implicit conversion will convert it into <code>ValueMapping[Map[Int, String]]</code>.
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>V</code>
  //DOTTY-ONLY  * @tparam K the type of the key in the <code>scala.collection.GenMap</code>
  //DOTTY-ONLY  * @tparam V the type of the value in the <code>scala.collection.GenMap</code>
  //DOTTY-ONLY  * @tparam MAP any subtype of <code>scala.collection.GenMap</code>
  //DOTTY-ONLY  * @return <code>ValueMapping</code> of type <code>MAP[K, V]</code>
  //DOTTY-ONLY  */
  //DOTTY-ONLY given equalityGenMapValueMapping[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]]: Conversion[Equality[V], ValueMapping[MAP[K, V]]] with {
  //DOTTY-ONLY   def apply(equality: Equality[V]): ValueMapping[MAP[K, V]] = convertEqualityToGenMapValueMapping(equality)
  //DOTTY-ONLY }  

  /**
   * Enable <code>ValueMapping</code> implementation for <code>java.util.Map</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of value in the <code>java.util.Map</code>
   * @tparam K the type of the key in the <code>java.util.Map</code>
   * @tparam V the type of the value in the <code>java.util.Map</code>
   * @tparam JMAP any subtype of <code>java.util.Map</code>
   * @return <code>ValueMapping[JMAP[K, V]]</code> that supports <code>java.util.Map</code> in <code>contain</code> <code>value</code> syntax
   */
  // SKIP-DOTTY-START
  implicit def valueMappingNatureOfJavaMap[K, V, JMAP[k, v] <: java.util.Map[k, v]](implicit equality: Equality[V]): ValueMapping[JMAP[K, V]] = 
  // SKIP-DOTTY-END
  //DOTTY-ONLY def valueMappingNatureOfJavaMap[K, V, JMAP[k, v] <: java.util.Map[k, v]](using equality: Equality[V]): ValueMapping[JMAP[K, V]] = 
    convertEqualityToJavaMapValueMapping(equality)

  /**
  // SKIP-DOTTY-START
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>V</code>
  // SKIP-DOTTY-END
  //DOTTY-ONLY * Converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>V</code>
   * into <code>ValueMapping</code> of type <code>JMAP[K, V]</code>, where <code>JMAP</code> is a subtype of <code>java.util.Map</code>.
   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * val javaMap = new java.util.HashMap[Int, String]()
   * javaMap.put(1, "one")
   * (javaMap should contain value "ONE") (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
   * and this implicit conversion will convert it into <code>ValueMapping[java.util.HashMap[Int, String]]</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>V</code>
   * @tparam K the type of the key in the <code>java.util.Map</code>
   * @tparam V the type of the value in the <code>java.util.Map</code>
   * @tparam JMAP any subtype of <code>java.util.Map</code>
   * @return <code>ValueMapping</code> of type <code>JMAP[K, V]</code>
   */
  // SKIP-DOTTY-START
  implicit def convertEqualityToJavaMapValueMapping[K, V, JMAP[k, v] <: java.util.Map[k, v]](equality: Equality[V]): ValueMapping[JMAP[K, V]] = 
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertEqualityToJavaMapValueMapping[K, V, JMAP[k, v] <: java.util.Map[k, v]](equality: Equality[V]): ValueMapping[JMAP[K, V]] = 
    new ValueMapping[JMAP[K, V]] {
      def containsValue(jMap: JMAP[K, V], value: Any): Boolean = {
        jMap.asScala.values.exists((v: V) => equality.areEqual(v, value))
      }
    }

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Given <code>ValueMapping</code> implementation for <code>java.util.Map</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of value in the <code>java.util.Map</code>
  //DOTTY-ONLY   * @tparam K the type of the key in the <code>java.util.Map</code>
  //DOTTY-ONLY   * @tparam V the type of the value in the <code>java.util.Map</code>
  //DOTTY-ONLY   * @tparam JMAP any subtype of <code>java.util.Map</code>
  //DOTTY-ONLY   * @return <code>ValueMapping[JMAP[K, V]]</code> that supports <code>java.util.Map</code> in <code>contain</code> <code>value</code> syntax
  //DOTTY-ONLY   */
  //DOTTY-ONLY given [K, V, JMAP[k, v] <: java.util.Map[k, v]](using equality: Equality[V]): ValueMapping[JMAP[K, V]] =  convertEqualityToJavaMapValueMapping(equality)

  //DOTTY-ONLY /**
  //DOTTY-ONLY  * Given conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>V</code>
  //DOTTY-ONLY  * into <code>ValueMapping</code> of type <code>JMAP[K, V]</code>, where <code>JMAP</code> is a subtype of <code>java.util.Map</code>.
  //DOTTY-ONLY  * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * <pre class="stHighlight">
  //DOTTY-ONLY  * val javaMap = new java.util.HashMap[Int, String]()
  //DOTTY-ONLY  * javaMap.put(1, "one")
  //DOTTY-ONLY  * (javaMap should contain value "ONE") (after being lowerCased)
  //DOTTY-ONLY  * </pre>
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
  //DOTTY-ONLY  * and this implicit conversion will convert it into <code>ValueMapping[java.util.HashMap[Int, String]]</code>.
  //DOTTY-ONLY  *
  //DOTTY-ONLY  * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>V</code>
  //DOTTY-ONLY  * @tparam K the type of the key in the <code>java.util.Map</code>
  //DOTTY-ONLY  * @tparam V the type of the value in the <code>java.util.Map</code>
  //DOTTY-ONLY  * @tparam JMAP any subtype of <code>java.util.Map</code>
  //DOTTY-ONLY  * @return <code>ValueMapping</code> of type <code>JMAP[K, V]</code>
  //DOTTY-ONLY  */
  //DOTTY-ONLY given equalityJavaMapValueMapping[K, V, JMAP[k, v] <: java.util.Map[k, v]]: Conversion[Equality[V], ValueMapping[JMAP[K, V]]] with {
  //DOTTY-ONLY   def apply(equality: Equality[V]): ValueMapping[JMAP[K, V]] = convertEqualityToJavaMapValueMapping(equality)
  //DOTTY-ONLY }  
}
