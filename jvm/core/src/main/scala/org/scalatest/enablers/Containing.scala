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

import org.scalactic.{Equality, NormalizingEquality, Every}

import org.scalactic.ColCompatHelper.{Iterable, IterableOnce}
import org.scalactic.ColCompatHelper.aggregate

/**
 * Supertrait for typeclasses that enable certain <code>contain</code> matcher syntax for containers.
 *
 * <p>
 * A <code>Containing[C]</code> provides access to the "containing nature" of type <code>C</code> in such
 * a way that relevant <code>contain</code> matcher syntax can be used with type <code>C</code>. A <code>C</code>
 * can be any type of "container," a type that in some way can contains one or more other objects. ScalaTest provides
 * implicit implementations for several types. You can enable the <code>contain</code> matcher syntax on your own
 * type <code>U</code> by defining an <code>Containing[U]</code> for the type and making it available implicitly.
 * 
 * <p>
 * ScalaTest provides implicit <code>Containing</code> instances for <code>org.scalactic.ColCompatHelper.Iterable</code>,
 * <code>java.util.Collection</code>, <code>java.util.Map</code>, <code>String</code>, <code>Array</code>, 
 * and <code>scala.Option</code> in the <code>Containing</code> companion object.
 * </p>
 *
 * <a name="containingVersusAggregating"></a>
 * <h2><code>Containing</code> versus <code>Aggregating</code></h2>
 * 
 * <p>
 * The difference between <code>Containing</code> and <a href="Aggregating.html"><code>Aggregating</code></a> is that
 * <code>Containing</code> enables <code>contain</code> matcher syntax that makes sense for "box" types that can
 * contain at most one value (for example, <code>scala.Option</code>),
 * whereas <code>Aggregating</code> enables <code>contain</code> matcher syntax for full-blown collections and other 
 * aggregations of potentially more than one object. For example, it makes sense to make assertions like these, which 
 * are enabled by <code>Containing</code>, for <code>scala.Option</code>:
 * </p>
 * 
 * <pre class="stHighlight">
 * val option: Option[Int] = Some(7)
 * option should contain (7)
 * option should contain oneOf (6, 7, 8)
 * option should contain noneOf (3, 4, 5)
 * </pre>
 *
 * <p>
 * However, given a <code>scala.Option</code> can only ever contain at most one object, it doesn't make
 * sense to make assertions like the following, which are enabled via <code>Aggregation</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * // Could never succeed, so does not compile
 * option should contain allOf (6, 7, 8)
 * </pre>
 * 
 * <p>
 * The above assertion could never succceed, because an option cannot contain more than
 * one value. By default the above statement does not compile, because <code>contain</code> <code>allOf</code>
 * is enabled by <code>Aggregating</code>, and ScalaTest provides no implicit <code>Aggregating</code> instance
 * for type <code>scala.Option</code>.
 * </p>
 */
trait Containing[-C] {

  /**
   * Implements <code>contain</code> <code>&lt;value&gt;</code> syntax for containers of type <code>C</code>.
   *
   * @param container a container about which an assertion is being made
   * @param element an element that should be contained in the passed container
   * @return true if the passed container contains the passed element
   */
  def contains(container: C, element: Any): Boolean

  /**
   * Implements <code>contain</code> <code>oneOf</code> syntax for containers of type <code>C</code>.
   *
   * @param container a container about which an assertion is being made
   * @param elements elements exactly one (<em>i.e.</em>, one and only one) of which should be contained in the passed container
   * @return true if the passed container contains exactly one of the passed elements
   */
  def containsOneOf(container: C, elements: scala.collection.Seq[Any]): Boolean

  /**
   * Implements <code>contain</code> <code>noneOf</code> syntax for containers of type <code>C</code>.
   *
   * @param container a container about which an assertion is being made
   * @param elements elements none of which should be contained in the passed container
   * @return true if the passed container contains none of the passed elements
   */
  def containsNoneOf(container: C, elements: scala.collection.Seq[Any]): Boolean
}

/*
  @tailrec
  def containsOneOf[T](left: T, rightItr: Iterator[Any])(implicit holder: Containing[T]): Boolean = {
    if (rightItr.hasNext) {
      val nextRight = rightItr.next
      if (holder.contains(left, nextRight)) // Found one of right in left, can succeed early
        true
      else
        containsOneOf(left, rightItr)
    }
    else // No more elements in right, left does not contain one of right.
      false
  }
*/

trait ContainingImpls {

  private def tryEquality[T](left: Any, right: Any, equality: Equality[T]): Boolean =
    try equality.areEqual(left.asInstanceOf[T], right)
    catch {
      case cce: ClassCastException => false
    }

  private[scalatest] def checkOneOf[T](left: IterableOnce[T], right: Iterable[Any], equality: Equality[T]): Set[Any] = {
    // aggregate version is more verbose, but it allows parallel execution.
    aggregate(right, Set.empty[Any])(
      { case (fs, r) =>
        if (left.toIterable.exists(t => equality.areEqual(t, r))) {
          // r is in the left
          if (fs.size != 0) // This .size should be safe, it won't go > 1
            return fs + r // fail early by returning early, hmm..  not so 'functional'??
          else
            fs + r
        }
        else
          fs // r is not in the left
      },
      { case (fs1, fs2) =>
        val fs = fs1 + fs2
        if (fs.size > 1)
          return fs // fail early by returning early
        else
          fs
      }
    )
  }

  private[scalatest] def checkNoneOf[T](left: IterableOnce[T], right: Iterable[Any], equality: Equality[T]): Option[Any] = {
    aggregate(right, None)(
      { case (f, r) =>
        if (left.toIterable.exists(t => equality.areEqual(t, r)))
          return Some(r) // r is in the left, fail early by returning.
        else
          None // r is not in the left
      },
      { case (f1, f2) => None }
    )
  }

}

trait JavaContainingImplicits extends ContainingImpls {

  /**
    // SKIP-DOTTY-START
    * Implicit to support <code>Containing</code> nature of <code>java.util.Collection</code>.
    // SKIP-DOTTY-END
    //DOTTY-ONLY   * To support <code>Containing</code> nature of <code>java.util.Collection</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>java.util.Collection</code>
    * @tparam E the type of the element in the <code>java.util.Collection</code>
    * @tparam JCOL any subtype of <code>java.util.Collection</code>
    * @return <code>Containing[JCOL[E]]</code> that supports <code>java.util.Collection</code> in relevant <code>contain</code> syntax
    */
  // SKIP-DOTTY-START
  implicit def containingNatureOfJavaCollection[E, JCOL[e] <: java.util.Collection[e]](implicit equality: Equality[E]): Containing[JCOL[E]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def containingNatureOfJavaCollection[E, JCOL[e] <: java.util.Collection[e]](using equality: Equality[E]): Containing[JCOL[E]] =
    convertEqualityToJavaCollectionContaining(equality)
  
  /**
    // SKIP-DOTTY-START
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    // SKIP-DOTTY-END
    //DOTTY-ONLY   * converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * into <code>Containing</code> of type <code>JCOL[E]</code>, where <code>JCOL</code> is a subtype of <code>java.util.Collection</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * val javaList = new java.util.ArrayList[String]()
    * javaList.add("hi")
    * (javaList should contain oneOf ("HI")) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
    * and this implicit conversion will convert it into <code>Containing[java.util.ArrayList[String]]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * @tparam E type of elements in the <code>java.util.Collection</code>
    * @tparam JCOL subtype of <code>java.util.Collection</code>
    * @return <code>Containing</code> of type <code>JCOL[E]</code>
    */
  // SKIP-DOTTY-START  
  implicit def convertEqualityToJavaCollectionContaining[E, JCOL[e] <: java.util.Collection[e]](equality: Equality[E]): Containing[JCOL[E]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertEqualityToJavaCollectionContaining[E, JCOL[e] <: java.util.Collection[e]](equality: Equality[E]): Containing[JCOL[E]] =
    new Containing[JCOL[E]] {
      def contains(javaColl: JCOL[E], ele: Any): Boolean = {
        val it: java.util.Iterator[E] = javaColl.iterator
        var found = false
        while (!found && it.hasNext) {
          found = equality.areEqual(it.next , ele)
        }
        found
      }
      import scala.collection.JavaConverters._
      def containsOneOf(javaColl: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = {

        val foundSet = checkOneOf[E](javaColl.asScala, elements, equality)
        foundSet.size == 1
      }
      def containsNoneOf(javaColl: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = {
        val found = checkNoneOf[E](javaColl.asScala, elements, equality)
        !found.isDefined
      }
    }

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Given support <code>Containing</code> nature of <code>java.util.Collection</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>java.util.Collection</code>
  //DOTTY-ONLY   * @tparam E the type of the element in the <code>java.util.Collection</code>
  //DOTTY-ONLY   * @tparam JCOL any subtype of <code>java.util.Collection</code>
  //DOTTY-ONLY   * @return <code>Containing[JCOL[E]]</code> that supports <code>java.util.Collection</code> in relevant <code>contain</code> syntax
  //DOTTY-ONLY   */
  //DOTTY-ONLY given [E, JCOL[e] <: java.util.Collection[e]] (using equality: Equality[E]): Containing[JCOL[E]] = convertEqualityToJavaCollectionContaining(equality)

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * into <code>Containing</code> of type <code>E</code>.
  //DOTTY-ONLY   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <pre class="stHighlight">
  //DOTTY-ONLY   * val javaList = new java.util.ArrayList[String]()
  //DOTTY-ONLY   * javaList.add("hi")
  //DOTTY-ONLY   * (javaList should contain oneOf ("HI")) (after being lowerCased)
  //DOTTY-ONLY   * </pre>
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
  //DOTTY-ONLY   * and this implicit conversion will convert it into <code>Containing[java.util.ArrayList[String]]</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * @tparam E type of elements in the <code>java.util.Collection</code>
  //DOTTY-ONLY   * @tparam JCOL subtype of <code>java.util.Collection</code>
  //DOTTY-ONLY   * @return <code>Containing</code> of type <code>JCOL[E]</code>
  //DOTTY-ONLY   */
  //DOTTY-ONLY given equalityJavaCollectionContaining[E, JCOL[e] <: java.util.Collection[e]]: Conversion[Equality[E], Containing[JCOL[E]]] with {
  //DOTTY-ONLY   def apply(equality: Equality[E]): Containing[JCOL[E]] = convertEqualityToJavaCollectionContaining(equality)
  //DOTTY-ONLY }  

  /**
    // SKIP-DOTTY-START
    * Implicit to support <code>Containing</code> nature of <code>java.util.Map</code>.
    // SKIP-DOTTY-END
    //DOTTY-ONLY   * To support <code>Containing</code> nature of <code>java.util.Map</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of entry in the <code>java.util.Map</code>
    * @tparam K the type of the key in the <code>java.util.Map</code>
    * @tparam V the type of the value in the <code>java.util.Map</code>
    * @tparam JMAP any subtype of <code>java.util.Map</code>
    * @return <code>Containing[JMAP[K, V]]</code> that supports <code>java.util.Map</code> in relevant <code>contain</code> syntax
    */
  // SKIP-DOTTY-START  
  implicit def containingNatureOfJavaMap[K, V, JMAP[k, v] <: java.util.Map[k, v]](implicit equality: Equality[java.util.Map.Entry[K, V]]): Containing[JMAP[K, V]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def containingNatureOfJavaMap[K, V, JMAP[k, v] <: java.util.Map[k, v]](using equality: Equality[java.util.Map.Entry[K, V]]): Containing[JMAP[K, V]] =
    convertEqualityToJavaMapContaining(equality)

  /**
    // SKIP-DOTTY-START
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
    // SKIP-DOTTY-END
    //DOTTY-ONLY   * Converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
    * into <code>Containing</code> of type <code>JMAP[K, V]</code>, where <code>JMAP</code> is a subtype of <code>java.util.Map</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * val javaMap = new java.util.HashMap[Int, String]()
    * javaMap.put(1, "one")
    * // lowerCased needs to be implemented as Normalization[java.util.Map.Entry[K, V]]
    * (javaMap should contain (Entry(1, "ONE"))) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>java.util.Map.Entry[Int, String]</code></a>
    * and this implicit conversion will convert it into <code>Containing[java.util.HashMap[Int, String]]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
    * @tparam K the type of the key in the <code>java.util.Map</code>
    * @tparam V the type of the value in the <code>java.util.Map</code>
    * @tparam JMAP any subtype of <code>java.util.Map</code>
    * @return <code>Containing</code> of type <code>JMAP[K, V]</code>
    */
  // SKIP-DOTTY-START
  implicit def convertEqualityToJavaMapContaining[K, V, JMAP[k, v] <: java.util.Map[k, v]](equality: Equality[java.util.Map.Entry[K, V]]): Containing[JMAP[K, V]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertEqualityToJavaMapContaining[K, V, JMAP[k, v] <: java.util.Map[k, v]](equality: Equality[java.util.Map.Entry[K, V]]): Containing[JMAP[K, V]] =
    new Containing[JMAP[K, V]] {
      import scala.collection.JavaConverters._
      def contains(map: JMAP[K, V], ele: Any): Boolean = {
        map.entrySet.asScala.exists((e: java.util.Map.Entry[K, V]) => equality.areEqual(e, ele))
      }
      def containsOneOf(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        val foundSet = checkOneOf[java.util.Map.Entry[K, V]](map.entrySet.asScala, elements, equality)
        foundSet.size == 1
      }
      def containsNoneOf(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        val found = checkNoneOf[java.util.Map.Entry[K, V]](map.entrySet.asScala, elements, equality)
        !found.isDefined
      }
    }

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Given support <code>Containing</code> nature of <code>java.util.Map</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
  //DOTTY-ONLY   * @tparam K the type of the key in the <code>java.util.Map</code>
  //DOTTY-ONLY   * @tparam V the type of the value in the <code>java.util.Map</code>
  //DOTTY-ONLY   * @tparam JMAP any subtype of <code>java.util.Map</code>
  //DOTTY-ONLY   * @return <code>Containing</code> of type <code>JMAP[K, V]</code>
  //DOTTY-ONLY   */
  //DOTTY-ONLY given [K, V, JMAP[k, v] <: java.util.Map[k, v]] (using equality: Equality[java.util.Map.Entry[K, V]]): Containing[JMAP[K, V]] = convertEqualityToJavaMapContaining(equality)

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
  //DOTTY-ONLY   * Converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
  //DOTTY-ONLY   * into <code>Containing</code> of type <code>JMAP[K, V]</code>, where <code>JMAP</code> is a subtype of <code>java.util.Map</code>.
  //DOTTY-ONLY   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <pre class="stHighlight">
  //DOTTY-ONLY   * val javaMap = new java.util.HashMap[Int, String]()
  //DOTTY-ONLY   * javaMap.put(1, "one")
  //DOTTY-ONLY   * // lowerCased needs to be implemented as Normalization[java.util.Map.Entry[K, V]]
  //DOTTY-ONLY   * (javaMap should contain (Entry(1, "ONE"))) (after being lowerCased)
  //DOTTY-ONLY   * </pre>
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>java.util.Map.Entry[Int, String]</code></a>
  //DOTTY-ONLY   * and this implicit conversion will convert it into <code>Containing[java.util.HashMap[Int, String]]</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
  //DOTTY-ONLY   * @tparam K the type of the key in the <code>java.util.Map</code>
  //DOTTY-ONLY   * @tparam V the type of the value in the <code>java.util.Map</code>
  //DOTTY-ONLY   * @tparam JMAP any subtype of <code>java.util.Map</code>
  //DOTTY-ONLY   * @return <code>Containing</code> of type <code>JMAP[K, V]</code>
  //DOTTY-ONLY   */
  //DOTTY-ONLY given equalityJavaMapContaining[K, V, JMAP[k, v] <: java.util.Map[k, v]]: Conversion[Equality[java.util.Map.Entry[K, V]], Containing[JMAP[K, V]]] with {
  //DOTTY-ONLY   def apply(equality: Equality[java.util.Map.Entry[K, V]]): Containing[JMAP[K, V]] = convertEqualityToJavaMapContaining(equality)
  //DOTTY-ONLY }

}

trait ContainingStandardImplicits extends JavaContainingImplicits {

  import scala.language.higherKinds
  // SKIP-DOTTY-START
  import scala.language.implicitConversions
  // SKIP-DOTTY-END

  /**
    // SKIP-DOTTY-START
    * Implicit to support <code>Containing</code> nature of <code>Iterable</code>.
    // SKIP-DOTTY-END
    //DOTTY-ONLY   * To support <code>Containing</code> nature of <code>Iterable</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Iterable</code>
    * @tparam E the type of the element in the <code>Iterable</code>
    * @tparam ITR any subtype of <code>Iterable</code>
    * @return <code>Containing[TRAV[E]]</code> that supports <code>Iterable</code> in relevant <code>contain</code> syntax
    */
  // SKIP-DOTTY-START
  implicit def containingNatureOfIterable[E, ITR[e] <: Iterable[e]](implicit equality: Equality[E]): Containing[ITR[E]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def containingNatureOfIterable[E, ITR[e] <: Iterable[e]](using equality: Equality[E]): Containing[ITR[E]] =
    convertEqualityToIterableContaining(equality)

  /**
    // SKIP-DOTTY-START
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    // SKIP-DOTTY-END
    //DOTTY-ONLY * Converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * into <code>Containing</code> of type <code>TRAV[E]</code>, where <code>TRAV</code> is a subtype of <code>Iterable</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * (List("hi") should contain oneOf ("HI")) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
    * and this implicit conversion will convert it into <code>Containing[List[String]]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * @tparam E type of elements in the <code>Iterable</code>
    * @tparam ITR subtype of <code>Iterable</code>
    * @return <code>Containing</code> of type <code>ITR[E]</code>
    */
  // SKIP-DOTTY-START
  implicit def convertEqualityToIterableContaining[E, ITR[e] <: Iterable[e]](equality: Equality[E]): Containing[ITR[E]] = 
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertEqualityToIterableContaining[E, ITR[e] <: Iterable[e]](equality: Equality[E]): Containing[ITR[E]] = 
    new Containing[ITR[E]] {
      def contains(itr: ITR[E], ele: Any): Boolean = {
        equality match {
          case normEq: NormalizingEquality[_] =>
            val normRight = normEq.normalizedOrSame(ele)
            itr.exists((e: E) => normEq.afterNormalizationEquality.areEqual(normEq.normalized(e), normRight))
          case _ => itr.exists((e: E) => equality.areEqual(e, ele))
        }
      }
      def containsOneOf(itr: ITR[E], elements: scala.collection.Seq[Any]): Boolean = {
        val foundSet = checkOneOf[E](itr, elements, equality)
        foundSet.size == 1
      }
      def containsNoneOf(itr: ITR[E], elements: scala.collection.Seq[Any]): Boolean = {
        val found = checkNoneOf[E](itr, elements, equality)
        !found.isDefined
      }
    }

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Given support <code>Containing</code> nature of <code>Iterable</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Iterable</code>
  //DOTTY-ONLY   * @tparam E the type of the element in the <code>Iterable</code>
  //DOTTY-ONLY   * @tparam ITR any subtype of <code>Iterable</code>
  //DOTTY-ONLY   * @return <code>Containing[ITR[E]]</code> that supports <code>Iterable</code> in relevant <code>contain</code> syntax
  //DOTTY-ONLY   */
  //DOTTY-ONLY given [E, ITR[e] <: Iterable[e]] (using equality: Equality[E]): Containing[ITR[E]] = convertEqualityToIterableContaining(equality)

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * into <code>Containing</code> of type <code>TRAV[E]</code>, where <code>TRAV</code> is a subtype of <code>Iterable</code>.
  //DOTTY-ONLY   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <pre class="stHighlight">
  //DOTTY-ONLY   * (List("hi") should contain oneOf ("HI")) (after being lowerCased)
  //DOTTY-ONLY   * </pre>
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
  //DOTTY-ONLY   * and this implicit conversion will convert it into <code>Containing[List[String]]</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * @tparam E type of elements in the <code>Iterable</code>
  //DOTTY-ONLY   * @tparam ITR subtype of <code>Iterable</code>
  //DOTTY-ONLY   * @return <code>Containing</code> of type <code>ITR[E]</code>
  //DOTTY-ONLY   */
  //DOTTY-ONLY given equalityIterableContaining[E, ITR[e] <: Iterable[e]]: Conversion[Equality[E], Containing[ITR[E]]] with {
  //DOTTY-ONLY   def apply(equality: Equality[E]): Containing[ITR[E]] = convertEqualityToIterableContaining(equality)
  //DOTTY-ONLY }

  // OPT so that it will work with Some also, but it doesn't work with None
  /**
    // SKIP-DOTTY-START
    * Implicit to support <code>Containing</code> nature of <code>scala.Option</code>.
    // SKIP-DOTTY-END
    //DOTTY-ONLY * To support <code>Containing</code> nature of <code>scala.Option</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Option</code>
    * @tparam E the type of the element in the <code>scala.Option</code>
    * @tparam OPT any subtype of <code>scala.Option</code>
    * @return <code>Containing[OPT[E]]</code> that supports <code>scala.Option</code> in relevant <code>contain</code> syntax
    */
  // SKIP-DOTTY-START
  implicit def containingNatureOfOption[E, OPT[e] <: Option[e]](implicit equality: Equality[E]): Containing[OPT[E]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def containingNatureOfOption[E, OPT[e] <: Option[e]](using equality: Equality[E]): Containing[OPT[E]] =
    convertEqualityToOptionContaining(equality)

  /**
    // SKIP-DOTTY-START
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    // SKIP-DOTTY-END
    //DOTTY-ONLY Converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * into <code>Containing</code> of type <code>OPT[E]</code>, where <code>OPT</code> is a subtype of <code>scala.Option</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * (Some("hi") should contain oneOf ("HI")) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
    * and this implicit conversion will convert it into <code>Containing[Some[String]]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * @tparam E type of elements in the <code>scala.Option</code>
    * @tparam OPT subtype of <code>scala.Option</code>
    * @return <code>Containing</code> of type <code>OPT[E]</code>
    */
  // SKIP-DOTTY-START
  implicit def convertEqualityToOptionContaining[E, OPT[e] <: Option[e]](equality: Equality[E]): Containing[OPT[E]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertEqualityToOptionContaining[E, OPT[e] <: Option[e]](equality: Equality[E]): Containing[OPT[E]] =
    new Containing[OPT[E]] {
      def contains(opt: OPT[E], ele: Any): Boolean = {
        opt.exists((e: E) => equality.areEqual(e, ele))
      }
      def containsOneOf(opt: OPT[E], elements: scala.collection.Seq[Any]): Boolean = {
        val foundSet = checkOneOf[E](opt, elements, equality)
        foundSet.size == 1
      }
      def containsNoneOf(opt: OPT[E], elements: scala.collection.Seq[Any]): Boolean = {
        val found = checkNoneOf[E](opt, elements, equality)
        !found.isDefined
      }
    }

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Given support <code>Containing</code> nature of <code>scala.Option</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>scala.Option</code>
  //DOTTY-ONLY   * @tparam E the type of the element in the <code>scala.Option</code>
  //DOTTY-ONLY   * @tparam OPT any subtype of <code>scala.Option</code>
  //DOTTY-ONLY   * @return <code>Containing[OPT[E]]</code> that supports <code>scala.Option</code> in relevant <code>contain</code> syntax
  //DOTTY-ONLY   */
  //DOTTY-ONLY given [E, OPT[e] <: Option[e]] (using equality: Equality[E]): Containing[OPT[E]] = convertEqualityToOptionContaining(equality)

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * into <code>Containing</code> of type <code>OPT[E]</code>, where <code>OPT</code> is a subtype of <code>scala.Option</code>.
  //DOTTY-ONLY   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <pre class="stHighlight">
  //DOTTY-ONLY   * (Some("hi") should contain oneOf ("HI")) (after being lowerCased)
  //DOTTY-ONLY   * </pre>
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
  //DOTTY-ONLY   * and this implicit conversion will convert it into <code>Containing[Some[String]]</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * @tparam E type of elements in the <code>scala.Option</code>
  //DOTTY-ONLY   * @tparam OPT subtype of <code>scala.Option</code>
  //DOTTY-ONLY   * @return <code>Containing</code> of type <code>OPT[E]</code>
  //DOTTY-ONLY   */
  //DOTTY-ONLY given equalityOptionContaining[E, OPT[e] <: Option[e]]: Conversion[Equality[E], Containing[OPT[E]]] with {
  //DOTTY-ONLY   def apply(equality: Equality[E]): Containing[OPT[E]] = convertEqualityToOptionContaining(equality)
  //DOTTY-ONLY }

  /**
    // SKIP-DOTTY-START
    * Implicit to support <code>Containing</code> nature of <code>Array</code>.
    // SKIP-DOTTY-END
    //DOTTY-ONLY * To support <code>Containing</code> nature of <code>Array</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Array</code>
    * @tparam E the type of the element in the <code>Array</code>
    * @return <code>Containing[Array[E]]</code> that supports <code>Array</code> in relevant <code>contain</code> syntax
    */
  // SKIP-DOTTY-START
  implicit def containingNatureOfArray[E](implicit equality: Equality[E]): Containing[Array[E]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def containingNatureOfArray[E](using equality: Equality[E]): Containing[Array[E]] =
    convertEqualityToArrayContaining(equality)

  /**
    // SKIP-DOTTY-START
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    // SKIP-DOTTY-END
    //DOTTY-ONLY * Converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * into <code>Containing</code> of type <code>Array[E]</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * (Array("hi") should contain oneOf ("HI")) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
    * and this implicit conversion will convert it into <code>Containing[Array[String]]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * @tparam E type of elements in the <code>Array</code>
    * @return <code>Containing</code> of type <code>Array[E]</code>
    */
  // SKIP-DOTTY-START
  implicit def convertEqualityToArrayContaining[E](equality: Equality[E]): Containing[Array[E]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertEqualityToArrayContaining[E](equality: Equality[E]): Containing[Array[E]] =
    new Containing[Array[E]] {
      def contains(arr: Array[E], ele: Any): Boolean =
        arr.exists((e: E) => equality.areEqual(e, ele))
      def containsOneOf(arr: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        val foundSet = checkOneOf[E](arr, elements, equality)
        foundSet.size == 1
      }
      def containsNoneOf(arr: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        val found = checkNoneOf[E](arr, elements, equality)
        !found.isDefined
      }
    }

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Given support <code>Containing</code> nature of <code>Array</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * @tparam E type of elements in the <code>Array</code>
  //DOTTY-ONLY   * @return <code>Containing</code> of type <code>Array[E]</code>
  //DOTTY-ONLY   */
  //DOTTY-ONLY given [E] (using equality: Equality[E]): Containing[Array[E]] = convertEqualityToArrayContaining(equality)

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * into <code>Containing</code> of type <code>Array[E]</code>.
  //DOTTY-ONLY   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <pre class="stHighlight">
  //DOTTY-ONLY   * (Array("hi") should contain oneOf ("HI")) (after being lowerCased)
  //DOTTY-ONLY   * </pre>
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
  //DOTTY-ONLY   * and this implicit conversion will convert it into <code>Containing[Array[String]]</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * @tparam E type of elements in the <code>Array</code>
  //DOTTY-ONLY   * @return <code>Containing</code> of type <code>Array[E]</code>
  //DOTTY-ONLY   */
  //DOTTY-ONLY given equalityArrayContaining[E]: Conversion[Equality[E], Containing[Array[E]]] with {
  //DOTTY-ONLY   def apply(equality: Equality[E]): Containing[Array[E]] = convertEqualityToArrayContaining(equality)
  //DOTTY-ONLY }

  /**
    // SKIP-DOTTY-START
    * Implicit to support <code>Containing</code> nature of <code>String</code>.
    // SKIP-DOTTY-END
    //DOTTY-ONLY * To support <code>Containing</code> nature of <code>String</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of <code>Char</code> in the <code>String</code>
    * @return <code>Containing[String]</code> that supports <code>String</code> in relevant <code>contain</code> syntax
    */
  // SKIP-DOTTY-START
  implicit def containingNatureOfString(implicit equality: Equality[Char]): Containing[String] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def containingNatureOfString(using equality: Equality[Char]): Containing[String] =
    convertEqualityToStringContaining(equality)

  /**
    // SKIP-DOTTY-START
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Char</code>
    // SKIP-DOTTY-END
    //DOTTY-ONLY * Converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Char</code>
    * into <code>Containing</code> of type <code>String</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * // lowerCased needs to be implemented as Normalization[Char]
    * ("hi hello" should contain oneOf ('E')) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[Char]</code></a>
    * and this implicit conversion will convert it into <code>Containing[String]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Char</code>
    * @return <code>Containing</code> of type <code>String</code>
    */
  // SKIP-DOTTY-START
  implicit def convertEqualityToStringContaining(equality: Equality[Char]): Containing[String] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertEqualityToStringContaining(equality: Equality[Char]): Containing[String] =
    new Containing[String] {
      def contains(str: String, ele: Any): Boolean =
        str.exists((e: Char) => equality.areEqual(e, ele))
      def containsOneOf(str: String, elements: scala.collection.Seq[Any]): Boolean = {
        val foundSet = checkOneOf[Char](str, elements, equality)
        foundSet.size == 1
      }
      def containsNoneOf(str: String, elements: scala.collection.Seq[Any]): Boolean = {
        val found = checkNoneOf[Char](str, elements, equality)
        !found.isDefined
      }
    }

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Given to support <code>Containing</code> nature of <code>String</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of <code>Char</code> in the <code>String</code>
  //DOTTY-ONLY   * @return <code>Containing[String]</code> that supports <code>String</code> in relevant <code>contain</code> syntax
  //DOTTY-ONLY   */
  //DOTTY-ONLY given (using equality: Equality[Char]): Containing[String] = convertEqualityToStringContaining(equality)

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Char</code>
  //DOTTY-ONLY   * into <code>Containing</code> of type <code>String</code>.
  //DOTTY-ONLY   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <pre class="stHighlight">
  //DOTTY-ONLY   * // lowerCased needs to be implemented as Normalization[Char]
  //DOTTY-ONLY   * ("hi hello" should contain oneOf ('E')) (after being lowerCased)
  //DOTTY-ONLY   * </pre>
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[Char]</code></a>
  //DOTTY-ONLY   * and this implicit conversion will convert it into <code>Containing[String]</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Char</code>
  //DOTTY-ONLY   * @return <code>Containing</code> of type <code>String</code>
  //DOTTY-ONLY   */
  //DOTTY-ONLY given equalityStringContaining: Conversion[Equality[Char], Containing[String]] with {
  //DOTTY-ONLY   def apply(equality: Equality[Char]): Containing[String] = convertEqualityToStringContaining(equality)
  //DOTTY-ONLY }  

  /**
    // SKIP-DOTTY-START
    * Implicit to support <code>Containing</code> nature of <code>Every</code>.
    // SKIP-DOTTY-END
    //DOTTY-ONLY * To support <code>Containing</code> nature of <code>Every</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Every</code>
    * @tparam E the type of the element in the <code>Every</code>
    * @return <code>Containing[Every[E]]</code> that supports <code>Every</code> in relevant <code>contain</code> syntax
    */
  // SKIP-DOTTY-START
  implicit def containingNatureOfEvery[E](implicit equality: Equality[E]): Containing[Every[E]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def containingNatureOfEvery[E](using equality: Equality[E]): Containing[Every[E]] =
    convertEqualityToEveryContaining(equality)

  /**
    // SKIP-DOTTY-START
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    // SKIP-DOTTY-END
    //DOTTY-ONLY * Converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * into <code>Containing</code> of type <code>Every[E]</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * (Every("hi", "he", "ho") should contain oneOf ("HI")) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
    * and this implicit conversion will convert it into <code>Containing[Every[String]]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * @tparam E type of elements in the <code>Every</code>
    * @return <code>Containing</code> of type <code>Every[E]</code>
    */
  // SKIP-DOTTY-START
  implicit def convertEqualityToEveryContaining[E](equality: Equality[E]): Containing[Every[E]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertEqualityToEveryContaining[E](equality: Equality[E]): Containing[Every[E]] =
    new Containing[Every[E]] {
      def contains(every: Every[E], ele: Any): Boolean =
        equality match {
          case normEq: NormalizingEquality[_] =>
            val normRight = normEq.normalizedOrSame(ele)
            every.exists((e: E) => normEq.afterNormalizationEquality.areEqual(normEq.normalized(e), normRight))
          case _ => every.exists((e: E) => equality.areEqual(e, ele))
        }
      def containsOneOf(every: Every[E], elements: scala.collection.Seq[Any]): Boolean = {
        val foundSet = checkOneOf[E](every, elements, equality)
        foundSet.size == 1
      }
      def containsNoneOf(every: Every[E], elements: scala.collection.Seq[Any]): Boolean = {
        val found = checkNoneOf[E](every, elements, equality)
        !found.isDefined
      }
    }

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Given support <code>Containing</code> nature of <code>Every</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Every</code>
  //DOTTY-ONLY   * @tparam E the type of the element in the <code>Every</code>
  //DOTTY-ONLY   * @return <code>Containing[Every[E]]</code> that supports <code>Every</code> in relevant <code>contain</code> syntax
  //DOTTY-ONLY   */
  //DOTTY-ONLY given [E] (using equality: Equality[E]): Containing[Every[E]] = convertEqualityToEveryContaining(equality)

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * into <code>Containing</code> of type <code>Every[E]</code>.
  //DOTTY-ONLY   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <pre class="stHighlight">
  //DOTTY-ONLY   * (Every("hi", "he", "ho") should contain oneOf ("HI")) (after being lowerCased)
  //DOTTY-ONLY   * </pre>
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
  //DOTTY-ONLY   * and this implicit conversion will convert it into <code>Containing[Every[String]]</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * @tparam E type of elements in the <code>Every</code>
  //DOTTY-ONLY   * @return <code>Containing</code> of type <code>Every[E]</code>
  //DOTTY-ONLY   */
  //DOTTY-ONLY given equalityEveryContaining[E]: Conversion[Equality[E], Containing[Every[E]]] with {
  //DOTTY-ONLY   def apply(equality: Equality[E]): Containing[Every[E]] = convertEqualityToEveryContaining(equality)
  //DOTTY-ONLY }  
}

trait ContainingHighPriorityImplicits extends ContainingStandardImplicits {

  /**
    // SKIP-DOTTY-START
    * Implicit to support <code>Containing</code> nature of <code>scala.collection.GenMap</code>.
    // SKIP-DOTTY-END
    //DOTTY-ONLY * To support <code>Containing</code> nature of <code>scala.collection.GenMap</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of entry in the <code>scala.collection.GenMap</code>
    * @tparam K the type of the key in the <code>scala.collection.GenMap</code>
    * @tparam V the type of the value in the <code>scala.collection.GenMap</code>
    * @tparam MAP any subtype of <code>scala.collection.GenMap</code>
    * @return <code>Containing[MAP[K, V]]</code> that supports <code>scala.collection.GenMap</code> in relevant <code>contain</code> syntax
    */
  // SKIP-DOTTY-START
  implicit def containingNatureOfMap[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](implicit equality: Equality[(K, V)]): Containing[MAP[K, V]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def containingNatureOfMap[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](using equality: Equality[(K, V)]): Containing[MAP[K, V]] =
    convertEqualityToMapContaining(equality)

  /**
    // SKIP-DOTTY-START
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Tuple2[K, V]</code>
    // SKIP-DOTTY-END
    //DOTTY-ONLY * Converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Tuple2[K, V]</code>
    * into <code>Containing</code> of type <code>MAP[K, V]</code>, where <code>MAP</code> is a subtype of <code>scala.collection.GenMap</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * val map = Map(1 -> "one")
    * // lowerCased needs to be implemented as Normalization[Tuple2[K, V]]
    * (map should contain ((1, "ONE"))) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Tuple2[Int, String]</code></a>
    * and this implicit conversion will convert it into <code>Containing[scala.collection.GenMap[Int, String]]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Tuple2[K, V]</code>
    * @tparam K the type of the key in the <code>scala.collection.GenMap</code>
    * @tparam V the type of the value in the <code>scala.collection.GenMap</code>
    * @tparam MAP any subtype of <code>scala.collection.GenMap</code>
    * @return <code>Containing</code> of type <code>MAP[K, V]</code>
    */
  // SKIP-DOTTY-START
  implicit def convertEqualityToMapContaining[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](equality: Equality[(K, V)]): Containing[MAP[K, V]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertEqualityToMapContaining[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](equality: Equality[(K, V)]): Containing[MAP[K, V]] =
    new Containing[MAP[K, V]] {
      def contains(map: MAP[K, V], ele: Any): Boolean = {
        map.exists((e: (K, V)) => equality.areEqual(e, ele))
      }
      def containsOneOf(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        val foundSet = checkOneOf[(K, V)](map, elements, equality)
        foundSet.size == 1
      }
      def containsNoneOf(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        val found = checkNoneOf[(K, V)](map, elements, equality)
        !found.isDefined
      }
    }

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Given support <code>Containing</code> nature of <code>scala.collection.GenMap</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of entry in the <code>scala.collection.GenMap</code>
  //DOTTY-ONLY   * @tparam K the type of the key in the <code>scala.collection.GenMap</code>
  //DOTTY-ONLY   * @tparam V the type of the value in the <code>scala.collection.GenMap</code>
  //DOTTY-ONLY   * @tparam MAP any subtype of <code>scala.collection.GenMap</code>
  //DOTTY-ONLY   * @return <code>Containing[MAP[K, V]]</code> that supports <code>scala.collection.GenMap</code> in relevant <code>contain</code> syntax
  //DOTTY-ONLY   */
  //DOTTY-ONLY given [K, V, MAP[k, v] <: scala.collection.GenMap[k, v]] (using equality: Equality[(K, V)]): Containing[MAP[K, V]] = convertEqualityToMapContaining(equality)

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Tuple2[K, V]</code>
  //DOTTY-ONLY   * into <code>Containing</code> of type <code>MAP[K, V]</code>, where <code>MAP</code> is a subtype of <code>scala.collection.GenMap</code>.
  //DOTTY-ONLY   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <pre class="stHighlight">
  //DOTTY-ONLY   * val map = Map(1 -> "one")
  //DOTTY-ONLY   * // lowerCased needs to be implemented as Normalization[Tuple2[K, V]]
  //DOTTY-ONLY   * (map should contain ((1, "ONE"))) (after being lowerCased)
  //DOTTY-ONLY   * </pre>
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Tuple2[Int, String]</code></a>
  //DOTTY-ONLY   * and this implicit conversion will convert it into <code>Containing[scala.collection.GenMap[Int, String]]</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Tuple2[K, V]</code>
  //DOTTY-ONLY   * @tparam K the type of the key in the <code>scala.collection.GenMap</code>
  //DOTTY-ONLY   * @tparam V the type of the value in the <code>scala.collection.GenMap</code>
  //DOTTY-ONLY   * @tparam MAP any subtype of <code>scala.collection.GenMap</code>
  //DOTTY-ONLY   * @return <code>Containing</code> of type <code>MAP[K, V]</code>
  //DOTTY-ONLY   */
  //DOTTY-ONLY given equalityMapContaining[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]]: Conversion[Equality[(K, V)], Containing[MAP[K, V]]] with {
  //DOTTY-ONLY   def apply(equality: Equality[(K, V)]): Containing[MAP[K, V]] = convertEqualityToMapContaining(equality)
  //DOTTY-ONLY }

}

/**
 * Companion object for <code>Containing</code> that provides implicit implementations for the following types:
 *
 * <ul>
 * <li><code>org.scalactic.ColCompatHelper.Iterable</code></li>
 * <li><code>String</code></li>
 * <li><code>Array</code></li>
 * <li><code>scala.Option</code></li>
 * <li><code>java.util.Collection</code></li>
 * <li><code>java.util.Map</code></li>
 * </ul>
 */
object Containing extends ContainingHighPriorityImplicits
