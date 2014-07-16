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
package org.scalactic.enablers

import org.scalactic.{Equality, NormalizingEquality, Every, Constraint}
import scala.collection.{GenTraversableOnce, GenTraversable}
import org.scalactic.TripleEqualsSupport.EqualityConstraint
import annotation.implicitNotFound

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
 * ScalaTest provides implicit <code>Containing</code> instances for <code>scala.collection.GenTraversable</code>,
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
@implicitNotFound(msg = "Could not find evidence that ${R} can be contained in ${C}; the missing implicit parameter is of type org.scalactic.enablers.ContainingConstraint[${C},${R}]")
trait ContainingConstraint[-C, R] {

  /**
   * Implements <code>contain</code> <code>&lt;value&gt;</code> syntax for containers of type <code>C</code>.
   *
   * @param container a container about which an assertion is being made
   * @param element an element that should be contained in the passed container
   * @return true if the passed container contains the passed element
   */
  def contains(container: C, element: R): Boolean

  /**
   * Implements <code>contain</code> <code>oneOf</code> syntax for containers of type <code>C</code>.
   *
   * @param container a container about which an assertion is being made
   * @param elements elements exactly one (<em>i.e.</em>, one and only one) of which should be contained in the passed container
   * @return true if the passed container contains exactly one of the passed elements
   */
  def containsOneOf(container: C, elements: scala.collection.Seq[R]): Boolean

  /**
   * Implements <code>contain</code> <code>noneOf</code> syntax for containers of type <code>C</code>.
   *
   * @param container a container about which an assertion is being made
   * @param elements elements none of which should be contained in the passed container
   * @return true if the passed container contains none of the passed elements
   */
  def containsNoneOf(container: C, elements: scala.collection.Seq[R]): Boolean
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

/**
 * Companion object for <code>Containing</code> that provides implicit implementations for the following types:
 *
 * <ul>
 * <li><code>scala.collection.GenTraversable</code></li>
 * <li><code>String</code></li>
 * <li><code>Array</code></li>
 * <li><code>scala.Option</code></li>
 * <li><code>java.util.Collection</code></li>
 * <li><code>java.util.Map</code></li>
 * </ul>
 */
object ContainingConstraint {
  
  private def tryEquality[T](left: Any, right: Any, equality: Equality[T]): Boolean = 
    try equality.areEqual(left.asInstanceOf[T], right)
      catch {
        case cce: ClassCastException => false
    }
  
  private[scalactic] def checkOneOf[T, R](left: GenTraversableOnce[T], right: GenTraversable[R], constraint: Constraint[T, R]): Set[R] = {
    // aggregate version is more verbose, but it allows parallel execution.
    right.aggregate(Set.empty[R])( 
      { case (fs, r) => 
          if (left.exists(t => constraint.areEqual(t, r))) {
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
        val fs = fs1 + fs2.asInstanceOf[R] // TODO: Get rid of this cast
        if (fs.size > 1)
          return fs // fail early by returning early
        else
          fs
      }
    )
  }
  
  private[scalactic] def checkNoneOf[T, R](left: GenTraversableOnce[T], right: GenTraversable[R], constraint: Constraint[T, R]): Option[R] = {
    right.aggregate(None)( 
      { case (f, r) => 
          if (left.exists(t => constraint.areEqual(t, r))) 
            return Some(r) // r is in the left, fail early by returning.
          else 
            None // r is not in the left
      }, 
      { case (f1, f2) => None }
    )
  }

  import scala.language.higherKinds

  /**
   * Implicit to support <code>Containing</code> nature of <code>java.util.Collection</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>java.util.Collection</code>
   * @tparam E the type of the element in the <code>java.util.Collection</code>
   * @tparam JCOL any subtype of <code>java.util.Collection</code>
   * @return <code>Containing[JCOL[E]]</code> that supports <code>java.util.Collection</code> in relevant <code>contain</code> syntax
   */
  implicit def containingNatureOfJavaCollection[E, JCOL[e] <: java.util.Collection[e], R](implicit constraint: Constraint[E, R]): ContainingConstraint[JCOL[E], R] = 
    new ContainingConstraint[JCOL[E], R] {
      def contains(javaColl: JCOL[E], ele: R): Boolean = {
        val it: java.util.Iterator[E] = javaColl.iterator
        var found = false
        while (!found && it.hasNext) {
          found = constraint.areEqual(it.next , ele)
        }
        found
      }
      import scala.collection.JavaConverters._
      def containsOneOf(javaColl: JCOL[E], elements: scala.collection.Seq[R]): Boolean = {
        
        val foundSet = checkOneOf[E, R](javaColl.asScala, elements, constraint)
        foundSet.size == 1
      }
      def containsNoneOf(javaColl: JCOL[E], elements: scala.collection.Seq[R]): Boolean = {
        val found = checkNoneOf[E, R](javaColl.asScala, elements, constraint)
        !found.isDefined
      }
    }

  import scala.language.implicitConversions

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
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
  implicit def convertEqualityToJavaCollectionContainingConstraint[E, JCOL[e] <: java.util.Collection[e], R](equality: Equality[E]): ContainingConstraint[JCOL[E], R] = 
    containingNatureOfJavaCollection(new EqualityConstraint[E, R](equality))

  /**
   * Implicit to support <code>Containing</code> nature of <code>GenTraversable</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>GenTraversable</code>
   * @tparam E the type of the element in the <code>GenTraversable</code>
   * @tparam TRAV any subtype of <code>GenTraversable</code>
   * @return <code>Containing[TRAV[E]]</code> that supports <code>GenTraversable</code> in relevant <code>contain</code> syntax
   */
  implicit def containingNatureOfGenTraversable[E, TRAV[e] <: scala.collection.GenTraversable[e], R](implicit constraint: Constraint[E, R]): ContainingConstraint[TRAV[E], R] = 
    new ContainingConstraint[TRAV[E], R] {
      def contains(trav: TRAV[E], ele: R): Boolean = {
        constraint match {
/*
          case normEq: NormalizingEquality[_] => 
            val normRight = normEq.normalizedOrSame(ele)
            trav.exists((e: E) => normEq.afterNormalizationEquality.areEqual(normEq.normalized(e), normRight))
*/
          case _ => trav.exists((e: E) => constraint.areEqual(e, ele))
        }
      }
      def containsOneOf(trav: TRAV[E], elements: scala.collection.Seq[R]): Boolean = {
        val foundSet = checkOneOf[E, R](trav, elements, constraint)
        foundSet.size == 1
      }
      def containsNoneOf(trav: TRAV[E], elements: scala.collection.Seq[R]): Boolean = {
        val found = checkNoneOf[E, R](trav, elements, constraint)
        !found.isDefined
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * into <code>Containing</code> of type <code>TRAV[E]</code>, where <code>TRAV</code> is a subtype of <code>GenTraversable</code>.
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
   * @tparam E type of elements in the <code>GenTraversable</code>
   * @tparam TRAV subtype of <code>GenTraversable</code>
   * @return <code>Containing</code> of type <code>TRAV[E]</code>
   */
  implicit def convertEqualityToGenTraversableContainingConstraint[E, TRAV[e] <: scala.collection.GenTraversable[e], R](equality: Equality[E]): ContainingConstraint[TRAV[E], R] = 
    containingNatureOfGenTraversable(new EqualityConstraint[E, R](equality))

  // OPT so that it will work with Some also, but it doesn't work with None
  /**
   * Implicit to support <code>Containing</code> nature of <code>scala.Option</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Option</code>
   * @tparam E the type of the element in the <code>scala.Option</code>
   * @tparam OPT any subtype of <code>scala.Option</code>
   * @return <code>Containing[OPT[E]]</code> that supports <code>scala.Option</code> in relevant <code>contain</code> syntax
   */
  implicit def containingNatureOfOption[E, OPT[e] <: Option[e], R](implicit constraint: Constraint[E, R]): ContainingConstraint[OPT[E], R] = 
    new ContainingConstraint[OPT[E], R] {
      def contains(opt: OPT[E], ele: R): Boolean = {
        opt.exists((e: E) => constraint.areEqual(e, ele))
      }
      def containsOneOf(opt: OPT[E], elements: scala.collection.Seq[R]): Boolean = {
        val foundSet = checkOneOf[E, R](opt, elements, constraint)
        foundSet.size == 1
      }
      def containsNoneOf(opt: OPT[E], elements: scala.collection.Seq[R]): Boolean = {
        val found = checkNoneOf[E, R](opt, elements, constraint)
        !found.isDefined
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
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
  implicit def convertEqualityToOptionContainingConstraint[E, OPT[e] <: Option[e], R](equality: Equality[E]): ContainingConstraint[OPT[E], R] = 
    containingNatureOfOption(new EqualityConstraint[E, R](equality))

  /**
   * Implicit to support <code>Containing</code> nature of <code>Array</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Array</code>
   * @tparam E the type of the element in the <code>Array</code>
   * @return <code>Containing[Array[E]]</code> that supports <code>Array</code> in relevant <code>contain</code> syntax
   */
  implicit def containingNatureOfArray[E, R](implicit constraint: Constraint[E, R]): ContainingConstraint[Array[E], R] = 
    new ContainingConstraint[Array[E], R] {
      def contains(arr: Array[E], ele: R): Boolean =
        arr.exists((e: E) => constraint.areEqual(e, ele))
      def containsOneOf(arr: Array[E], elements: scala.collection.Seq[R]): Boolean = {
        val foundSet = checkOneOf[E, R](arr, elements, constraint)
        foundSet.size == 1
      }
      def containsNoneOf(arr: Array[E], elements: scala.collection.Seq[R]): Boolean = {
        val found = checkNoneOf[E, R](arr, elements, constraint)
        !found.isDefined
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
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
  implicit def convertEqualityToArrayContaining[E, R](equality: Equality[E]): ContainingConstraint[Array[E], R] = 
    containingNatureOfArray(new EqualityConstraint[E, R](equality))

  /**
   * Implicit to support <code>Containing</code> nature of <code>String</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of <code>Char</code> in the <code>String</code>
   * @return <code>Containing[String]</code> that supports <code>String</code> in relevant <code>contain</code> syntax
   */
  implicit def containingNatureOfString[R](implicit constraint: Constraint[Char, R]): ContainingConstraint[String, R] = 
    new ContainingConstraint[String, R] {
      def contains(str: String, ele: R): Boolean =
        str.exists((e: Char) => constraint.areEqual(e, ele))
      def containsOneOf(str: String, elements: scala.collection.Seq[R]): Boolean = {
        val foundSet = checkOneOf[Char, R](str, elements, constraint)
        foundSet.size == 1
      }
      def containsNoneOf(str: String, elements: scala.collection.Seq[R]): Boolean = {
        val found = checkNoneOf[Char, R](str, elements, constraint)
        !found.isDefined
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Char</code>
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
  implicit def convertEqualityToStringContainingConstraint[R](equality: Equality[Char]): ContainingConstraint[String, R] = 
    containingNatureOfString(new EqualityConstraint[Char, R](equality))

  /**
   * Implicit to support <code>Containing</code> nature of <code>java.util.Map</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of entry in the <code>java.util.Map</code>
   * @tparam K the type of the key in the <code>java.util.Map</code>
   * @tparam V the type of the value in the <code>java.util.Map</code>
   * @tparam JMAP any subtype of <code>java.util.Map</code>
   * @return <code>Containing[JMAP[K, V]]</code> that supports <code>java.util.Map</code> in relevant <code>contain</code> syntax
   */
  implicit def containingNatureOfJavaMap[K, V, JMAP[k, v] <: java.util.Map[k, v], R](implicit constraint: Constraint[java.util.Map.Entry[K, V], R]): ContainingConstraint[JMAP[K, V], R] = 
    new ContainingConstraint[JMAP[K, V], R] {
      import scala.collection.JavaConverters._
      def contains(map: JMAP[K, V], ele: R): Boolean = {
        map.entrySet.asScala.exists((e: java.util.Map.Entry[K, V]) => constraint.areEqual(e, ele))
      }
      def containsOneOf(map: JMAP[K, V], elements: scala.collection.Seq[R]): Boolean = {
        val foundSet = checkOneOf[java.util.Map.Entry[K, V], R](map.entrySet.asScala, elements, constraint)
        foundSet.size == 1
      }
      def containsNoneOf(map: JMAP[K, V], elements: scala.collection.Seq[R]): Boolean = {
        val found = checkNoneOf[java.util.Map.Entry[K, V], R](map.entrySet.asScala, elements, constraint)
        !found.isDefined
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
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
  implicit def convertEqualityToJavaMapContainingConstraint[K, V, JMAP[k, v] <: java.util.Map[k, v], R](equality: Equality[java.util.Map.Entry[K, V]]): ContainingConstraint[JMAP[K, V], R] = 
    containingNatureOfJavaMap(new EqualityConstraint[java.util.Map.Entry[K, V], R](equality))

  /**
   * Implicit to support <code>Containing</code> nature of <code>Every</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Every</code>
   * @tparam E the type of the element in the <code>Every</code>
   * @return <code>Containing[Every[E]]</code> that supports <code>Every</code> in relevant <code>contain</code> syntax
   */
  implicit def containingNatureOfEvery[E, R](implicit constraint: Constraint[E, R]): ContainingConstraint[Every[E], R] =
    new ContainingConstraint[Every[E], R] {
      def contains(every: Every[E], ele: R): Boolean =
        constraint match {
/*
          case normEq: NormalizingEquality[_] =>
            val normRight = normEq.normalizedOrSame(ele)
            every.exists((e: E) => normEq.afterNormalizationEquality.areEqual(normEq.normalized(e), normRight))
*/
          case _ => every.exists((e: E) => constraint.areEqual(e, ele))
        }
      def containsOneOf(every: Every[E], elements: scala.collection.Seq[R]): Boolean = {
        val foundSet = checkOneOf[E, R](every, elements, constraint)
        foundSet.size == 1
      }
      def containsNoneOf(every: Every[E], elements: scala.collection.Seq[R]): Boolean = {
        val found = checkNoneOf[E, R](every, elements, constraint)
        !found.isDefined
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
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
  implicit def convertEqualityToEveryContainingConstraint[E, R](equality: Equality[E]): ContainingConstraint[Every[E], R] =
    containingNatureOfEvery(new EqualityConstraint[E, R](equality))
}


