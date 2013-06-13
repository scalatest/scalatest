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
import org.scalautils.NormalizingEquality
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce
import org.scalatest.FailureMessages

/**
 * Supertrait for typeclasses that enable <code>contain</code> matcher syntax for containers.
 *
 * <p>
 * A <code>Containing[C]</code> provides access to the "containing nature" of type <code>C</code> in such
 * a way that relevant <code>contain</code> matcher syntax can be used with type <code>C</code>. A <code>C</code>
 * can be any type of "container," a type that in some way can contains one or more other objects. ScalaTest provides
 * implicit implementations for several types. You can enable the <code>contain</code> matcher syntax on your own
 * type <code>U</code> by defining an <code>Containing[U}</code> for the type and making it available implicitly.
 * 
 * <p>
 * ScalaTest provides implicit <code>Aggregating</code> instances for <code>scala.collection.GenTraversable</code>,
 * <code>java.util.Collection</code>, <code>java.util.Map</code>, <code>String</code>, <code>Array</code>, 
 * and <code>Option</code> in the <code>Aggregating</code> companion object.
 * </p>
 *
 * <a name="containingVersusAggregating"></a>
 * <h2><code>Containing</code> versus <code>Aggregating</code></h2>
 * 
 * <p>
 * The difference between <code>Containing</code> and <a href="Aggregating.html"><code>Aggregating</code></a> is that
 * <code>Containing</code> enables <code>contain</code> matcher syntax that makes sense for "box" types that can
 * contain at most one value (for example, <code>Option</code>),
 * whereas <code>Aggregating</code> enables <code>contain</code> matcher syntax for full-blown collections and other 
 * aggregations of potentially more than one object. For example, it makes sense to make assertions like these, which 
 * are enabled by <code>Containing</code>, for <code>Option</code>:
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
 * However, given an <code>Option</code> can only ever contain at most one object, it doesn't make
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
 * for type <code>Option</code>.
 * </p>
 */
trait Containing[C] {

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
   * @param eles elements exactly one (<em>i.e.</em>, one and only one) of which should be contained in the passed container
   * @return true if the passed container contains exactly one of the passed elements
   */
  def containsOneOf(container: C, elements: scala.collection.Seq[Any]): Boolean

  /**
   * Implements <code>contain</code> <code>atLeastOneOf</code> syntax for containers of type <code>C</code>.
   *
   * @param container a container about which an assertion is being made
   * @param eles elements none of which should be contained in the passed container
   * @return true if the passed container contains at least one of the passed elements
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

object Containing {
  
  private def tryEquality[T](left: Any, right: Any, equality: Equality[T]): Boolean = 
    try equality.areEqual(left.asInstanceOf[T], right)
      catch {
        case cce: ClassCastException => false
    }
  
  private def checkOneOf[T](left: GenTraversableOnce[T], right: GenTraversable[Any], equality: Equality[T]): (Set[Any], Set[Any]) = {
    /*right.foldLeft(Set.empty[Any], Set.empty[Any]) { case ((fs, rs), r) =>
      if (rs.find(e => equality.areEqual(e.asInstanceOf[T], r)).isDefined)
        throw new IllegalArgumentException(FailureMessages("oneOfDuplicate", r))
      if (left.exists(t => equality.areEqual(t, r))) {
        // r is in the left
        if (fs.size != 0) // This .size should be safe, it won't go > 1
          return (fs + r, rs) // fail early by returning early, hmm..  not so 'functional'??
        else
          (fs + r, rs + r)
      }
      else 
        (fs, rs + r) // r is not in the left
    }*/
    // aggregate version is more verbose, but it allows parallel execution.
    right.aggregate(Set.empty[Any], Set.empty[Any])( 
      { case (((fs, rs), r)) => 
          if (rs.find(e => tryEquality(e, r, equality)).isDefined)
            throw new IllegalArgumentException(FailureMessages("oneOfDuplicate", r))
          if (left.exists(t => equality.areEqual(t, r))) {
            // r is in the left
            if (fs.size != 0) // This .size should be safe, it won't go > 1
              return (fs + r, rs) // fail early by returning early, hmm..  not so 'functional'??
            else
              (fs + r, rs + r)
          }
          else 
            (fs, rs + r) // r is not in the left
      }, 
      { case ((fs1, rs1), (fs2, rs2)) => 
        rs1.find(e1 => rs2.exists(e2 => equality.areEqual(e1.asInstanceOf[T], e2))) match {
          case Some(r) => 
            throw new IllegalArgumentException(FailureMessages("oneOfDuplicate", r))
          case None =>
        }
        val fs = fs1 + fs2
        if (fs.size > 1)
          return (fs, rs1 ++ rs2) // fail early by returning early
        else
          (fs, rs1 ++ rs2)
      }
    )
  }
  
  private def checkNoneOf[T](left: GenTraversableOnce[T], right: GenTraversable[Any], equality: Equality[T]): (Option[Any], Set[Any]) = {
    right.aggregate(None, Set.empty[Any])( 
      { case (((f, rs), r)) => 
          if (rs.find(e => tryEquality(e, r, equality)).isDefined)
            throw new IllegalArgumentException(FailureMessages("noneOfDuplicate", r))
          if (left.exists(t => equality.areEqual(t, r))) 
            return (Some(r), rs + r) // r is in the left, fail early by returning.
          else 
            (None, rs + r) // r is not in the left
      }, 
      { case ((f1, rs1), (f2, rs2)) => 
          rs1.find(e1 => rs2.exists(e2 => equality.areEqual(e1.asInstanceOf[T], e2))) match {
            case Some(r) => 
              throw new IllegalArgumentException(FailureMessages("noneOfDuplicate", r))
            case None =>
              (None, rs1 ++ rs2)
          }
      }
    )
  }

  implicit def withJavaCollectionElementEquality[E, JCOL[_] <: java.util.Collection[_]](implicit equality: Equality[E]): Containing[JCOL[E]] = 
    new Containing[JCOL[E]] {
      def contains(javaColl: JCOL[E], ele: Any): Boolean = {
        val it: java.util.Iterator[E] = javaColl.iterator.asInstanceOf[java.util.Iterator[E]]
        var found = false
        while (!found && it.hasNext) {
          found = equality.areEqual(it.next , ele)
        }
        found
      }
      import scala.collection.JavaConverters._
      def containsOneOf(javaColl: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = {
        
        val (foundSet, processedSet) = checkOneOf[E](javaColl.asInstanceOf[java.util.Collection[E]].asScala, elements, equality)
        foundSet.size == 1
      }
      def containsNoneOf(javaColl: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = {
        val (found, processedSet) = checkNoneOf[E](javaColl.asInstanceOf[java.util.Collection[E]].asScala, elements, equality)
        !found.isDefined
      }
    }

  implicit def convertEqualityToJavaCollectionContaining[E, JCOL[_] <: java.util.Collection[_]](equality: Equality[E]): Containing[JCOL[E]] = 
    withJavaCollectionElementEquality(equality)

  implicit def withGenTraversableElementEquality[E, TRAV[_] <: scala.collection.GenTraversable[_]](implicit equality: Equality[E]): Containing[TRAV[E]] = 
    new Containing[TRAV[E]] {
      def contains(trav: TRAV[E], ele: Any): Boolean = {
        equality match {
          case normEq: NormalizingEquality[_] => 
            val normRight = normEq.normalizedIfInstanceOfA(ele)
            trav.exists((e: Any) => normEq.afterNormalizationEquality.areEqual(normEq.normalized(e.asInstanceOf[E]), normRight)) // Don't know why the compiler requires e to be type Any. Should be E.
          case _ => trav.exists((e: Any) => equality.areEqual(e.asInstanceOf[E], ele)) // Don't know why the compiler requires e to be type Any. Should be E.
        }
      }
      def containsOneOf(trav: TRAV[E], elements: scala.collection.Seq[Any]): Boolean = {
        val (foundSet, processedSet) = checkOneOf[E](trav.asInstanceOf[GenTraversable[E]], elements, equality)
        foundSet.size == 1
      }
      def containsNoneOf(trav: TRAV[E], elements: scala.collection.Seq[Any]): Boolean = {
        val (found, processedSet) = checkNoneOf[E](trav.asInstanceOf[GenTraversable[E]], elements, equality)
        !found.isDefined
      }
    }

  // Enables (xs should contain ("HI")) (after being lowerCased)
  implicit def convertEqualityToGenTraversableContaining[E, TRAV[_] <: scala.collection.GenTraversable[_]](equality: Equality[E]): Containing[TRAV[E]] = 
    withGenTraversableElementEquality(equality)

  // OPT so that it will work with Some also, but it doesn't work with None
  implicit def withOptionValueEquality[E, OPT[_] <: Option[_]](implicit equality: Equality[E]): Containing[OPT[E]] = 
    new Containing[OPT[E]] {
      def contains(opt: OPT[E], ele: Any): Boolean = {
        opt.exists((e: Any) => equality.areEqual(e.asInstanceOf[E], ele)) // Don't know why the compiler requires e to be type Any. Should be E.
      }
      def containsOneOf(opt: OPT[E], elements: scala.collection.Seq[Any]): Boolean = {
        val (foundSet, processedSet) = checkOneOf[E](opt.asInstanceOf[Option[E]], elements, equality)
        foundSet.size == 1
      }
      def containsNoneOf(opt: OPT[E], elements: scala.collection.Seq[Any]): Boolean = {
        val (found, processedSet) = checkNoneOf[E](opt.asInstanceOf[Option[E]], elements, equality)
        !found.isDefined
      }
    }

  // supports (some should contain ("HI")) (after being lowerCased)
  implicit def convertEqualityToOptionContaining[E, OPT[_] <: Option[_]](equality: Equality[E]): Containing[OPT[E]] = 
    withOptionValueEquality(equality)

  implicit def withGenMapElementEquality[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]](implicit equality: Equality[(K, V)]): Containing[MAP[K, V]] = 
    new Containing[MAP[K, V]] {
      def contains(map: MAP[K, V], ele: Any): Boolean = {
        map.exists((e: Any) => equality.areEqual(e.asInstanceOf[(K, V)], ele)) // Don't know why the compiler requires e to be type Any. Should be E.
      }
      def containsOneOf(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        val (foundSet, processedSet) = checkOneOf[(K, V)](map.asInstanceOf[scala.collection.GenMap[K, V]], elements, equality)
        foundSet.size == 1
      }
      def containsNoneOf(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        val (found, processedSet) = checkNoneOf[(K, V)](map.asInstanceOf[scala.collection.GenMap[K, V]], elements, equality)
        !found.isDefined
      }
    }

  implicit def convertEqualityToGenMapContaining[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]](equality: Equality[(K, V)]): Containing[MAP[K, V]] = 
    withGenMapElementEquality(equality)

  implicit def withArrayElementEquality[E](implicit equality: Equality[E]): Containing[Array[E]] = 
    new Containing[Array[E]] {
      def contains(arr: Array[E], ele: Any): Boolean =
        arr.exists((e: E) => equality.areEqual(e, ele))
      def containsOneOf(arr: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        val (foundSet, processedSet) = checkOneOf[E](arr, elements, equality)
        foundSet.size == 1
      }
      def containsNoneOf(arr: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        val (found, processedSet) = checkNoneOf[E](arr, elements, equality)
        !found.isDefined
      }
    }

  implicit def convertEqualityToArrayContaining[E](equality: Equality[E]): Containing[Array[E]] = 
    withArrayElementEquality(equality)

  implicit def withStringCharacterEquality(implicit equality: Equality[Char]): Containing[String] = 
    new Containing[String] {
      def contains(str: String, ele: Any): Boolean =
        str.exists((e: Char) => equality.areEqual(e, ele))
      def containsOneOf(str: String, elements: scala.collection.Seq[Any]): Boolean = {
        val (foundSet, processedSet) = checkOneOf[Char](str, elements, equality)
        foundSet.size == 1
      }
      def containsNoneOf(str: String, elements: scala.collection.Seq[Any]): Boolean = {
        val (found, processedSet) = checkNoneOf[Char](str, elements, equality)
        !found.isDefined
      }
    }

  implicit def convertEqualityToStringContaining(equality: Equality[Char]): Containing[String] = 
    withStringCharacterEquality(equality)
    
  implicit def withJavaMapElementEquality[K, V, JMAP[_, _] <: java.util.Map[_, _]](implicit equality: Equality[(K, V)]): Containing[JMAP[K, V]] = 
    new Containing[JMAP[K, V]] {
      def contains(map: JMAP[K, V], ele: Any): Boolean = {
        import scala.collection.JavaConverters._
        map.asInstanceOf[java.util.Map[K, V]].asScala.exists((e: Any) => equality.areEqual(e.asInstanceOf[(K, V)], ele)) // Don't know why the compiler requires e to be type Any. Should be E.
      }
      import scala.collection.JavaConverters._
      def containsOneOf(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        val (foundSet, processedSet) = checkOneOf[(K, V)](map.asInstanceOf[java.util.Map[K, V]].asScala, elements, equality)
        foundSet.size == 1
      }
      def containsNoneOf(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        val (found, processedSet) = checkNoneOf[(K, V)](map.asInstanceOf[java.util.Map[K, V]].asScala, elements, equality)
        !found.isDefined
      }
    }

  implicit def convertEqualityToJavaMapContaining[K, V, JMAP[_, _] <: java.util.Map[_, _]](equality: Equality[(K, V)]): Containing[JMAP[K, V]] = 
    withJavaMapElementEquality(equality)
}

