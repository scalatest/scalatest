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
import Aggregating.tryEquality

/**
 * Supertrait for typeclasses that enable <code>empty</code> matcher syntax.
 *
 * <p>
 * A <code>Emptiness[C]</code> provides access to the "emptiness" of type <code>C</code> in such
 * a way that relevant <code>empty</code> matcher syntax can be used with type <code>C</code>. A <code>C</code>
 * can be any type that in some way can be empty. ScalaTest provides implicit implementations for several types. 
 * You can enable the <code>empty</code> matcher syntax on your own type <code>U</code> by defining an <code>Emptiness[U}</code> 
 * for the type and making it available implicitly.
 * 
 * <p>
 * ScalaTest provides implicit <code>Emptiness</code> instances for <code>scala.collection.GenTraversable</code>,
 * <code>java.util.Collection</code>, <code>java.util.Map</code>, <code>String</code>, <code>Array</code>, 
 * and <code>Option</code> in the <code>Emptiness</code> companion object.
 * </p>
 */
trait Emptiness[-T] {

  /**
   * Determines whether the passed thing is readable, <em>i.e.</em>, the passed file is readable.
   */
  def isEmpty(thing: T): Boolean
}

object Emptiness {

  // TODO: Don't need to capture the type parameter, more like length and size.
  // And after that I think it need not be contravariant.
  /**
   * Enable emptiness for <code>scala.collection.GenTraversable</code>
   */
  implicit def emptinessOfGenTraversable[TRAV <: scala.collection.GenTraversable[_]]: Emptiness[TRAV] =
    new Emptiness[TRAV] {
      def isEmpty(trav: TRAV): Boolean = trav.isEmpty
    }
  
  /**
   * Enable emptiness for <code>Array</code>
   */
  implicit def emptinessOfArray[E]: Emptiness[Array[E]] =
    new Emptiness[Array[E]] {
      def isEmpty(arr: Array[E]): Boolean = arr.length == 0
    }
  
  /**
   * Enable emptiness for <code>String</code>
   */
  implicit def emptinessOfString: Emptiness[String] =
    new Emptiness[String] {
      def isEmpty(str: String): Boolean = str.isEmpty
    }
  
  /**
   * Enable emptiness for <code>Option</code>
   */
  implicit def emptinessOfOption[OPT <: Option[_]]: Emptiness[OPT] =
    new Emptiness[OPT] {
      def isEmpty(opt: OPT): Boolean = opt.isEmpty
    }
  
  /**
   * Enable emptiness for <code>java.util.Collection</code>
   */
  implicit def emptinessOfJavaCollection[JCOL <: java.util.Collection[_]]: Emptiness[JCOL] =
    new Emptiness[JCOL] {
      def isEmpty(jcol: JCOL): Boolean = jcol.isEmpty
    }

  /**
   * Enable emptiness for <code>java.util.Map</code>
   */
  implicit def emptinessOfJavaMap[JMAP <: java.util.Map[_, _]]: Emptiness[JMAP] =
    new Emptiness[JMAP] {
      def isEmpty(jmap: JMAP): Boolean = jmap.isEmpty
    }
}

