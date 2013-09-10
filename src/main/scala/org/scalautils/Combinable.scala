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
package org.scalautils

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.collection.GenSet

trait Combinable[G, ERR, COLL[_]] {
  def combined: COLL[G] Or Every[ERR]
}

object Combinable {

  implicit def convertGenTraversableOnceToCombinable[G, ERR, EVERY[b] <: Every[b], TRAVONCE[+e] <: GenTraversableOnce[e]](xs: TRAVONCE[G Or EVERY[ERR]])(implicit cbf: CanBuildFrom[TRAVONCE[G Or EVERY[ERR]], G, TRAVONCE[G]]): Combinable[G, ERR, TRAVONCE] = 

    new Combinable[G, ERR, TRAVONCE] {

      def combined: TRAVONCE[G] Or Every[ERR] = {
        // So now I have an empty builder
        val emptyTRAVONCEOfGBuilder: Builder[G, TRAVONCE[G]] = cbf(xs)
        // So now I want to foldLeft across my TRAVONCE[G Or EVERY[ERR]], starting with an empty TRAVONCEOfGBuilder, and each step along the way, I'll
        // += into the builder, what? Oh I get it. The result type of my foldLeft needs to be Builder[Seq[G]] Or Every[ERR]
        val tempOr: Builder[G, TRAVONCE[G]] Or Every[ERR] = 
          xs.foldLeft((Good(emptyTRAVONCEOfGBuilder): Builder[G, TRAVONCE[G]] Or Every[ERR])) { (accumulator: Builder[G, TRAVONCE[G]] Or Every[ERR],  nextElem: G Or Every[ERR]) =>
            (accumulator, nextElem) match {
              case (Good(bldr), Good(ele)) => Good(bldr += ele)
              case (Good(_), Bad(err)) => Bad(err)
              case (Bad(errA), Bad(errB)) => Bad(errA ++ errB)
              case (Bad(errA), Good(_)) => Bad(errA)
            }
          }
        tempOr map (_.result)
      }
    }

  // Must have another one for Sets, because they are not covariant. Will need to handle Good/Nothing case specially therefore, and plan to do that
  // with another implicit here.
  implicit def convertGenSetToCombinable[G, ERR, X, EVERY[b] <: Every[b], SET[e] <: GenSet[e]](xs: SET[X with (G Or EVERY[ERR])])(implicit cbf: CanBuildFrom[SET[X with (G Or EVERY[ERR])], G, SET[G]]): Combinable[G, ERR, SET] = 

    new Combinable[G, ERR, SET] {

      def combined: SET[G] Or Every[ERR] = {
        // So now I have an empty builder
        val emptySETOfGBuilder: Builder[G, SET[G]] = cbf(xs)
        // So now I want to foldLeft across my SET[G Or EVERY[ERR]], starting with an empty SETOfGBuilder, and each step along the way, I'll
        // += into the builder, what? Oh I get it. The result type of my foldLeft needs to be Builder[Seq[G]] Or Every[ERR]
        val tempOr: Builder[G, SET[G]] Or Every[ERR] = 
          xs.foldLeft((Good(emptySETOfGBuilder): Builder[G, SET[G]] Or Every[ERR])) { (accumulator: Builder[G, SET[G]] Or Every[ERR],  nextElem: G Or Every[ERR]) =>
            (accumulator, nextElem) match {
              case (Good(bldr), Good(ele)) => Good(bldr += ele)
              case (Good(_), Bad(err)) => Bad(err)
              case (Bad(errA), Bad(errB)) => Bad(errA ++ errB)
              case (Bad(errA), Good(_)) => Bad(errA)
            }
          }
        tempOr map (_.result)
      }
    }

  implicit def convertEveryToCombinable[G, ERR](oneToMany: Every[G Or Every[ERR]]): Combinable[G, ERR, Every] = 

    new Combinable[G, ERR, Every] {

      def combined: Every[G] Or Every[ERR] = {
        val vec = oneToMany.toVector
        val z: Every[G] Or Every[ERR] =
          vec.head match {
            case Good(g) => Good(One(g))
            case Bad(err) => Bad(err)
          }
        vec.tail.foldLeft(z) { (accumulator: Every[G] Or Every[ERR],  nextElem: G Or Every[ERR]) =>
          (accumulator, nextElem) match {
            case (Good(everyG), Good(g)) => Good(everyG :+ g)
            case (Good(_), Bad(err)) => Bad(err)
            case (Bad(errA), Bad(errB)) => Bad(errA ++ errB)
            case (Bad(errA), Good(_)) => Bad(errA)
          }
        }
      }
    }

  implicit def convertOptionToCombinable[G, ERR](option: Option[G Or Every[ERR]]): Combinable[G, ERR, Option] = 
    new Combinable[G, ERR, Option] {
      def combined: Option[G] Or Every[ERR] =
        option match {
          case Some(Good(g)) => Good(Some(g))
          case Some(Bad(err)) => Bad(err)
          case None => Good(None)
        }
    }
}

