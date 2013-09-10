package org.scalautils

import scala.collection.GenTraversableOnce
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder
import scala.collection.GenSet

trait Validatable[G, COLL[_]] {
  def validatedBy[ERR, EVERY[e] <: Every[e]](fn: G => G Or EVERY[ERR]): COLL[G] Or Every[ERR]
}

object Validatable {

  implicit def convertGenTraversableOnceToValidatable[G, TRAVONCE[e] <: GenTraversableOnce[e]](xs: TRAVONCE[G])(implicit cbf: CanBuildFrom[TRAVONCE[G], G, TRAVONCE[G]]): Validatable[G, TRAVONCE] = 

    new Validatable[G, TRAVONCE] {

      def validatedBy[ERR, EVERY[e] <: Every[e]](fn: G => G Or EVERY[ERR]): TRAVONCE[G] Or Every[ERR] = {
        // So now I have an empty builder
        val emptyTRAVONCEOfGBuilder: Builder[G, TRAVONCE[G]] = cbf(xs)
        // So now I want to foldLeft across my TRAVONCE[G Or EVERY[ERR]], starting with an empty TRAVONCEOfGBuilder, and each step along the way, I'll
        // += into the builder, what? Oh I get it. The result type of my foldLeft needs to be Builder[Seq[G]] Or Every[ERR]
        val tempOr: Builder[G, TRAVONCE[G]] Or Every[ERR] = 
          xs.foldLeft((Good(emptyTRAVONCEOfGBuilder): Builder[G, TRAVONCE[G]] Or Every[ERR])) { (accumulator: Builder[G, TRAVONCE[G]] Or Every[ERR],  nextElem: G) =>
            (accumulator, fn(nextElem)) match {
              case (Good(bldr), Good(ele)) => Good(bldr += ele)
              case (Good(bldr), Bad(err)) => Bad(err)
              case (Bad(errA), Bad(errB)) => Bad(errA ++ errB)
              case (Bad(errA), Good(ele)) => Bad(errA)
            }
          }
        tempOr map (_.result)
      }
    }

  implicit def convertEveryToValidatable[G](oneToMany: Every[G]): Validatable[G, Every] = 
    new Validatable[G, Every] {
      def validatedBy[ERR, EVERY[e] <: Every[e]](fn: G => G Or EVERY[ERR]): Every[G] Or Every[ERR] = {
        val vec = oneToMany.toVector
        val z: Every[G] Or Every[ERR] =
          fn(vec.head) match {
            case Good(u) => Good(One(u))
            case Bad(err) => Bad(err)
          }
        vec.tail.foldLeft(z) { (accumulator: Every[G] Or Every[ERR],  nextElem: G) =>
          (accumulator, fn(nextElem)) match {
            case (Good(everyG), Good(u)) => Good(everyG :+ u)
            case (Good(_), Bad(err)) => Bad(err)
            case (Bad(errA), Bad(errB)) => Bad(errA ++ errB)
            case (Bad(errA), Good(_)) => Bad(errA)
          }
        }
      }
    }

  implicit def convertOptionToValidatable[G](option: Option[G]): Validatable[G, Option] = 
    new Validatable[G, Option] {
      def validatedBy[ERR, EVERY[e] <: Every[e]](fn: G => G Or EVERY[ERR]): Option[G] Or Every[ERR] = {
        option.map(fn) match {
          case Some(Good(g)) => Good(Some(g))
          case Some(Bad(err)) => Bad(err)
          case None => Good(None)
        }
      }
    }
}

