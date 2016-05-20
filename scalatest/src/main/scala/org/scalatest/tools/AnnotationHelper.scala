/*
 * Copyright 2001-2016 Artima, Inc.
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
package org.scalatest.tools

import java.lang.annotation.Annotation

import org.scalatest.WrapWith
import org.scalatest.Resources
import org.scalatest.exceptions.StackDepthExceptionHelper.getStackDepthFun
import org.scalatest.exceptions.NotAllowedException

import scala.collection.mutable

/**
 * A Helper Object to find an Annotation on a given class and all of its superclasses, interfaces and traits.
 *
 * @author Dennis Rieks
 */
object AnnotationHelper {
  /**
   * Builds a new Seq from the given Seq without any duplicate elements.
   *
   * In oposite to scala.collection.SeqLike.distinct
   * this function uses eq and not ==
   */
  private[scalatest] def distinctEq[A <: AnyRef](seq: Seq[A]): Seq[A] = {
    val seen = mutable.MutableList[A]()
    def addToSeen(a: A): A = {
      seen += a
      a
    }
    for {
      a <- seq
      if !seen.exists(_ eq a)
    } yield addToSeen(a)
  }

  /**
   * Finds all Annotations of the given type for the given class.
   *
   * @param anno The type of the Annotation to search for.
   *
   * @param clazz the class to search.
   *
   * @return a Seq of all found Annotations.
   */
  def findAll[A <: Annotation](anno: Class[A], clazz: Class[_]): Seq[A] = {
    if (Option(clazz).isEmpty) {
      Seq()
    } else {
      distinctEq(Option(clazz.getAnnotation(anno)).toSeq ++
        findAll(anno, clazz.getSuperclass) ++
        clazz.getInterfaces.flatMap(findAll(anno, _)))
    }
  }

  /**
   * Find zero or one instance of the given Annotation on the class.
   *
   * Returns <code>None<code> when the class is not annotated, or <code>Some(A)<code> when there was only
   * one instance of the annotation found on the class.
   *
   * @throws NotAllowedException when there was more then one annotation found one the class.
   *
   * @param anno The type of the Annotation to return.
   *
   * @param clazz the class to search.
   *
   * @return the found Annotation.
   */
  def find[A <: Annotation](anno: Class[A], clazz: Class[_]): Option[A] = {
    val ret = findAll(anno, clazz)
    if (ret.length > 1) {
      throw new NotAllowedException(Resources.moreThenOneAnnotationFound(anno.getName, clazz.getName), getStackDepthFun("AnnotationHelper", "find"))
    }
    ret.headOption
  }

  /**
   * Returns the instance of the given Annotation found on the class.
   *
   * @throws NotAllowedException when there was not exactly one instance of the annotation found one the class.
   *
   * @param anno The type of the Annotation to return.
   *
   * @param clazz the class to search.
   *
   * @return The instance of the Annotation of the given Type
   */
  def get[A <: Annotation](anno: Class[A], clazz: Class[_]): A = {
    val ret = findAll(anno, clazz)
    if (ret.length != 1) {
      throw new NotAllowedException(Resources.notExactlyOneAnnotationFound(anno.getName, clazz.getName), getStackDepthFun("AnnotationHelper", "get"))
    }
    ret.head
  }

  /**
   * Returns the WrapWith Annotation of the given class.
   *
   * @throws NotAllowedException when there was more then one annotation found one the class.
   *
   * @param anno The type of the Annotation to return.
   * @param clazz the class to search.
   *
   * @return the found Annotation.
   */
  def findWrapWith(clazz: Class[_]): Option[WrapWith] = {
    find(classOf[WrapWith], clazz)
  }
}
