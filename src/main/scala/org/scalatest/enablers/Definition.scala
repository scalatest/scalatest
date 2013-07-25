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

/**
 * Supertrait for typeclasses that enable the <code>be</code> <code>defined</code> matcher syntax.
 *
 * <p>
 * A <code>Definition[S]</code> provides access to the "definition nature" of type <code>S</code> in such
 * a way that <code>be</code> <code>defined</code> matcher syntax can be used with type <code>S</code>. An <code>S</code>
 * can be any type for which the concept of being sorted makes sense, such as <code>scala.Option</code>. ScalaTest provides
 * implicit implementation for Option. You can enable the <code>be</code> <code>defined</code> matcher syntax on your own
 * type <code>U</code> by defining a <code>Definition[U]</code> for the type and making it available implicitly.
 * 
 * <p>
 * ScalaTest provides an implicit <code>Definition</code> instance for <code>scala.Option</code>
 * in the <code>Definition</code> companion object.
 * </p>
 */
trait Definition[-T] {

  /**
   * Determines whether the passed in thing is defined, <em>i.e.</em>, the passed in <code>scala.Option</code> is defined.
   */
  def isDefined(thing: T): Boolean
}

object Definition {

  /**
   * Provides Definition implementation for <code>scala.Option</code>
   */
  implicit def definitionOfOption[E, OPT[e] <: scala.Option[e]]: Definition[OPT[E]] =
    new Definition[OPT[E]] {
      def isDefined(option: OPT[E]): Boolean = option.isDefined
    }

}

