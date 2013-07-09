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
 * Supertrait for typeclasses that enable <code>definedAt</code> matcher syntax.
 * 
 * <p>
 * ScalaTest provides implicit <code>Definition</code> instances for <code>scala.PartialFunction</code> in the
 * <code>Definition</code> companion object.
 * </p>
 */
trait Definition[-T] {
  
  def isDefinedAt(target: T, at: Any): Boolean
  
}

object Definition {
  
  implicit def definitionNatureOfPartialFunction[A, B, PFUN[a, b] <: PartialFunction[a, b]]: Definition[PFUN[A, B]] = 
    new Definition[PFUN[A, B]] {
      def isDefinedAt(partialFun: PFUN[A, B], at: Any): Boolean = {
        at match {
          case a: A => partialFun.isDefinedAt(a)
          case _ => false
        }
        
      }
    }
}