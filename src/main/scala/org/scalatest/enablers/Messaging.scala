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
 * Supertrait for <code>Messaging</code> typeclasses.
 *
 * @author Bill Venners
 */
trait Messaging[T] {

  /**
   * Returns the message of the passed object.
   *
   * @param the object whose message to return
   * @return the message of the passed object
   */
  def messageOf(o: T): String
}

object Messaging {

  implicit def messagingNatureOfThrowable[EX <: Throwable]: Messaging[EX] = 
    new Messaging[EX] {
      def messageOf(exception: EX): String = exception.getMessage
    }
}


