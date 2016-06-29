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
package org.scalatest

import org.scalactic.Prettifier

private[scalatest] sealed trait MessageBuilder extends Serializable {
  def build: String
}

private[scalatest] object MessageBuilder {

  class MessageBuilder0(fun: => String) extends MessageBuilder {
    def build: String = fun
  }

  def of(fun: => String): MessageBuilder = new MessageBuilder0(fun)

  case class MessageBuilder1(prettifier: Prettifier, fun: (Prettifier) => String) extends MessageBuilder {
    def build: String = fun(prettifier)
  }

  def of(prettifier: Prettifier, fun: (Prettifier) => String): MessageBuilder = MessageBuilder1(prettifier, fun)

  case class MessageBuilder2[T1](prettifier: Prettifier, p1: T1, fun: (Prettifier, T1) => String) extends MessageBuilder {
    def build: String = fun(prettifier, p1)
  }

  def of[T1](prettifier: Prettifier, p1: T1, fun: (Prettifier, T1) => String): MessageBuilder = MessageBuilder2(prettifier, p1, fun)

  case class MessageBuilder3[T1, T2](prettifier: Prettifier, p1: T1, p2: T2, fun: (Prettifier, T1, T2) => String) extends MessageBuilder {
    def build: String = fun(prettifier, p1, p2)
  }

  def of[T1, T2](prettifier: Prettifier, p1: T1, p2: T2, fun: (Prettifier, T1, T2) => String): MessageBuilder = MessageBuilder3(prettifier, p1, p2, fun)

  case class MessageBuilder4[T1, T2, T3](prettifier: Prettifier, p1: T1, p2: T2, p3: T3, fun: (Prettifier, T1, T2, T3) => String) extends MessageBuilder {
    def build: String = fun(prettifier, p1, p2, p3)
  }

  def of[T1, T2, T3](prettifier: Prettifier, p1: T1, p2: T2, p3: T3, fun: (Prettifier, T1, T2, T3) => String): MessageBuilder = MessageBuilder4(prettifier, p1, p2, p3, fun)

  case class MessageBuilder5[T1, T2, T3, T4](prettifier: Prettifier, p1: T1, p2: T2, p3: T3, p4: T4, fun: (Prettifier, T1, T2, T3, T4) => String) extends MessageBuilder {
    def build: String = fun(prettifier, p1, p2, p3, p4)
  }

  def of[T1, T2, T3, T4](prettifier: Prettifier, p1: T1, p2: T2, p3: T3, p4: T4, fun: (Prettifier, T1, T2, T3, T4) => String): MessageBuilder = MessageBuilder5(prettifier, p1, p2, p3, p4, fun)

}