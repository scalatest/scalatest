/*
 * Copyright 2001-2025 Artima, Inc.
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

private[scalatest] sealed trait AnsiColor extends Product with Serializable {
  val code: String
}
private[scalatest] case object AnsiGreen extends AnsiColor {
  val code: String =  "\u001b[32m"
}
private[scalatest] case object AnsiCyan extends AnsiColor {
  val code: String =  "\u001b[36m"
}
private[scalatest] case object AnsiYellow extends AnsiColor {
  val code: String =  "\u001b[33m"
}
private[scalatest] case object AnsiRed extends AnsiColor {
  val code: String =  "\u001b[31m"
}

