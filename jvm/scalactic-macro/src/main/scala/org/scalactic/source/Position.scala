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
package org.scalactic.source

/**
 * A source file position consisting of a simple source file name, the
 * full path to the file, and a line number.
 *
 * <p>
 * Class <code>Position</code> is used by ScalaTest assertions, matchers,
 * and testing styles to provide locations for failures in test results.
 * </p>
 *
 * <p>
 * The <code>filePathname</code> should always end with the <code>fileName</code>, which
 * is the simple file name. For example, if the <code>filePathname</code> were <code>"/Users/this/is/a/class/Act.scala"</code>,
 * the <code>fileName</code> would be <code>"Act.scala"</code>.
 * </p>
 * 
 * <p>
 * <em>Note: Class <code>Position</code> is in part inspired by the <a href="https://github.com/lihaoyi/sourcecode" target="_blank"><code>sourcecode</code></a> library designed by Li Haoyi.</a></em>
 * </p>
 *
 * @param fileName the simple name of a source file
 * @param filePathname the fully qualified pathname of the source file
 * @param lineNumber a line number inside the source file with the given filePathname and fileName
 */
case class Position(fileName: String, filePathname: String, lineNumber: Int)

/**
 * Companion object for <code>Position</code> that defines an implicit
 * method that uses a macro to grab the enclosing position.
 */
object Position {

  import scala.language.experimental.macros

  /**
   * Implicit method, implemented with a macro, that returns the enclosing
   * source position where it is invoked.
   *
   * @return the enclosing source position
   */
  implicit def here: Position = macro PositionMacro.genPosition
}

