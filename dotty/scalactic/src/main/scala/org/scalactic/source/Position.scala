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

import scala.quoted.*

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
 * The <code>pathFilename<code> should always end with the <code>fileName</code>, which
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
 * @param lineNumber a line number inside the source file with the given filePathname and fileNamae
 */
case class Position(fileName: String, filePathname: String, lineNumber: Int)

/**
 * Companion object for <code>Position</code> that defines an implicit
 * method that uses a macro to grab the enclosing position.
 */
object Position {

  import org.scalactic.Resources

  /**
   * Inline given method, implemented with a macro, that returns the enclosing
   * source position where it is invoked.
   *
   * @return the enclosing source position
   */
  inline given here: Position = ${ genPosition }

  private[scalactic] lazy val showScalacticFillFilePathnames: Boolean = 
    Option(System.getenv("SCALACTIC_FILL_FILE_PATHNAMES")) == Some("yes")

  private[org] def filePathnames(path: String): String = 
    if (showScalacticFillFilePathnames) path else Resources.pleaseDefineScalacticFillFilePathnameEnvVar

  /**
   * Helper method for Position macro.
   */
  private def genPosition(using Quotes): Expr[Position] = {
    val pos = quotes.reflect.Position.ofMacroExpansion
    val file = pos.sourceFile
    val fileName: String = Option(file.jpath).map(_.getFileName.toString).getOrElse("<unknown>")
    val filePath: String = filePathnames(file.toString)
    val lineNo: Int = pos.startLine + 1
    '{ Position(${Expr(fileName)}, ${Expr(filePath)}, ${Expr(lineNo)}) }
  }

  def withPosition[T](fun: Expr[Position => T])(using quotes: Quotes, typeOfT: Type[T]): Expr[T] = {
    val pos = quotes.reflect.Position.ofMacroExpansion
    val file = pos.sourceFile
    val fileName: String = Option(file.jpath).map(_.getFileName.toString).getOrElse("<unknown>")
    val filePath: String = org.scalactic.source.Position.filePathnames(file.toString)
    val lineNo: Int = pos.startLine + 1
   '{${fun}.apply(org.scalactic.source.Position(${Expr(fileName)}, ${Expr(filePath)}, ${Expr(lineNo)}))}
  }

  /**
   * Helper method for macros that need to respect a caller-provided <code>Position</code>, falling back to the
   * position of the macro expansion itself when no caller-supplied <code>Position</code> exists.
   *
   * <p>
   * A <code>Position</code> resolved from the inline given <code>Position.here</code> shows up inside an inlined
   * method body as a reference to a synthetic "$proxy" val created while inlining the given. In that case no
   * caller-supplied <code>Position</code> exists, so the position of the invocation itself is produced via
   * <code>withPosition</code>. Otherwise, the <code>Position</code> provided by the caller is respected and
   * passed to <code>call</code>.
   * </p>
   *
   * <p>
   * Note: this method is intended for use by the ScalaTest/Scalactic frameworks' own macros and is not part of
   * the public API surface meant for user code.
   * </p>
   *
   * @param pos the expression representing the caller-provided position
   * @param call function expression invoked with either the caller-provided position or the expansion-site position
   * @return the resulting expression
   */
  def withCallerPosition[T](pos: Expr[Position], call: Expr[Position => T])(using quotes: Quotes, typeOfT: Type[T]): Expr[T] = {
    import quotes.reflect.*
    def unwrap(term: Term): Term =
      term match {
        case Inlined(_, _, inner) => unwrap(inner)
        case Block(Nil, inner) => unwrap(inner)
        case Typed(inner, _) => unwrap(inner)
        case _ => term
      }
    val isCallerProvidedPosition =
      unwrap(pos.asTerm) match {
        case Ident(name) => !name.contains("$proxy")
        case _ => true
      }
    if (isCallerProvidedPosition)
      '{ $call($pos) }
    else
      withPosition[T](call)
  }

}

