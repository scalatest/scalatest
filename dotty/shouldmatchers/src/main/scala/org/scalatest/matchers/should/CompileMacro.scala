/*
 * Copyright 2001-2012 Artima, Inc.
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
package org.scalatest.matchers.should

import org.scalactic._
import org.scalatest.Assertion
import org.scalatest.verbs.{CompileWord, TypeCheckWord}

import scala.quoted._

object CompileMacro {

  // used by should compile syntax, delegate to assertCompileImpl to generate code
  def shouldCompileImpl(self: Expr[Matchers#AnyShouldWrapper[_]], compileWord: Expr[CompileWord])(pos: Expr[source.Position])(implicit qctx: QuoteContext): Expr[Assertion] =
    org.scalatest.matchers.CompileMacro.assertCompileImpl(self, compileWord, pos)("should")

  // used by shouldNot compile syntax, delegate to assertNotCompileImpl to generate code
  def shouldNotCompileImpl(self: Expr[Matchers#AnyShouldWrapper[_]], compileWord: Expr[CompileWord])(pos: Expr[source.Position])(implicit qctx: QuoteContext): Expr[Assertion] =
    org.scalatest.matchers.CompileMacro.assertNotCompileImpl(self, compileWord, pos)("should")

  // used by shouldNot typeCheck syntax, delegate to assertNotTypeCheckImpl to generate code
  def shouldNotTypeCheckImpl(self: Expr[Matchers#AnyShouldWrapper[_]], typeCheckWord: Expr[TypeCheckWord])(pos: Expr[source.Position])(implicit qctx: QuoteContext): Expr[Assertion] =
    org.scalatest.matchers.CompileMacro.assertNotTypeCheckImpl(self, typeCheckWord, pos)("should")  
  

}