/*
 * Copyright 2001-2019 Artima, Inc.
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

import org.scalactic.MacroContext.Context
import org.scalactic.Resources

/**
 * Helper class for Position macro. (Will be removed from the public API if possible in a subsequent 3.0.0-RCx release.)
 */
object PositionMacro {   
  private object sourceCompatHack {
    object internal { object decorators {} }
  }
  import sourceCompatHack._

  private[scalactic] lazy val showScalacticFillFilePathnames: Boolean = {
    val value = System.getenv("SCALACTIC_FILL_FILE_PATHNAMES")
    value != null && value == "yes"
  }

  private class PositionMacroImpl(val universe: scala.reflect.api.Universe) {

    private val PositionModule = universe.rootMirror.staticModule("org.scalactic.source.Position")
    private val PositionClass = universe.rootMirror.staticClass("org.scalactic.source.Position")
    private val Position_apply = PositionModule.typeSignature.declaration(universe.TermName("apply"))

    def apply(context: Context): context.Tree = {
      import context.universe._

      // On 2.10, this imports the empty object defined above
      // On 2.11+, this imports `Tree.setType` extension method
      import internal.decorators._ 

      // Our types don't capture the fact that context.universe eq this.universe, so we have to cast.
      implicit def castSymbol(s: universe.Symbol): context.universe.Symbol = s.asInstanceOf[context.universe.Symbol]
      implicit def castType(t: universe.Type): context.universe.Type = t.asInstanceOf[context.universe.Type]

      import treeBuild.{mkAttributedIdent, mkAttributedSelect}

      // Because this macro is called so frequently, we assign types as we build them, rather
      // than returning an untyped tree and letting the typechecker fill in the types.
      // (`setType` and `mkAttributedXxx` assign Tree.tpe.)
      def strLit(s: String) = {
        Literal(Constant(s)).setType(definitions.StringClass.toTypeConstructor)
      }
      def intLit(i: Int) = {
        Literal(Constant(i)).setType(definitions.IntTpe)
      }
      val args = List(
        strLit(context.enclosingPosition.source.file.name),
        if (showScalacticFillFilePathnames) strLit(context.enclosingPosition.source.path) else strLit(Resources.pleaseDefineScalacticFillFilePathnameEnvVar), 
        intLit(context.enclosingPosition.line)
      )

      Apply(mkAttributedSelect(mkAttributedIdent(PositionModule), Position_apply), args).setType(PositionClass.toTypeConstructor)
    }
  }

  // Cache to avoid needing to lookup of PositionModule/PositionClass/Position_apply each macro expansion.
  private var cache = new java.lang.ref.WeakReference[PositionMacroImpl](null)

  /**
   * Helper method for Position macro.
   */
  def genPosition(context: Context): context.Expr[Position] = {
    var impl = cache.get()
    if (impl == null || impl.universe != context.universe) {
      impl = new PositionMacroImpl(context.universe)
      cache = new java.lang.ref.WeakReference[PositionMacroImpl](impl)
    }
    context.Expr[Position](impl.apply(context))
  }
}