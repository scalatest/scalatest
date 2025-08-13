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

import org.scalactic.{MacroOwnerRepair, Resources}

import scala.reflect.macros.whitebox.Context

/**
  * Helper class for Position macro. (Will be removed from the public API in future ScalaTest release, if possible.)
  */
object TypeInfoMacro {

  /**
    * Helper method for TypeInfo macro.
    */
  def genTypeInfo[T: context.WeakTypeTag](context: Context) = {
    import context.universe._

    val TypeApply(_, List(typeTree)) = context.macroApplication

    val expandedCode =
    context.Expr(
      Apply(
        TypeApply(
          Select(
            Select(
              Select(
                Select(
                  Select(
                    Ident(TermName("_root_")),
                    TermName("org")
                  ),
                  TermName("scalactic")
                ),
                TermName("source")
              ),
              TermName("TypeInfo")
            ),
            TermName("apply")
          ),
          List(typeTree.duplicate)
        ),
        List(
          Literal(Constant(typeTree.toString()))
        )
      )
    )

    val ownerRepair = new MacroOwnerRepair[context.type](context)
    ownerRepair.repairOwners(expandedCode)
  }

}