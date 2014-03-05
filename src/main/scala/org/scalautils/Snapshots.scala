/*
 * Copyright 2001-2014 Artima, Inc.
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
package org.scalautils

import reflect.macros.Context

/**
 * Case class to store the name and value of an expression.
 *
 * @param name the name (as in source) of the expression
 * @param value the value of the expression
 */
case class Snapshot(name: String, value: Any) {

  /**
   * Overriden <code>toString</code> to print in {name} = {value} format.
   *
   * @return string in {name} = {value} format
   */
  override def toString: String = Resources("aEqualB", name, Prettifier.default(value))

}

/**
 * Trait that contains a <code>snap</code> method that can be used to capture snapshot of expression.
 * A <code>Snapshot</code> contains the name (as in source) and the value of the expression.
 */
trait Snapshots {

  import language.experimental.macros

  /**
   * Snap the given expressions.
   *
   * @param expressions expressions to be snapped
   * @return an <code>IndexedSeq</code> of <code>Snapshot</code> for the given expressions.
   */
  def snap(expressions: Any*): IndexedSeq[Snapshot] = macro SnapshotsMacro.snap
}

/**
 * Companion object that facilitates the importing of <code>Snapshots</code> members as
 * an alternative to mixing it in. One use case is to import <code>Snapshots</code> members so you can use
 * them in the Scala interpreter:
 *
 * <pre class="stREPL">
 * $scala -classpath scalatest.jar
 * Welcome to Scala version 2.10.3.final (Java HotSpot(TM) Client VM, Java xxxxxx).
 * Type in expressions to have them evaluated.
 * Type :help for more information.
 * &nbsp;
 * scala&gt; import org.scalautils.Snapshots._
 * import org.scalatest.Snapshots._
 * &nbsp;
 * scala&gt; val a = 8
 * a: Int = 8
 *&nbsp;
 * scala&gt; snap(a)
 * res0: scala.collection.immutable.Vector[org.scalautils.Snapshot] = Vector(a = 8)
 * </pre>
 */
object Snapshots extends Snapshots

private[scalautils] object SnapshotsMacro {

  def snap(context: Context)(expressions: context.Expr[Any]*): context.Expr[IndexedSeq[Snapshot]] = {
    import context.universe._

    val snapshots =
      expressions.map { expr =>
        Apply(
          Select(
            Select(
              Select(
                Ident(newTermName("org")),
                newTermName("scalautils")
              ),
              newTermName("Snapshot")
            ),
            newTermName("apply")
          ),
          List(context.literal(show(expr.tree)).tree, expr.tree.duplicate)
        )
      }

    context.Expr(
      Apply(
        Select(
          Ident("Vector"),
          newTermName("apply")
        ),
        List(snapshots: _*)
      )
    )
  }
}