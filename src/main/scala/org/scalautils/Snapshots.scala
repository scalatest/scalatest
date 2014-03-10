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
final case class Snapshot(name: String, value: Any) {

  /**
   * Overriden <code>toString</code> to print in {name} = {value} format.
   *
   * @return string in {name} = {value} format
   */
  override def toString: String = Resources("variableWasValue", name, Prettifier.default(value))
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
  def snap(expressions: Any*): SnapshotSeq = macro SnapshotsMacro.snap
}

final class SnapshotSeq(underlying: collection.immutable.IndexedSeq[Snapshot]) extends collection.immutable.IndexedSeq[Snapshot] {

  /**
   * Selects an element by its index in the sequence.
   *
   * <p>
   * This method invokes <code>apply</code> on the underlying immutable <code>IndexedSeq[String]</code>, passing in <code>idx</code>, and returns the result.
   * </p>
   *
   * @param idx the index to select
   * @return the element of this sequence at index <code>idx</code>, where 0 indicates the first element
   */
  def apply(idx: Int): Snapshot = underlying.apply(idx)

  /**
   * The length of this sequence.
   *
   * <p>
   * This method invokes <code>length</code> on the underlying immutable <code>IndexedSeq[String]</code> and returns the result.
   * </p>
   *
   * @return the number of elements in this sequence
   */
  def length: Int = underlying.length

  /**
   * Appends a string element to this sequence, if it doesn't already exist in the sequence.
   *
   * <p>
   * If the string element already exists in this sequence, this method returns itself. If not,
   * this method returns a new <code>MultiSelOptionSeq</code> with the passed value appended to the
   * end of the original <code>MultiSelOptionSeq</code>.
   * </p>
   *
   * @param the string element to append to this sequence
   * @return a <code>MultiSelOptionSeq</code> that contains the passed string value
   */
  def +(value: Snapshot): SnapshotSeq = {
    if (!underlying.contains(value))
      new SnapshotSeq(underlying :+ value)
    else
      this
  }

  /**
   * Removes a string element to this sequence, if it already exists in the sequence.
   *
   * <p>
   * If the string element does not already exist in this sequence, this method returns itself. If the element
   * is contained in this sequence, this method returns a new <code>MultiSelOptionSeq</code> with the passed value
   * removed from the the original <code>MultiSelOptionSeq</code>, leaving any other elements in the same order.
   * </p>
   *
   * @param the string element to append to this sequence
   * @return a <code>MultiSelOptionSeq</code> that contains the passed string value
   */
  def -(value: Snapshot): SnapshotSeq = {
    if (underlying.contains(value))
      new SnapshotSeq(underlying.filter(_ != value))
    else
      this
  }

  override def toString: String = mkString(", ")

  def lines: String = mkString("\n")
}

object SnapshotSeq {
  def apply(snapshots: Snapshot*): SnapshotSeq = new SnapshotSeq(Vector(snapshots: _*))
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

  def snap(context: Context)(expressions: context.Expr[Any]*): context.Expr[SnapshotSeq] = {
    import context.universe._

    def getPosition(expr: Tree): Int = {
      expr match {
        case apply: Apply => getPosition(apply.fun)
        case typeApply: TypeApply => getPosition(typeApply.fun)
        case select: Select => getPosition(select.qualifier)
        case other => other.pos.asInstanceOf[scala.reflect.internal.util.Position].point
      }
    }

    def getEndOffset(tree: Tree): Int = {
      tree match {
        case apply: Apply =>
          if (apply.args.length > 0)
            getEndOffset(apply.args.last)
          else
            getEndOffset(apply.fun)
        case typeApply: TypeApply =>
          if (typeApply.args.length > 0)
            getEndOffset(typeApply.args.last)
          else
            getEndOffset(typeApply.fun)
        case other =>
          val otherPos = other.pos
          val otherOffset = otherPos.point
          val otherColumn = otherPos.column
          val lineContent = otherPos.lineContent
          val lineLength = lineContent.length
          otherOffset + lineLength - otherColumn
      }
    }

    val content = context.macroApplication.pos.source.content.mkString

    val offsetList =
      (expressions.map { expr =>
        getPosition(expr.tree)
      }).toList

    val rangeList = offsetList.sliding(2)
    val initSourceList =
      (rangeList.map { case List(start, end) =>
        val raw = content.substring(start, end).trim
        if (raw.endsWith(","))
          raw.substring(0, raw.length - 1)
        else
          raw
      }).toList

    // Let's get the last one, the last one is the most tricky one,
    // we'll double check the source we get by parse and type check it, and
    // compare it to the original tree's text (by calling show()).  If they are
    // different, we'll just fallback to use the show(original) text.
    val lastSource = content.substring(offsetList.last, getEndOffset(expressions.last.tree))
    val lastShowText = show(expressions.last.tree)
    val sourceList =
      try {
        if (show(context.typeCheck(context.parse(lastSource))) == lastShowText)
          initSourceList ++ List(lastSource)
        else
          initSourceList ++ List(show(lastShowText))
      }
      catch {
        case e: Throwable => initSourceList ++ List(lastShowText)
      }

    val snapshots =
      expressions.zipWithIndex.map { case (expr, idx) =>
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
          List(context.literal(sourceList(idx)).tree, expr.tree.duplicate)
        )
      }

    context.Expr(
      Apply(
        Select(
          Select(
            Select(
              Ident(newTermName("org")),
              newTermName("scalautils")
            ),
            newTermName("SnapshotSeq")
          ),
          newTermName("apply")
        ),
        List(snapshots: _*)
      )
    )
  }
}